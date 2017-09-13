//! A multipart format, in which each part can be compressed independently.

use ast::grammar::{ Field, FieldName, NodeName, Syntax };
use bytes;
use bytes::varnum::*;
use token::io::*;

use std;
use std::collections::HashMap;
use std::cell::RefCell;
use std::hash::Hash;
use std::io::{ Write };
use std::rc::Rc;

/// A value that may be serialized to bytes, optionally compressed.
trait Serializable {
    /// Write the data, without compression.
    fn write<W: Write>(&self, &mut W) -> Result<(), std::io::Error>;

    /// Write the data, with compression.
    fn write_with_compression<W: Write>(&self, out: &mut W, compression: &Compression) -> Result<(), std::io::Error> {
        let mut uncompressed = Vec::with_capacity(2048);
        self.write(&mut uncompressed)?;
        compression.apply(&uncompressed, out)?;
        Ok(())
    }
}

impl Serializable for String {
    fn write<W: Write>(&self, out: &mut W) -> Result<(), std::io::Error> {
        out.write_varnum(self.len() as u32)?;
        out.write(self.as_bytes())?;
        Ok(())
    }
}
impl Serializable for Option<String> {
    fn write<W: Write>(&self, out: &mut W) -> Result<(), std::io::Error> {
        const EMPTY_STRING: [u8; 2] = [255, 0];
        match *self {
            None => {
                out.write_varnum(EMPTY_STRING.len() as u32)?;
                out.write(&EMPTY_STRING)?;
            },
            Some(ref data) => {
                data.write(out)?;
            }
        }
        Ok(())
    }
}

struct TableEntry<T> where T: Clone { // We shouldn't need the `Clone`, sigh.
    /// Number of instances of this entry around.
    instances: RefCell<u32>,
    data: T,
    index: TableIndex<T>
}
impl<T> TableEntry<T> where T: Clone {
    fn new(data: T) -> Self {
        TableEntry {
            instances: RefCell::new(0),
            data,
            index: TableIndex::new()
        }
    }
    fn acquire(&self) { // FIXME: Not a terribly good API.
        let mut borrow = self.instances.borrow_mut();
        *borrow += 1;
    }
}

struct GenericTable<Key, Value> where Key: Eq + Hash + Clone, Value: Clone + Serializable {
    map: HashMap<Key, TableEntry<Value>>
}
impl<Key, Value> GenericTable<Key, Value> where Key: Eq + Hash + Clone, Value: Clone + Serializable {
    fn get(&self, kind: &Key) -> Option<&TableEntry<Value>> {
        self.map.get(kind)
    }
    fn insert(&mut self, key: Key, value: Value) -> TableIndex<Value> {
        let entry = TableEntry::new(value);
        entry.acquire();
        let index = entry.index.clone();
        if let Some(_) = self.map.insert(key, entry) {
            panic!("The table already contains an entry for this key");
        }
        index
    }
}
impl<Key, Value> Serializable for GenericTable<Key, Value> where Key: Eq + Hash + Clone, Value: Clone + Serializable {
    fn write<W: Write>(&self, out: &mut W) -> Result<(), std::io::Error> {
        // Sort entries by number of uses.
        let mut contents : Vec<_> = self.map.values().collect();
        contents.sort_unstable_by(|a, b| u32::cmp(&*a.instances.borrow(), &*b.instances.borrow()));

        // Assign TableIndex
        for i in 0..contents.len() {
            let mut borrow = contents[i].index.index.borrow_mut();
            *borrow = Some(i as u32);
        }

        // Serialize each entry
        let mut serialized = Vec::with_capacity(contents.len());
        for entry in contents.drain(..) {
            let mut bytes = Vec::with_capacity(256);
            entry.data.write(&mut bytes)?;
            serialized.push(bytes);
        }

        // Write number of entries
        out.write_varnum(serialized.len() as u32)?;

        // Write length of each entry
        for entry in &serialized {
            out.write_varnum(entry.len() as u32)?;
        }

        // Write actual content of each entry
        for entry in &serialized {
            out.write(&entry)?;
        }

        Ok(())
    }
}


#[derive(PartialEq, Eq, Clone)] // FIXME: Clone shouldn't be necessary. Sigh.
pub struct NodeDescription {
    pub kind: NodeName,
    pub fields: Vec<FieldName>,
}

impl Serializable for NodeDescription {
    fn write<W: Write>(&self, out: &mut W) -> Result<(), std::io::Error> {
        self.kind.to_string().write(out)?;
        out.write_varnum(self.fields.len() as u32)?;
        for field in &self.fields {
            field.to_string().write(out)?;
        }
        Ok(())
    }
}

enum Item {
    String(TableIndex<Option<String>>),
    NodeDescription(TableIndex<NodeDescription>),
    Encoded(Vec<u8>),
    List {
        items: Vec<Tree>,
        byte_len: bool
    }
}

#[derive(Clone)]
pub struct Tree(Rc<Item>);

impl Serializable for Tree {
    fn write<W: Write>(&self, out: &mut W) -> Result<(), std::io::Error> {
        match *self.0 {
            Item::String(ref index) => {
                index.write(out)?;
            },
            Item::NodeDescription(ref index) => {
                index.write(out)?;
            },
            Item::Encoded(ref vec) => {
                out.write(&vec)?;
            },
            Item::List { ref items, ref byte_len } => {
                if *byte_len {
                    // Compute byte length
                    let mut buf = Vec::with_capacity(1024);
                    for item in items {
                        item.write(&mut buf)?;
                    }
                    // Write byte length
                    out.write_varnum(buf.len() as u32)?;
                    // Write data
                    out.write(&buf)?;
                } else {
                    for item in items {
                        item.write(out)?;
                    }
                }
            }
        }
        Ok(())
    }
}

#[derive(Clone)]
struct TableIndex<T> where T: Clone {
    phantom: std::marker::PhantomData<T>,
    index: RefCell<Option<u32>>,
}
impl<T> TableIndex<T> where T: Clone {
    fn new() -> Self {
        TableIndex {
            phantom: std::marker::PhantomData,
            index: RefCell::new(None)
        }
    }
}
impl<T> Serializable for TableIndex<T> where T: Clone {
    fn write<W: Write>(&self, out: &mut W) -> Result<(), std::io::Error> {
        if let Some(ref i) = *self.index.borrow() {
            out.write_varnum(*i)?;
        } else {
            panic!("Attempting to serialize a TableIndex whose index is None");
        }
        Ok(())
    }
}

pub struct TreeTokenWriter<W: Write> {
    /// The table defining the accepted TaggedTuple
    /// and how they are laid out in the binary.
    grammar_table: GenericTable<NodeName, NodeDescription>,

    /// The strings used in the binary.
    strings_table: GenericTable<Option<String>, Option<String>>,

    root: Tree,

    syntax: Syntax,

    out: W,

    options: WriteOptions,
}

/// The compression mechanisms supported by this encoder.
/// They are designed to match HTTP's Accept-Encoding:
/// https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept-Encoding
pub enum Compression {
    /// no compression (`identity;`)
    Identity,
    /// gzip compression (`gzip;`)
    Gzip,
    /// zlib compression (`deflate;`)
    Deflate,
    /// brotly compression (`br;`)
    Brotli,
    /// Lwz compression (`compress;`)
    Lzw,
} 

impl Compression {
    // Format:
    // - compression type (string);
    // - compressed byte length (varnum);
    // - data.
    fn apply<W: Write>(&self, data: &[u8], out: &mut W) -> Result<(), std::io::Error> {
        match *self {
            Compression::Identity => {
                out.write(b"identity;")?;
                out.write_varnum(data.len() as u32)?;
                out.write(data)?;
            }
            Compression::Gzip => {
                use flate2;
                out.write(b"gzip;")?;
                // Compress
                let buffer = Vec::with_capacity(data.len());
                let mut encoder = flate2::write::GzEncoder::new(buffer, flate2::Compression::Best);
                encoder.write(data)?;
                let buffer = encoder.finish()?;
                // Write
                out.write_varnum(buffer.len() as u32)?;
                out.write(&buffer)?;
            }
            Compression::Deflate => {
                use flate2;
                out.write(b"deflate;")?;
                // Compress
                let buffer = Vec::with_capacity(data.len());
                let mut encoder = flate2::write::ZlibEncoder::new(buffer, flate2::Compression::Best);
                encoder.write(data)?;
                let buffer = encoder.finish()?;
                // Write
                out.write_varnum(buffer.len() as u32)?;
                out.write(&buffer)?;
            }
            Compression::Brotli => {
                use brotli;
                out.write(b"br;")?;
                // Compress
                let mut buffer = Vec::with_capacity(data.len());
                {
                    let len = buffer.len();
                    let mut encoder = brotli::CompressorWriter::new(&mut buffer, len, /* quality ? */ 11, /*window_size ?*/ 22);
                    encoder.write(data)?;
                }
                // Write
                out.write_varnum(buffer.len() as u32)?;
                out.write(&buffer)?;
            }
            Compression::Lzw => {
                use lzw;
                out.write(b"compress;")?;
                // Compress
                let mut buffer = Vec::with_capacity(data.len());
                {
                    let writer = lzw::LsbWriter::new(&mut buffer);
                    let mut encoder = lzw::Encoder::new(writer, /*min_code_size ?*/8)?;
                    encoder.encode_bytes(data)?;
                }
                // Write
                out.write_varnum(buffer.len() as u32)?;
                out.write(&buffer)?;                
            }
        }
        Ok(())
    }
}

pub struct WriteOptions {
    grammar_table: Compression,
    strings_table: Compression,
    tree: Compression,
}
#[derive(Debug)]
pub enum TokenWriterError {
    MissingKind(String),
    WriteError(std::io::Error),
}
impl<W: Write> TreeTokenWriter<W> {
    fn register(&mut self, data: Item) -> Tree {
        let result = Rc::new(data);
        self.root = Tree(result.clone());
        Tree(result)
    }
}
impl<W: Write> TokenWriter for TreeTokenWriter<W> {
    type Tree = Tree;
    type Error = TokenWriterError;

    fn done(&mut self) -> Result<(), Self::Error> {
        // Write header to byte stream
        self.out.write(b"BINJS")
            .map_err(TokenWriterError::WriteError)?;
        self.out.write_varnum(0)
            .map_err(TokenWriterError::WriteError)?;

        // Write grammar table to byte stream.
        self.grammar_table.write_with_compression(&mut self.out, &self.options.grammar_table)
            .map_err(TokenWriterError::WriteError)?;

        // Write strings table to byte stream.
        self.strings_table.write_with_compression(&mut self.out, &self.options.strings_table)
            .map_err(TokenWriterError::WriteError)?;

        // Write tree itself to byte stream.
        self.root.write_with_compression(&mut self.out, &self.options.tree)
            .map_err(TokenWriterError::WriteError)?;

        Ok(())
    }

    fn float(&mut self, value: Option<f64>) -> Result<Self::Tree, Self::Error> {
        let bytes : Vec<_> = bytes::float::bytes_of_float(value).iter().cloned().collect();
        Ok(self.register(Item::Encoded(bytes)))
    }

    fn bool(&mut self, data: Option<bool>)  -> Result<Self::Tree, Self::Error> {
        let bytes = bytes::bool::bytes_of_bool(data).iter().cloned().collect();
        Ok(self.register(Item::Encoded(bytes)))
    }
    fn string(&mut self, data: Option<&str>) -> Result<Self::Tree, Self::Error> {
        let key = data.map(str::to_string);
        let value = key.clone(); // FIXME: That's pretty wasteful.
        let index = self.strings_table.insert(key, value);
        Ok(self.register(Item::String(index)))
    }
    fn list(&mut self, children: Vec<Self::Tree>) -> Result<Self::Tree, Self::Error> {
        let mut items = Vec::with_capacity(children.len() + 1);
        // First child is the number of children.
        let mut encoded_number_of_items = Vec::with_capacity(8);
        encoded_number_of_items.write_varnum(children.len() as u32)
            .map_err(TokenWriterError::WriteError)?;
        items.push(Tree(Rc::new(Item::Encoded(encoded_number_of_items))));
        // Next, we have `children`.
        items.extend(children);
        let result = Item::List {
            items,
            byte_len: true
        };
        Ok(self.register(result))
    }
    fn untagged_tuple(&mut self, children: &[Self::Tree]) -> Result<Self::Tree, Self::Error> {
        let result = Item::List {
            items: children.iter().cloned().collect(),
            byte_len: false
        };
        Ok(self.register(result))
    }
    fn tagged_tuple(&mut self, tag: &str, children: &[(&Field, Self::Tree)]) -> Result<Self::Tree, Self::Error> {
        let mut data : Vec<Tree> = vec![];
        {
            let tag = match self.syntax.get_node_name(tag) {
                None => return Err(TokenWriterError::MissingKind(tag.to_string())),
                Some(node) => node
            };

            // Establish the order in which children need to be written.
            let mut order = vec![];
            let index = if let Some(entry) = self.grammar_table.get(tag) {
                entry.acquire();
                for name in &entry.data.fields {
                    order.push(name.clone())
                }
                Some(entry.index.clone())
            } else {
                for child in children {
                    order.push(child.0.name().clone())
                }
                None
            };
            let index = match index {
                None => {
                    let description = NodeDescription {
                        kind: tag.clone(),
                        fields: order.clone()
                    };
                    self.grammar_table.insert(tag.clone(), description)
                }
                Some(index) => index
            };

            // Now write data.
            data.push(Tree(Rc::new(Item::NodeDescription(index))));

            for field in order.drain(..) {
                for child in children {
                    if child.0.name() == &field {
                        data.push(child.1.clone());
                        break
                    }
                }
            }
        }
        Ok(self.register(Item::List {
            items: data,
            byte_len: false
        }))
    }
}

trait ExtendFromUTF8 {
    fn extend_from_str(&mut self, &str);
}

impl ExtendFromUTF8 for Vec<u8> {
    fn extend_from_str(&mut self, data: &str) {
        self.extend_from_slice(data.as_bytes());
    }
}

#[test]
fn test_simple_io() {
    use ast::annotation::*;
    use ast::grammar::*;

    use std::fs::*;

    use serde_json;
    use serde_json::Value as JSON;

    use std::io::{ Cursor, Write };

    type Object = serde_json::Map<String, JSON>;

    debug!("Setting up syntax");
    let mut builder = SyntaxBuilder::new();
    let null = builder.node_name("Null");
    let null_kind = builder.kind_name("Null");
    builder.add_kinded_interface(&null).unwrap();

    let kinded = builder.node_name("Pattern");
    let field_string = Field::new(builder.field_name("id"), Type::string());
    let field_number = Field::new(builder.field_name("value"), Type::number());

    builder.add_kinded_interface(&kinded).unwrap()
        .with_own_field(field_string.clone())
        .with_own_field(field_number.clone());

    struct FakeAnnotator;
    impl Annotator for FakeAnnotator {
        fn name(&self) -> String {
            unimplemented!()
        }
        fn process_references(&self, _: &Annotator, _: &mut Context<RefContents>, _: &mut Object) -> Result<(), ASTError> {
            unimplemented!()
        }
        fn process_declarations(&self, _: &Annotator, _: &mut Context<DeclContents>, _: &mut Object) -> Result<(), ASTError> {
            unimplemented!()
        }
    }

    let syntax = builder.into_syntax(SyntaxOptions {
        root: &kinded,
        null: &null_kind,
        annotator: Box::new(FakeAnnotator)
    });

    debug!("Testing string I/O");

    {
        let mut writer = TreeTokenWriter::new();
        writer.string(Some("simple string"))
            .expect("Writing simple string");

        File::create("/tmp/test-simple-string.binjs").unwrap()
            .write_all(writer.data()).unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(writer.data()), &syntax);
        let simple_string = reader.string()
            .expect("Reading simple string")
            .expect("Non-null string");
        assert_eq!(&simple_string, "simple string");
    }


    {
        let data = "string with escapes \u{0}\u{1}\u{0}";
        let mut writer = TreeTokenWriter::new();
        writer.string(Some(data))
            .expect("Writing string with escapes");

        File::create("/tmp/test-string-with-escapes.binjs").unwrap()
            .write_all(writer.data()).unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(writer.data()), &syntax);
        let escapes_string = reader.string()
            .expect("Reading string with escapes")
            .expect("Non-null string");
        assert_eq!(&escapes_string, data);
    }

    debug!("Testing untagged tuple I/O");

    {
        let mut writer = TreeTokenWriter::new();
        writer.untagged_tuple(&[])
            .expect("Writing empty untagged tuple");

        File::create("/tmp/test-empty-untagged-tuple.binjs").unwrap()
            .write_all(writer.data()).unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(writer.data()), &syntax);
        let _ = reader.untagged_tuple()
            .expect("Reading empty untagged tuplelist");
    }

    {
        let mut writer = TreeTokenWriter::new();
        let item_0 = writer.string(Some("foo")).unwrap();
        let item_1 = writer.string(Some("bar")).unwrap();
        writer.untagged_tuple(&[item_0, item_1])
            .expect("Writing trivial untagged tuple");

        File::create("/tmp/test-trivial-untagged-tuple.binjs").unwrap()
            .write_all(writer.data()).unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(writer.data()), &syntax);
        let mut sub = reader.untagged_tuple()
            .expect("Reading trivial untagged tuple");
        let simple_string = sub.string()
            .expect("Reading trivial tuple[0]")
            .expect("Non-null string");
        assert_eq!(&simple_string, "foo");
        let simple_string = sub.string()
            .expect("Reading trivial tuple[1]")
            .expect("Non-null string");
        assert_eq!(&simple_string, "bar");
    }

    debug!("Testing tagged tuple I/O");

    {
        let mut writer = TreeTokenWriter::new();
        let item_0 = writer.string(Some("foo")).unwrap();
        let item_1 = writer.float(Some(3.1415)).unwrap();
        writer.tagged_tuple(kinded.to_str(), &[(&field_string, item_0), (&field_number, item_1)])
            .expect("Writing trivial tagged tuple");

        File::create("/tmp/test-simple-tagged-tuple.binjs").unwrap()
            .write_all(writer.data()).unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(writer.data()), &syntax);
        let (name, fields, mut sub) = reader.tagged_tuple()
            .expect("Reading trivial tagged tuple");
        assert_eq!(name, "Pattern".to_string());

        // Order of fields is not deterministic
        if fields[0].name().to_string() == &"id".to_string() {
            assert_eq!(fields[0].name().to_string(), &"id".to_string());
            assert_eq!(*fields[0].type_(), Type::string());
            assert_eq!(fields[1].name().to_string(), &"value".to_string());
            assert_eq!(*fields[1].type_(), Type::number());
            let simple_string = sub.string()
                .expect("Reading trivial tagged tuple[0]")
                .expect("Reading a non-null string");
            let simple_float = sub.float()
                .expect("Reading trivial tagged tuple[1]")
                .expect("Reading a non-null float");
            assert_eq!(&simple_string, "foo");
            assert_eq!(simple_float, 3.1415);
        } else {
            assert_eq!(fields[1].name().to_string(), &"id".to_string());
            assert_eq!(*fields[1].type_(), Type::string());
            assert_eq!(fields[0].name().to_string(), &"value".to_string());
            assert_eq!(*fields[0].type_(), Type::number());
            let simple_float = sub.float()
                .expect("Reading trivial tagged tuple[1]")
                .expect("Reading a non-null float");
            let simple_string = sub.string()
                .expect("Reading trivial tagged tuple[0]")
                .expect("Reading a non-null string");
            assert_eq!(&simple_string, "foo");
            assert_eq!(simple_float, 3.1415);
        }
    }

    debug!("Testing list I/O");

    {
        let mut writer = TreeTokenWriter::new();
        writer.list(vec![])
            .expect("Writing empty list");

        File::create("/tmp/test-empty-list.binjs").unwrap()
                .write_all(writer.data()).unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(writer.data()), &syntax);
        let (len, _) = reader.list()
            .expect("Reading empty list");
        assert_eq!(len, 0);
    }

    {
        let mut writer = TreeTokenWriter::new();
        let item_0 = writer.string(Some("foo")).unwrap();
        let item_1 = writer.string(Some("bar")).unwrap();
        writer.list(vec![item_0, item_1])
            .expect("Writing trivial list");

        File::create("/tmp/test-trivial-list.binjs").unwrap()
            .write_all(writer.data()).unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(writer.data()), &syntax);
        let (len, mut sub) = reader.list()
            .expect("Reading trivial list");
        assert_eq!(len, 2);

        let simple_string = sub.string()
            .expect("Reading trivial list[0]")
            .expect("Non-null string");
        assert_eq!(&simple_string, "foo");
        let simple_string = sub.string()
            .expect("Reading trivial list[1]")
            .expect("Non-null string");
        assert_eq!(&simple_string, "bar");
    }

    {
        let mut writer = TreeTokenWriter::new();
        let item_0 = writer.string(Some("foo")).unwrap();
        let item_1 = writer.string(Some("bar")).unwrap();
        let list = writer.list(vec![item_0, item_1])
            .expect("Writing inner list");
        writer.list(vec![list])
            .expect("Writing outer list");

        File::create("/tmp/test-nested-lists.binjs").unwrap()
            .write_all(writer.data()).unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(writer.data()), &syntax);
        let (len, mut sub) = reader.list()
            .expect("Reading outer list");
        assert_eq!(len, 1);

        let (len, mut sub) = sub.list()
            .expect("Reading inner list");
        assert_eq!(len, 2);

        let simple_string = sub.string()
            .expect("Reading trivial list[0]")
            .expect("Non-null string");
        assert_eq!(&simple_string, "foo");
        let simple_string = sub.string()
            .expect("Reading trivial list[1]")
            .expect("Non-null string");
        assert_eq!(&simple_string, "bar");
    }

}
