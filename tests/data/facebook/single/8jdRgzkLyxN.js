if (self.CavalryLogger) { CavalryLogger.start_js(["Zr3Qg"]); }

__d("SharerType",[],(function a(b,c,d,e,f,g){f.exports={ALL_MODES:"all_modes",FRIEND_TIMELINE_ONLY:"friend_timeline_only",MESSAGE_ONLY:"message_only",OWN_ONLY:"own_only",PAGE_ONLY:"page_only",PAGE_VIEWER:"page_viewer"};}),null);
__d('SocialWifiCodeComposer',['Event','CSS','$','ge','Banzai','BanzaiNectar'],(function a(b,c,d,e,f,g){var h=c('BanzaiNectar').create(c('Banzai').VITAL);function i(j,k){this._form=j;this._appSection=k;this._useCodeLink=c('ge')('wifiCodeInsteadLink');this._skipCodeLink=c('ge')('wifiSkipCheckinLink');this._useCheckinLink=c('$')('wifiCodeInsteadGoBackLink');this.init();}Object.assign(i.prototype,{init:function j(){c('Event').listen(this._form,'success',function(event){if(!event.data.response.error)window.location=event.data.response.payload.redirect_uri;});if(this._useCodeLink)c('Event').listen(this._useCodeLink,'click',this.showCodeForm.bind(this));if(this._skipCodeLink)c('Event').listen(this._skipCodeLink,'click',this.skipCodeForm.bind(this));c('Event').listen(this._useCheckinLink,'click',this.hideCodeForm.bind(this));},_codeFormShown:false,showCodeForm:function j(){c('CSS').addClass(document.body,'fbWifiCodeComposerShowCode');if(!this._codeFormShown){h.log('socialwifi','code_instead',this._appSection);this._codeFormShown=true;}},hideCodeForm:function j(){c('CSS').removeClass(document.body,'fbWifiCodeComposerShowCode');},skipCodeForm:function j(){var event=document.createEvent('MouseEvents');event.initMouseEvent('click',true,true,window,0,0,0,0,0,false,false,false,false,0,null);c('$')('wifiCodeInsteadSubmit').dispatchEvent(event);}});f.exports=i;}),null);