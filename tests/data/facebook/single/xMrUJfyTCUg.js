if (self.CavalryLogger) { CavalryLogger.start_js(["UgkbK"]); }

__d('XFBConfirmationCliffTypedLogger',['Banzai','GeneratedLoggerUtils','nullthrows'],(function a(b,c,d,e,f,g){'use strict';function h(){this.clear();}h.prototype.log=function(){c('GeneratedLoggerUtils').log('logger:XFBConfirmationCliffLoggerConfig',this.$XFBConfirmationCliffTypedLogger1,c('Banzai').BASIC);};h.prototype.logVital=function(){c('GeneratedLoggerUtils').log('logger:XFBConfirmationCliffLoggerConfig',this.$XFBConfirmationCliffTypedLogger1,c('Banzai').VITAL);};h.prototype.clear=function(){this.$XFBConfirmationCliffTypedLogger1={};return this;};h.prototype.updateData=function(j){this.$XFBConfirmationCliffTypedLogger1=babelHelpers['extends']({},this.$XFBConfirmationCliffTypedLogger1,j);return this;};h.prototype.setCliffCaller=function(j){this.$XFBConfirmationCliffTypedLogger1.cliff_caller=j;return this;};h.prototype.setError=function(j){this.$XFBConfirmationCliffTypedLogger1.error=j;return this;};h.prototype.setSurface=function(j){this.$XFBConfirmationCliffTypedLogger1.surface=j;return this;};h.prototype.updateExtraData=function(j){j=c('nullthrows')(c('GeneratedLoggerUtils').serializeMap(j));c('GeneratedLoggerUtils').checkExtraDataFieldNames(j,i);this.$XFBConfirmationCliffTypedLogger1=babelHelpers['extends']({},this.$XFBConfirmationCliffTypedLogger1,j);return this;};h.prototype.addToExtraData=function(j,k){var l={};l[j]=k;return this.updateExtraData(l);};var i={cliff_caller:true,error:true,surface:true};f.exports=h;}),null);
__d('XFBConfirmationCliff',['XFBConfirmationCliffTypedLogger'],(function a(b,c,d,e,f,g){var h={logCliff:function i(event,j){if(event==='banner_seen'){var k=document.getElementsByClassName('hideBanner');if(k.length>0)return;}new (c('XFBConfirmationCliffTypedLogger'))().setSurface(event).setCliffCaller(j).log();}};f.exports=h;b.XFBConfirmationCliff=h;}),null);
__d('ConfirmationBannerError',['cx','Event','Input','XUIError'],(function a(b,c,d,e,f,g,h){var i={getErrorMessage:function j(k,l,m){if(k===l||k.length<5)return m;return null;},attachValidator:function j(k,l,m){function n(){var o=i.getErrorMessage(c('Input').getValue(k),l,m);if(o){c('XUIError').set({target:k,message:o});}else c('XUIError').clear(k);}c('Event').listen(k,'change',n);c('Event').listen(k,'keypress',n);c('Event').listen(k,'keyup',n);n();},attachCliffValidator:function j(k){if(!k)return;var l=document.getElementById('code_in_cliff');if(!l){k.disabled=false;CSS.removeClass(k,"_42fr");return;}l.oninput=function(){if(c('Input').getValue(l).length!==0){k.disabled=false;CSS.removeClass(k,"_42fr");}};}};f.exports=i;}),null);
__d('ConfirmationCliffDialog',[],(function a(b,c,d,e,f,g){var h={useSimplifiedBlueBar:function i(){var j=document.getElementById('bluebar_profile_and_home');if(j)j.parentNode.removeChild(j);var k=document.getElementById('bluebar_padlock');if(k)k.parentNode.removeChild(k);},expandResendButtons:function i(j,k){j.addEventListener('click',function(){CSS.hide(j);CSS.show(k);var l=document.getElementById('should_focus_button');if(l)l.focus();});}};f.exports=h;}),null);
__d('ProgressBarBase',['emptyFunction','requestAnimationFrame','removeFromArray','arrayContains'],(function a(b,c,d,e,f,g){var h=[];function i(j,k){'use strict';this._min=j||0;this._max=k||100;this._initialPosition=0;this._position=0;this._initialVelocity=0;this._velocity=0;this._acceleration=0;this.useAcceleration=true;this._targetPosition=0;this._targetTime=0;this._startTime=null;this._onComplete=c('emptyFunction');}i.prototype.setPosition=function(j){'use strict';j=this._normalizePosition(j);this._initialPosition=j;this._position=j;this.updateMeter(this._position);this.stop();return this;};i.prototype.setCompleteHandler=function(j){'use strict';this._onComplete=j||c('emptyFunction');return this;};i.prototype.setTarget=function(j,k){'use strict';this._stopAnimating();this._clearOnCompleteTimeout();this._targetPosition=j;var l=this._normalizePosition(j);this._targetTime=k;this._initialVelocity=this._velocity;this._initialPosition=this._position;if(this.useAcceleration){this._acceleration=2*(l-this._initialPosition-this._initialVelocity*k)/(k*k);}else{this._acceleration=0;this._velocity=this._initialVelocity=(l-this._initialPosition)/k;}if(this._position>=l){this._onComplete();}else this._start();return this;};i.prototype.setNoAcceleration=function(j){'use strict';this.useAcceleration=!j;return this;};i.prototype._clearOnCompleteTimeout=function(){'use strict';b.clearTimeout(this._onCompleteTimeout);};i.prototype.stop=function(){'use strict';this._clearOnCompleteTimeout();this._velocity=0;this._initialVelocity=0;this._acceleration=0;this._stopAnimating();return this;};i.prototype._start=function(){'use strict';this._startTime=Date.now();this._onCompleteTimeout=b.setTimeout(function(){this.setPosition(this._targetPosition);this._onComplete();}.bind(this),this._targetTime);this._startAnimating();return this;};i.prototype._loop=function(){'use strict';var j=Date.now()-this._startTime;this._position=.5*this._acceleration*j*j+this._initialVelocity*j+this._initialPosition;var k=this._velocity;this._velocity=this._acceleration*j+this._initialVelocity;var l=k<0!==this._velocity<0;if(this._position>this._normalizePosition(this._targetPosition)||l){this.setPosition(this._targetPosition);this._onComplete();}else this.updateMeter(this._position);};i.prototype.updateMeter=function(j){'use strict';throw new Error("Unimplemented function: updateMeter");};i.prototype._normalizePosition=function(j){'use strict';return Math.min(Math.max((j-this._min)/(this._max-this._min),0),1);};i.prototype._startAnimating=function(){'use strict';if(!c('arrayContains')(h,this)){h.push(this);if(h.length===1)c('requestAnimationFrame')(i.prototype._requestAnimationFrameCallback);}};i.prototype._stopAnimating=function(){'use strict';c('removeFromArray')(h,this);};i.prototype._requestAnimationFrameCallback=function(){'use strict';h.forEach(function(j){j._loop();});if(h.length)c('requestAnimationFrame')(i.prototype._requestAnimationFrameCallback);};i.setPosition=function(j,k){'use strict';j.setPosition(k);};i.setTarget=function(j,k,l){'use strict';j.setTarget(k,l);};f.exports=i;}),null);
__d('ProgressBar',['cx','csx','ProgressBarBase','CSS','Style','DOM'],(function a(b,c,d,e,f,g,h,i){var j,k;j=babelHelpers.inherits(l,c('ProgressBarBase'));k=j&&j.prototype;function l(m,n,o){'use strict';k.constructor.call(this,n,o);this._root=m;this._meter=c('DOM').find(m,"div._5e4k");this._meter2=c('DOM').scry(m,"div._5e2g")[0];}l.prototype.getRoot=function(){'use strict';return this._root;};l.prototype.updateMeter=function(m){'use strict';var n=Math.min(Math.max(m,0),1);c('CSS').conditionClass(this._meter,"_5e2d",n<=0);c('CSS').conditionClass(this._meter,"_5e4j",n>=1);this._root.setAttribute('aria-valuenow',n*100);n=n*100+'%';c('Style').set(this._meter,'width',n);if(this._meter2){c('Style').set(this._meter2,'left',n);c('Style').set(this._meter2,'width',n);}};l.prototype.changeLabel=function(m){'use strict';var n=c('DOM').scry(this._root,"span._5e2h");n.forEach(function(o){c('DOM').setContent(o,m);});return this;};f.exports=l;}),null);