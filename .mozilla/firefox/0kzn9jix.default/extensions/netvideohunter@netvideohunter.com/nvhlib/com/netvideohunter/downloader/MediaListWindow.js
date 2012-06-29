Components.utils.import("resource://nvhlib/com/netvideohunter/net/HttpRequest.js");
Components.utils.import("resource://nvhlib/com/netvideohunter/utils/Logger.js");

var EXPORTED_SYMBOLS = ["MediaListWindow"];

var MediaListWindow=function(main,window) {
	this.main=main;
	this.document=window.document;
	var listBox=null;
	this.embedWindow=null;
	this.browser=null;

	var self=this;
    var homeLoaded=false;
    var firstUse=1;

	var updateSelectButtons=function(){
		//var selButton=document.getElementById("removeSelectedButton");
		var downloadButton=self.document.getElementById("downloadSelectedButton");
		if(listBox.hasCheckedItem){
			//selButton.setAttribute("disabled","false");
			downloadButton.setAttribute("disabled","false");
		}else{
			//selButton.setAttribute("disabled","true");
			downloadButton.setAttribute("disabled","true");
		}
	}

	var onMediaAdded=function(event){
		self.updateTitle();
	}

	var onMediaRemoved=function(event){
		self.updateTitle();
	}

	var onMediaListCleared=function(event){
		self.updateTitle();
	}

	this.updateTitle=function(){
		var winElement=window.document.getElementById("netvideohunterMediaListWin");
		if(!winElement){
			var wm = Components.classes["@mozilla.org/appshell/window-mediator;1"].getService(Components.interfaces.nsIWindowMediator);
			winElement = wm.getMostRecentWindow("netvideohunter")
			if(winElement) winElement=winElement.document.getElementById("netvideohunterMediaListWin");
		}
		if(winElement){
			winElement.setAttribute("title","NetVideoHunter ("+self.main.mediaList.getLength()+")");
		}

	}

	this.onLoad=function(event){
		
		this.document=window.document;
		listBox = this.document.getElementById("netvideohunterMediaList");
		if(this.main){
			listBox.data=this.main.mediaList;   
		}
		listBox.addEventListener("checked",this.itemChecked,false);
		updateSelectButtons();
		this.browser = this.document.getElementById("homeBrowser");	

        var pref=Components.classes["@mozilla.org/preferences-service;1"]
                            .getService(Components.interfaces.nsIPrefBranch)
        this.firstUse=(pref.getBoolPref("extensions.netvideohunter.firstUse")) ? 1 : 0;
        var infoIsOpen=(this.browser.boxObject.width>0) ? 1 : 0;                

        
        var main=this.main
        var self=this;
        var messageRequest=new HttpRequest()
        var nocache=new Date();
        nocache=Math.round(nocache.getTime()/(60*60))
        messageRequest.reqUrl="http://www.netvideohunter.com/addon_message_id?nocache="+nocache+"&infoisopen="+infoIsOpen+"&firstuse="+this.firstUse;
        messageRequest.onLoad=function(success,response){        
            if(success && response && response.length>0){
                  var lastMessageId=pref.getIntPref("extensions.netvideohunter.lastMessageId");
                  if(lastMessageId!=response){
                      if(!infoIsOpen){
                        self.goHome();
                        self.openRightBrowser();
                      }
                      pref.setIntPref("extensions.netvideohunter.lastMessageId",response);
                  }
            }                   
        } 
        messageRequest.send()
        pref.setBoolPref("extensions.netvideohunter.firstUse",false);  
        if(infoIsOpen){ 
            this.goHome();        
        }else{            
            this.document.getElementById("windowSplitter").addEventListener("click",this.onSplitterClick,false);
        }

		this.document.getElementById("netvideohunterWarning").setAttribute("hidden",this.main.enabled);
		this.main.addEventListener("enabledChanged",this.onEnabledChanged,this);
		this.main.mediaList.addEventListener("itemAdded",onMediaAdded,this);
		this.main.mediaList.addEventListener("itemRemoved",onMediaRemoved,this);
		this.main.mediaList.addEventListener("cleared",onMediaListCleared,this);
		this.updateTitle();
		this.regLastWindowObserver();
	}


	this.onUnload=function(event){		
		this.document.getElementById("netvideohunterMediaList").removeEventListener("checked",this.itemChecked,false);
		this.document.getElementById("windowSplitter").removeEventListener("click",this.onSplitterClick,false);
		this.main.removeEventListener("enabledChanged",this.onEnabledChanged);
		this.main.mediaList.removeEventListener("itemAdded",onMediaAdded)
		this.main.mediaList.removeEventListener("itemRemoved",onMediaRemoved)
		this.main.mediaList.removeEventListener("cleared",onMediaListCleared)
		this.unregLastWindowObserver();
		Logger.log(listBox);
		var listBox = this.document.getElementById("netvideohunterMediaList");
		listBox.removeAllDataListener(); //to remove listBox from memory
	}

	this.regLastWindowObserver=function(){
		try{
			var observerService = Components.classes["@mozilla.org/observer-service;1"].getService(Components.interfaces.nsIObserverService);
			observerService.addObserver(this.lastWindowObserver, "browser-lastwindow-close-granted", false);
		}catch(e){
			Logger.log(e);
		}
		
	}

	this.unregLastWindowObserver=function(){
		try{
			var observerService = Components.classes["@mozilla.org/observer-service;1"].getService(Components.interfaces.nsIObserverService);
			observerService.removeObserver(this.lastWindowObserver, "browser-lastwindow-close-granted");
		}catch(e){
			Logger.log(e);
		}
		
	}

	this.lastWindowObserver={
		observe:function(aSubject, aTopic, aData) {
			if(window && window.close) window.close();
		}
	}

	this.onEnabledChanged=function(event){
		this.document.getElementById("netvideohunterWarning").setAttribute("hidden",this.main.enabled);
	}
    
    this.onSplitterClick=function(){
      if(!homeLoaded){
         self.goHome();  
      }      
    }
	
	this.onBrowserClick=function(){
		//if(this.browser.currentURI!="chrome://netvideohunter/content/options.xul"){
			//document.getElementById('windowSplitter').setAttribute('substate','before');
			//document.getElementById('windowSplitter').setAttribute('state','collapsed');
		//}
	}

	this.onOptionsClick=function(){
		this.showOptions();
		
	}

	this.onCloseOptionsClick=function(){
		this.browser.goBack();
		this.showBrowser();
	}

	this.itemChecked=function(event){
		updateSelectButtons();
	}

	this.onSelectAll=function(event){
		listBox.checkAll();
		this.toggleAllNoneButtons()
	}
	
	this.onSelectNone=function(event){
		listBox.checkNone();
		this.toggleAllNoneButtons()
	}

	this.toggleAllNoneButtons=function(){
		var selectAllIsHidden=(this.document.getElementById('selectAllButton').getAttribute('hidden')=='true');
		this.document.getElementById('selectAllButton').setAttribute('hidden',!selectAllIsHidden);
		var selectNoneIsHidden=(this.document.getElementById('selectNoneButton').getAttribute('hidden')=='true');
		this.document.getElementById('selectNoneButton').setAttribute('hidden',!selectNoneIsHidden);
	}
	
	this.goHome=function(){	
		this.showBrowser();
		this.browser.loadURI("http://netvideohunter.com/pages/addon-news-new?&version="+this.main.version+"&firstuse="+this.firstUse);
        this.homeLoaded=true;
	}		

	this.performCommand=function (aCmd, aItem){
		if (!aItem) {
			aItem= listBox.selectedItem;
		} else {
			while ((aItem.nodeName != "richlistitem") && aItem.parentNode){				
                aItem = aItem.parentNode;
                
			}
		}
		var id=aItem.getAttribute("id");

		if(aCmd=="onpopupshow"){
			var mediaId=aItem.getAttribute("id");
			var mediaItem=this.main.mediaList.getItemById(mediaId);
			if(mediaItem && mediaItem.youtubeQualityUrls && mediaItem.youtubeQualityUrls.length>1){
				var qualityUrls=mediaItem.youtubeQualityUrls;
				
				var popup=this.document.getElementById("itemMenu");
				var sep = this.document.createElement("menuseparator");
				sep.setAttribute("class","itemMenu-qualityItem");
				popup.appendChild(sep);
				for(var i=0; i < qualityUrls.length; i++){
					var qualityItem = this.document.createElement("menuitem");
					var qualityName = qualityUrls[i].qualityName;
					Logger.log("generate ")
					Logger.log(qualityName)
					qualityItem.setAttribute("label","Download in "+qualityName+" quality");
					//sep.setAttribute("oncommand","com.netvideohunter.downloader.MediaList.main.saveUrl('+qualityUrls[i].url+')");
					qualityItem.setAttribute("class","itemMenu-qualityItem");
					
					qualityItem.addEventListener("click",
						function(id,quality){ 
							return function(event) { 
								Logger.log(this);
								Logger.log(id);
								Logger.log(quality);
								window.com.netvideohunter.downloader.MediaListWindow_Instance.main.download(id,null,false,quality);
								window.com.netvideohunter.downloader.MediaListWindow_Instance.markDownloaded(id);
							}
						}(mediaId,qualityName));
					
					
					popup.appendChild(qualityItem);
				}
			}
		}

		if(aCmd=="onpopuphide"){
			var qualityItems = this.document.getElementsByClassName("itemMenu-qualityItem");
			var popup = this.document.getElementById("itemMenu");
			while( popup.childNodes.length > 3 ) {			
				popup.removeChild(popup.childNodes[popup.childNodes.length-1]);
			}
		}

		if(aCmd=="download"){
			aItem.setAttribute("downloadStarted",true)
			this.main.download(id)		
			aItem.selected=false
		}
		if(aCmd=="downloadcache"){
			aItem.setAttribute("downloadStarted",true)
			this.main.download(id,null,true)		
			aItem.selected=false
		}
        if(aCmd=="openpage"){
			this.openPage(id)
		}
		if(aCmd=="play"){			
			this.insertPlayer(this.main.mediaList.getItemById(id));
		}	 
		if(aCmd=="externalPlay"){
			this.main.playInExternal(this.main.mediaList.getItemById(id).url)
		}	
		if(aCmd=="copy"){
			this.copyURL(id);
		}
		if(aCmd=="embed"){
			this.openEmbed(id);					
		}
		
	}

	this.downloadSelected=function(){
		var ids=listBox.getCheckedIds()
			var titles={};
		for(var i=0;i < ids.length;i++){
			var mediaTitle=main.mediaList.getItemById(ids[i]).title;
			
			while(typeof(titles[mediaTitle])!='undefined'){
				var r=/(\d+)$/;				
				var m = r.exec(mediaTitle);				
				if (m != null && typeof(m[1])!='undefined') {
					var newNum=Number(m[1])+1;
					mediaTitle=mediaTitle.replace(r,newNum);
				}else{
					mediaTitle=mediaTitle+" 1";
				}
			}
			main.mediaList.getItemById([ids[i]]).title=mediaTitle;
			this.markDownloaded(ids[i])
			this.main.download(ids[i],true);
			titles[mediaTitle]=true;			
		}
	}

	this.markDownloaded=function(id){
		this.document.getElementById(id).setAttribute("downloadStarted",true);
	}

	this.insertPlayer=function(mediaItem){
		var url=mediaItem.url;
		url=this.addQueryParam(url,'nvhnocache','1');


		var playerContainer=this.document.getElementById("playerContainer")
		if(mediaItem.type=="webm" || mediaItem.type=="ogg"){
			url=url.replace(/&/g,"&amp;");
			Logger.log(url);
			playerContainer.innerHTML="<html:video src='"+url+"' width='100%' height='100%' autoplay='true' controls='true' flex='1' style='background:black'></html:video>"
		}else{
			url=encodeURIComponent(url);
			playerContainer.innerHTML="<html:embed id='player' flex='1' width='100%' height='100%' src='chrome://netvideohunter/content/NetMediaPlayer.swf' allowfullscreen='true' allowscriptaccess='always' flashvars='autostart=true&amp;showstop=true&amp;usefullscreen=true&amp;file="+url+"' visible='false'/>"
		}
		this.showPlayer()
		//box.setAttribute("style","overflow:hidden")
		
	}

	this.showBrowser=function(){	
		var player=this.document.getElementById("playerContainer");
		player.setAttribute("style","display:none");
		var box=this.document.getElementById("homeBrowser");
		box.setAttribute("style","display:block");
		this.document.getElementById("closeOptionsButton").setAttribute("hidden","true");
		this.document.getElementById("closePlayerButton").setAttribute("hidden","true");
		this.document.getElementById("optionsButton").setAttribute("hidden","false");
		this.document.getElementById("netvideohunterMediaListWin").setAttribute("playerIsActive","false");
	}

	this.showPlayer=function(){	
		this.openRightBrowser();
		var player=this.document.getElementById("playerContainer")
		player.setAttribute("style","display:block;height:100%;-moz-box-flex:1;")
		player.style.overflow="hidden";
		var box=this.document.getElementById("homeBrowser")
		box.setAttribute("style","display:none")
		this.document.getElementById("optionsButton").setAttribute("hidden","false")
		this.document.getElementById("closeOptionsButton").setAttribute("hidden","true")
		this.document.getElementById("closePlayerButton").setAttribute("hidden","false")
		this.document.getElementById("netvideohunterMediaListWin").setAttribute("playerIsActive","true");
	}

	this.showOptions=function(){	
		this.browser.loadURI("");
		this.browser.loadURI("chrome://netvideohunter/content/options.xul");
		this.showBrowser();
		this.document.getElementById('optionsButton').setAttribute('hidden','true');
		this.document.getElementById('closeOptionsButton').setAttribute('hidden','false');
		this.document.getElementById("closePlayerButton").setAttribute("hidden","true")
		this.document.getElementById('windowSplitter').setAttribute('state','open');
		this.document.getElementById("netvideohunterMediaListWin").setAttribute("playerIsActive","false");
	}

	this.openRightBrowser=function(){
		this.document.getElementById('windowSplitter').setAttribute('state','open');
	}

	this.addQueryParam=function(source,param,value){
	  if(source.indexOf("?")==-1){
		source+="?";
	  }
	  urlsplit=source.split("?");
	  urlsplit[1]+="&"+param+"="+value;
	  return urlsplit.join("?");
	}

	this.copyURL=function(id){
		const gClipboardHelper = Components.classes["@mozilla.org/widget/clipboardhelper;1"].
                                    getService(Components.interfaces.nsIClipboardHelper);
		gClipboardHelper.copyString(this.main.mediaList.getItemById(id).url);

	}

    this.openPage=function (id) {
		var wm = Components.classes["@mozilla.org/appshell/window-mediator;1"].getService(Components.interfaces.nsIWindowMediator);
        var w = wm.getMostRecentWindow("navigator:browser");
		var newTab=w.gBrowser.addTab(this.main.mediaList.getItemById(id).pageUrl); 
		w.gBrowser.selectedTab=newTab;
    }

	
}