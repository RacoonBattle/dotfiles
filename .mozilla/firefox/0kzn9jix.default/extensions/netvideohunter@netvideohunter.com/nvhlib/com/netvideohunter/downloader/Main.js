Components.utils.import("resource://nvhlib/com/netvideohunter/data/List.js");
Components.utils.import("resource://nvhlib/com/netvideohunter/data/HashList.js");
Components.utils.import("resource://nvhlib/com/netvideohunter/net/HttpRequest.js");
Components.utils.import("resource://nvhlib/com/netvideohunter/net/HttpUtils.js");
Components.utils.import("resource://nvhlib/com/netvideohunter/downloader/data/MediaData.js");
Components.utils.import("resource://nvhlib/com/netvideohunter/utils/Logger.js");
Components.utils.import("resource://nvhlib/com/netvideohunter/utils/EventDispatcher.js"); 

var EXPORTED_SYMBOLS = ["Main"];

// === Main ===

var Main=new function () {					// SINGLETON CLASS
	EventDispatcher.call(this);

	
	this.getExtension=function(){ // not working in FF4 ?
		Application = Components.classes["@mozilla.org/fuel/application;1"].getService(Components.interfaces.fuelIApplication);
		var extension=null;
		if(Application.extensions){
			return Application.extensions.get('netvideohunter@netvideohunter.com');
		}else if(Application.getExtensions){
			Application.getExtensions(function (extensions){
				extension=extensions.get('netvideohunter@netvideohunter.com');
			});
		}
		return extension;
	}


    // PRIVATE METHODS:
	this.version="unknow";
    var getVersion=function(){	
		try{
			Components.utils.import("resource://gre/modules/AddonManager.jsm");
			AddonManager.getAddonByID("netvideohunter@netvideohunter.com", function(addon) {
			   self.version=addon.version;
			});
		}catch(e){}
		if(self.version=="unknow"){
			var ext=self.getExtension();
			if(ext) self.version=ext.version;
		}
		
        return self.version;
    }

	
    var uniqueFile = function(aLocalFile){
		
        while (!createFile(aLocalFile)) {		
			var r=/(\d+)?(\.[^.]+)?$/;			
			var m = r.exec(aLocalFile.leafName);				
			if (m != null && typeof(m[1])!='undefined') {
				var newNum=Number(m[1])+1;
				aLocalFile.leafName=aLocalFile.leafName.replace(r,String(newNum)+m[2]);
			}else{
				aLocalFile.leafName=aLocalFile.leafName.replace(/(.)?(\.[^.]+)?$/,"$1 1$2");
			}		
        }
        return aLocalFile;
    }

	var createFile=function(file){
		try{
			file.create(0,0777)
			 return true;
		}catch(e){
			if(e.name=="NS_ERROR_FILE_ALREADY_EXISTS"){
				return false;
			}
			return true;
		}
	}

    var getDefaultDownloadDir = function(){

        var dir;
        var downloadPrefs=Components.classes["@mozilla.org/preferences-service;1"].getService(Components.interfaces.nsIPrefService).getBranch("browser.download.");
 
        // 1. try last used download directory
        try {
            dir = downloadPrefs.getComplexValue("lastDir", Components.interfaces.nsILocalFile);
        } catch(e) {Logger.log(e);}
        var dirExists = dir && dir.exists();

        if(!dirExists){
            // 2. try download manager default directory:
            try{
                var dlMgr = Components.classes["@mozilla.org/download-manager;1"].getService(Components.interfaces.nsIDownloadManager);
                dir = dlMgr.userDownloadsDirectory;

            }catch(e){Logger.log(e);}

            dirExists = dir && dir.exists();
            if(!dirExists){
                // 3. use desktop directory:
                var fileLocator = Components.classes["@mozilla.org/file/directory_service;1"].getService(Components.interfaces.nsIProperties);
                dir = fileLocator.get("Desk", Components.interfaces.nsILocalFile);
            }
        }

        return dir;
    }

	var cacheListener=new function(){
		this.onCacheEntryAvailable=function(descriptor,accessGranted,status){		
           
		}
	}

    var isInCache=function(key){
        var cacheService = Components.classes['@mozilla.org/network/cache-service;1'].getService(Components.interfaces.nsICacheService);
        var httpCacheSession = cacheService.createSession("HTTP", Components.interfaces.nsICache.STORE_ANYWHERE, true);
        httpCacheSession.doomEntriesIfExpired = false;

        try {
            var cacheEntryDescriptor = httpCacheSession.openCacheEntry(key, 1, false);
        }catch (e){
            Logger.log(e);
            if ( e.name == "NS_ERROR_CACHE_WAIT_FOR_VALIDATION" )
				httpCacheSession.asyncOpenCacheEntry(key, 1, cacheListener)
				return false;

            if ( e.name == "NS_ERROR_CACHE_KEY_NOT_FOUND" )
				return false;

            return false;
        }
		//cacheEntryDescriptor.setExpirationTime(3600*24);
        return cacheEntryDescriptor;
    }

	


    var forceCacheSites=["trilulilu","grooveshark"];

    // PUBLIC METHODS:

    this.observe=function(aSubject, aTopic, aData) {
         
        try{
            
            var request=aSubject.QueryInterface(Components.interfaces.nsIRequest);
			var channel=aSubject.QueryInterface(Components.interfaces.nsIChannel); 
		 
			if(aTopic=="http-on-modify-request"){
                var httpChannel=aSubject.QueryInterface(Components.interfaces.nsIHttpChannel); 
                //HttpUtils.forceCaching(httpChannel);
                /*var cachingChannel = aSubject.QueryInterface(Components.interfaces.nsICachingChannel); 
                cachingChannel.cacheForOfflineUse=true;*/

                return true;  
            }
            // filter ads:
            //if(aSubject.name.indexOf("doubleclick.net")!=-1) return true;
            //if(aSubject.name.indexOf("upload_status")!=-1) return true;
            var mData=new MediaData(this)
			mData.captureSwf=this.captureSwf;
            mData.url=request.name;            
            mData.contentType=channel.contentType
            mData.size=channel.contentLength

            if(mData.type!==null){
            
				var httpChannel=aSubject.QueryInterface(Components.interfaces.nsIHttpChannel); 
				HttpUtils.forceCaching(httpChannel);              
				// filter video played in the built-in player:
				if(mData.url.indexOf("nvhnocache=1")!=-1) return null;                
				// referrer:
				if(httpChannel.referrer) mData.referrer=httpChannel.referrer.spec;
              
				// requestMethod:
				mData.requestMethod=httpChannel.requestMethod;
                //upload stream for POST
				mData.uploadStream=HttpUtils.getUploadStream(httpChannel);
                //caching channel
                if (channel instanceof Components.interfaces.nsICachingChannel){
                    var cachingChannel = aSubject.QueryInterface(Components.interfaces.nsICachingChannel); 
                    mData.cacheKey= cachingChannel.cacheKey                 
                    /*var token = cachingChannel.cacheToken;
                    var info = token.QueryInterface(Components.interfaces.nsICacheEntryInfo);
                    mData.cacheKey=info.key;*/
                    
                }
				// getDOMWindow
				var DOMWindow;
                var interfaceRequestor;
                try{       
                    if(request.loadGroup && request.loadGroup.notificationCallbacks){
                        interfaceRequestor = request.loadGroup.notificationCallbacks.QueryInterface(Components.interfaces.nsIInterfaceRequestor);
                        DOMWindow = interfaceRequestor.getInterface(Components.interfaces.nsIDOMWindow);
                    }
                }catch(e){ Logger.log(e)}

                if(!DOMWindow){
                    try{                              
           
                        interfaceRequestor = httpChannel.notificationCallbacks.QueryInterface(Components.interfaces.nsIInterfaceRequestor);
                        DOMWindow = interfaceRequestor.getInterface(Components.interfaces.nsIDOMWindow);
                    }catch(e){ Logger.log(e)}
                }               
               
                if(!DOMWindow) return false;
				// filter video played in the built-in player:
				if(DOMWindow && DOMWindow.document){ 
					var w=DOMWindow.document.getElementById("netvideohunterMediaListWin");
					if(w){
						if(w.getAttribute("playerIsActive")=="true") return null;
					}
				}
				mData.setWindow(DOMWindow);
                  Logger.log(mData);
				if(this.ignoreSites.getItemById(mData.host)){
					return null;	
				}else{                
					this.mediaList.addItem(mData);
				}
                
            }else{
                delete mData;
            }
        }catch (e) {
            Logger.log(e)
        }

        return null;
    }

    this.regListener=function(){
        var observerService = Components.classes["@mozilla.org/observer-service;1"].getService(Components.interfaces.nsIObserverService);
        observerService.addObserver(this, "http-on-examine-response", false);
        observerService.addObserver(this, "http-on-examine-cached-response", false);
        observerService.addObserver(this, "http-on-examine-merged-response", false);
        observerService.addObserver(this, "http-on-modify-request", false);
    }

    this.unregListener=function(){
        var observerService = Components.classes["@mozilla.org/observer-service;1"].getService(Components.interfaces.nsIObserverService);
        observerService.removeObserver(this, "http-on-examine-response");
        observerService.removeObserver(this, "http-on-examine-cached-response");
        observerService.removeObserver(this, "http-on-examine-merged-response");
        observerService.removeObserver(this, "http-on-modify-request");
    }



	var prefBranch;

	this.regPrefObserver=function(){
		var prefs = Components.classes["@mozilla.org/preferences-service;1"].getService(
                          Components.interfaces.nsIPrefService);
        prefBranch = prefs.getBranch("extensions.netvideohunter.");
        prefBranch.QueryInterface(Components.interfaces.nsIPrefBranch2);
        prefBranch.addObserver("", self.prefObserver, false);
	}

	this.prefObserver={
		observe:function(subject, topic, data) {
			if(data=="captrueSwf"){
				self.captureSwf=subject.getBoolPref(data);
			}
            if(data=="disabled"){

            }
		}
	}

	this.unregPrefOBserver=function(){
		try {
            prefBranch.removeObserver("", this.prefObserver);
        } catch (err) {
			
        }
	}




    this.onMediaAdded=function(event){

    }

    this.onMediaRemoved=function(event){

    }

    this.onMediaListClear=function(event){

    }

	this.downloadMostRecent=function(){
		var lastId=this.mediaList.getLastId();
		if(lastID!==null) this.download(lastId);
	}

    this.download=function(id,skipPrompt,forceUseCache,quality){
        try{
            var mediaItem=this.mediaList.getItemById(id);
            if(typeof(quality)!="undefined" && quality!=null){                
                mediaItem.youtubeQuality=quality;
            }else if(mediaItem.youtubeQualityUrls && mediaItem.youtubeQualityUrls.length>0 && this.pref.getBoolPref("extensions.netvideohunter.download.bestQuality")){
                mediaItem.youtubeQuality=mediaItem.youtubeQualityUrls[0].qualityName;
            }
            var statEnabled=true;
            statEnabled=self.pref.getBoolPref("extensions.netvideohunter.statEnabled");
            //stat:
            if(this.stat && statEnabled){
                var params="data[Videostat][mediaurl]="+encodeURIComponent(mediaItem.url)
                params+="&data[Videostat][pageurl]="+encodeURIComponent(mediaItem.pageUrl)
                params+="&data[Videostat][title]="+encodeURIComponent(mediaItem.title)
                params+="&data[Videostat][size]="+encodeURIComponent(mediaItem.size)
                params+="&data[Videostat][type]="+encodeURIComponent(mediaItem.type)
                this.stat.send(params);
            }
            //start saving:           
            this.saveUrl(mediaItem,skipPrompt,forceUseCache);            
            mediaItem.downloadStarted=true;
            if(this.pref.getBoolPref("extensions.netvideohunter.list.removeAfterDownload")){
                this.mediaList.removeItem(id);
            }
        }catch(e){
            Logger.log(e);
        }
    }



    this.saveUrl=function(mediaItem, aSkipPrompt, forceUseCache){
        var targetFileURI=this.getTargetFile(mediaItem.filename, aSkipPrompt);
  
        if(targetFileURI===false) return false;

        // make persist:
        var persist = Components.classes["@mozilla.org/embedding/browser/nsWebBrowserPersist;1"].createInstance(Components.interfaces.nsIWebBrowserPersist);

        // Calculate persist flags.
        const nsIWBP = Components.interfaces.nsIWebBrowserPersist;
        const flags = nsIWBP.PERSIST_FLAGS_REPLACE_EXISTING_FILES;
        persist.persistFlags = flags;  

		var cacheEntry = isInCache(mediaItem.cacheKey);
        Logger.log("Cache key: "+mediaItem.cacheKey);
		var cacheKey=null;
        if((forceCacheSites.indexOf(mediaItem.site)!=-1 || forceUseCache || (cacheEntry && cacheEntry.dataSize>0))){
            persist.persistFlags |= nsIWBP.PERSIST_FLAGS_FROM_CACHE;
            cacheKey=mediaItem.cacheKey;
			Logger.log("using cache");

        }else{
            persist.persistFlags |= nsIWBP.PERSIST_FLAGS_BYPASS_CACHE;
        }        
		
        persist.persistFlags |= nsIWBP.PERSIST_FLAGS_AUTODETECT_APPLY_CONVERSION;
        persist.persistFlags |= nsIWBP.PERSIST_FLAGS_DONT_CHANGE_FILENAMES    

        var ioService=Components.classes["@mozilla.org/network/io-service;1"].getService(Components.interfaces.nsIIOService)

        var sourceURI=ioService.newURI(mediaItem.url,"UTF-8",null);

        var tr = Components.classes["@mozilla.org/transfer;1"].createInstance(Components.interfaces.nsITransfer);
        tr.init(sourceURI,targetFileURI, "", null, null, null, persist);
        persist.progressListener = {
			onProgressChange: function(aWebProgress, aRequest, aCurSelfProgress, aMaxSelfProgress, aCurTotalProgress, aMaxTotalProgress) {
				var percentComplete = (aCurTotalProgress/aMaxTotalProgress)*100;
				//Logger.log(percentComplete +"%");
				tr.onProgressChange(aWebProgress, aRequest, aCurSelfProgress, aMaxSelfProgress, aCurTotalProgress, aMaxTotalProgress);
			},
			onStateChange: function(aWebProgress, aRequest, aStateFlags, aStatus) {
				//Logger.log("status: "+aStatus);
				tr.onStateChange(aWebProgress, aRequest, aStateFlags, aStatus);
			},
			onDownloadStateChange: function(aState, aDownload){
				//Logger.log("state:"+aState);
				tr.onDownloadStateChange(aState, aDownload);
			},
			onSecurityChange : function(prog, req, state, dl) {
				tr.onSecurityChange(prog, req, state, dl);
			}
		}
		var referrerURI=null;
		if(mediaItem.referrer){
			try{
				referrerURI=ioService.newURI(mediaItem.referrer,"UTF-8",null)
			}catch(e){ Logger.log(e); }
		}
        persist.saveURI(sourceURI,cacheKey, referrerURI, mediaItem.uploadStream, null, targetFileURI);
       
    }

    this.getTargetFile=function(fileName, aSkipPrompt){
       
        var prefDirType=this.pref.getIntPref("extensions.netvideohunter.download.dirType");
        if(typeof(aSkipPrompt)=="undefined"){
            aSkipPrompt=(prefDirType!=1);
        }
        var askDownloadDir = (prefDirType==1 && !aSkipPrompt);
        

        var inPrivateBrowsing = false;
        try {
            var pbs = Components.classes["@mozilla.org/privatebrowsing;1"].getService(Components.interfaces.nsIPrivateBrowsingService);
            inPrivateBrowsing = pbs.privateBrowsingEnabled;
        }catch (e) {Logger.log(e);}
       
        var downloadDir = getDefaultDownloadDir();

        if (!askDownloadDir) {
            if(prefDirType==3){
                var userDownloadDir=this.pref.getComplexValue("extensions.netvideohunter.download.customDir",Components.interfaces.nsILocalFile);
                if(userDownloadDir && userDownloadDir.exists()){
                    downloadDir=userDownloadDir;
                }
            }

            downloadDir.append(fileName);
            var ioService = Components.classes["@mozilla.org/network/io-service;1"].getService(Components.interfaces.nsIIOService);
            return ioService.newFileURI(uniqueFile(downloadDir));
        }

        // get window:
        var wm = Components.classes["@mozilla.org/appshell/window-mediator;1"].getService(Components.interfaces.nsIWindowMediator);
        var window = wm.getMostRecentWindow("navigator:browser");

        var fp = Components.classes["@mozilla.org/filepicker;1"].createInstance(Components.interfaces.nsIFilePicker);
        fp.init(window, "NetVideoHunter Save",Components.interfaces.nsIFilePicker.modeSave);
        fp.displayDirectory = downloadDir;
        fp.defaultExtension = fileName.substr(fileName.lastIndexOf(".")+1);
        fp.defaultString = fileName;

        if (fp.show() == Components.interfaces.nsIFilePicker.returnCancel || !fp.file)
            return false;

        // Do not store the last save directory as a pref inside the private browsing mode       
        if (!inPrivateBrowsing){
            var lastDownloadDir = fp.file.parent.QueryInterface(Components.interfaces.nsILocalFile);
            var downloadPrefs=Components.classes["@mozilla.org/preferences-service;1"].getService(Components.interfaces.nsIPrefService).getBranch("browser.download.");
            downloadPrefs.setComplexValue("lastDir", Components.interfaces.nsILocalFile, lastDownloadDir);
        }

        return fp.fileURL;
    }

	this.__defineSetter__("enabled", function(value){
        if(_enabled){
            this.unregListener();
            this.pref.setBoolPref("extensions.netvideohunter.disabled",true);
        }else{
            this.regListener();
            this.pref.setBoolPref("extensions.netvideohunter.disabled",false);
        }
        _enabled=!_enabled
		this.dispatchEvent({type:'enabledChanged',value:_enabled});
    });

    this.__defineGetter__("enabled", function(){
        return _enabled;
    });



	this.message=function(message){
		var wm = Components.classes["@mozilla.org/appshell/window-mediator;1"].getService(Components.interfaces.nsIWindowMediator);
        var window = wm.getMostRecentWindow("navigator:browser");
		window.alert(message);

	}





	
    // CONSTRUCTOR:    
    var self=this;

	this.ignoreSites=new HashList('id',"extensions.netvideohunter.ignoreSites");
	this.ignoreSites.saveEachChange=true;
    this.mediaList=new HashList('id');			// list of com.netvideohunter.downloader.MediaData objects
    this.mediaList.addEventListener("itemAdded",this.onMediaAdded,this);
    this.mediaList.addEventListener("itemRemoved",this.onMediaRemoved,this);
    this.mediaList.addEventListener("cleared",this.onMediaListClear,this);
    this.pref=Components.classes["@mozilla.org/preferences-service;1"].getService(Components.interfaces.nsIPrefBranch);
    this.version=getVersion();
    var _enabled=!this.pref.getBoolPref("extensions.netvideohunter.disabled");
	
    this.stat=new HttpRequest();
    this.stat.reqUrl="http://www.netvideohunter.com/videostats/add";
    this.stat.onLoad=function(success,response){
        if(success && response && response.length>0){
			//Logger.log("Stat: "+response);
        }
    };
    this.captureSwf=this.pref.getBoolPref("extensions.netvideohunter.captureSwf");
    if(_enabled){
        this.regListener(); 
    }
	
	this.regPrefObserver();




}

