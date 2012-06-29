Components.utils.import("resource://nvhlib/com/netvideohunter/utils/Logger.js");
Components.utils.import("resource://nvhlib/com/netvideohunter/net/UriUtils.js");

var EXPORTED_SYMBOLS = ["MediaData"]; 

// === MediaData ===

var MediaData=function(main){
	var main=main;
	this.id=null;
	this.extension=null;
	this.embedCode=null;
	this.downloadStarted=false;	
	this.referrer=null;
	this.requestMethod="GET";
	this.uploadStream=null;
	this.cacheKey=null;
	this.captureSwf=false;
	var _title;
	var _site;
	var _pageUrl;
	var _filename;
	var _url;
	var _contentType;
	var _size=0;
	var _favicon=null;
	var _youtubeQualityUrls=[];
	var _host;
	var self=this;
	var _originalUrl;
	var _youtubeQuality;



	var embedIds={
		youtube:"embed_code",
		metacafe:"EmbedCode",
		livevideo:"ctl00_ContentPlaceHolder1_embedCode",
		rutube:"pcode_main",
		youku:"link3"
	};

	var youtubeItags={
		43:{name:"360p (WebM)", order:1, type:"video/webm"},
		44:{name:"480p (WebM)", order:2, type:"video/webm"},
		45:{name:"720p HD (WebM)", order:3, type:"video/webm"},
		5:{name:"240p (FLV)", order:4, type:"video/flv"},
		34:{name:"360p (FLV)", order:5, type:"video/flv"},
		18:{name:"360p (MP4)", order:6, type:"video/mp4"},
		35:{name:"480p (FLV)", order:7, type:"video/flv"},
		22:{name:"720p HD (MP4)", order:8, type:"video/mp4"},
		37:{name:"1080p HD (MP4)", order:9, type:"video/mp4"},
		38:{name:"4K (MP4)", order:10, type:"video/mp4"}

	};


	this.__defineGetter__("youtubeQualityUrls", function(){
		return _youtubeQualityUrls;	
	});

	this.__defineSetter__("title", function(value){
		var r=new RegExp("^YouTube[\ -]*","gi");
		_title=value.replace(r,"");
	});

	this.__defineGetter__("title", function(){
		return _title;
	});

	this.__defineGetter__("favicon", function(){
		return _favicon;
	});

	this.__defineGetter__("filename", function(){
		if(!self.title) return "video."+self.type;
		var s=self.title;
		s=s.substr(0,255);
		r=new RegExp('[\\\/\:\*\?\"\<\>\|\.]',"gi");
		s=s.replace(r,"");
		s=s.replace(/^\s*/, "").replace(/\s*$/, "");
		return s+"."+self.type;

	});

	this.__defineSetter__("size", function(value){
		_size=value;
		self.generateId();
	});

	this.__defineGetter__("size", function(){
		return _size;
	});

	this.__defineSetter__("contentType", function(value){
		if(value=="video/flv") value="video/x-flv";
		_contentType=value;
	});

	this.__defineGetter__("contentType", function(){
		return _contentType;
	});

	//detecting filetypes to capture only media links (using file extensions, contenttype header, and filesize)
	this.__defineGetter__("type",function(){
		 var t=null;
		 if(_url)
			self.extension=_url.substr(_url.lastIndexOf(".")+1,3);
		 if(self.contentType){
			 if(_size < 50000 ) return null;
			 // Filter out silverlight fragments,
			 if(_url.indexOf("QualityLevels")!=-1 && _url.indexOf("Fragments")!=-1){
					return null;
			 }

			 if(
					self.contentType.indexOf("video/x-flv")!=-1 ||
					self.contentType.indexOf("video/flv")!=-1 ||
					self.extension=="flv" ||
					(
						self.contentType.indexOf("application/octet-stream")!=-1 &&
						(
						  self.extension!="zip" &&
						  self.extension!="js" &&
						  self.extension!="exe" &&
						  self.extension!="jpg" &&
						  self.extension!="gif" &&
						  self.extension!="png"
						) &&
						_size > 200000
					)
			 ){
					t="flv";
	
			 }else if(self.contentType.indexOf("video/mp4")!=-1 || self.extension=="mp4"){
				t="mp4";
			 }else if(self.contentType.indexOf("video/webm")!=-1 || self.extension=="webm"){
			 	t="webm";
			 }else if(self.contentType.indexOf("audio/mpeg")!=-1 || self.extension=="mp3"){
				t="mp3";
			 }else if(self.contentType.indexOf("audio")!=-1 || self.contentType.indexOf("video")!=-1){
				if(self.extension.length>0 && self.extension.length<=5){
					t=self.extension;
				}else{
					t="video";
				}
			 }else if(self.captureSwf && (self.contentType.indexOf("application/x-shockwave-flash")!=-1 || self.extension=="swf")){
				t="swf";		 
			 }
			 
		 }
		 return t;
	});

	//cleaning the seek parameters from urls to prevent capturing the same video with different starting times
	this.__defineSetter__("url", function(value){
	 var  r=new RegExp("(fs|start|begin)=[0-9]+","g");
	  _url=value.replace(r,"");
	  r=new RegExp("(&ext=.flv){2,}","g");
	  _url=_url.replace(r,"&ext=.flv");
	  r=new RegExp("[&]*nvhnocache=1","g");
	  _url=_url.replace(r,"");
	  self.generateId(); // set url as mediaId
	
	});

	this.__defineSetter__("youtubeQuality", function(value){
		if(!value && _originalUrl){
		 	_url=_originalUrl;
		}
		if(_youtubeQualityUrls && _youtubeQualityUrls.length>0){
			_originalUrl=_url;
			var urlData=self.getVideoUrlByQuality(value);
			if(urlData){
				_contentType=urlData.type;
				_url=urlData.url;
			}

		}
		_youtubeQuality=value;	  
	});

	this.__defineGetter__("youtubeQuality", function(){
		return _youtubeQuality;
	});

	this.__defineGetter__("url", function(){
	  return _url;
	  
	});

	

	this.__defineSetter__("pageUrl", function(value){

		_pageUrl=value;
		_site=UriUtils.siteFromUrl(_pageUrl);
		_host=UriUtils.mainHostFromUrl(_pageUrl);

		//var ioService = Components.classes["@mozilla.org/network/io-service;1"].getService(Components.interfaces.nsIIOService);
		//var uri=ioService.newURI(aURL,"UTF-8");
		//_host=uri.host;
	});

	this.__defineGetter__("pageUrl", function(){
		return _pageUrl;
	});

	this.__defineGetter__("host", function(){
		return _host;
	});


	this.__defineGetter__("site", function(){
		return _site;
	});

	this.updateEmbed=function(doc){
		if(!self.grabCode(doc)){
			 var embeds = doc.getElementsByTagName("embed");
			 var maxArea = -1;
			 var width;
			 var height
			 var area;
			 var bigest;
			for (var i = 0; i < embeds.length; i++) {
				width=embeds[i].getAttribute("width");
				if(width.indexOf("%")!=-1) width=3000
				height=embeds[i].getAttribute("height");
				if(height.indexOf("%")!=-1) height=3000
				area=parseInt(width)*parseInt(height);
				if(area>maxArea){
					maxArea=area;
					bigest=embeds[i]
				}
			}
			if(bigest) {
				var code=""
				if(bigest.parentNode.nodeName=="OBJECT"){
					 code="<"+bigest.parentNode.nodeName+" "
					for (i in bigest.parentNode.attributes){
						if(bigest.parentNode.attributes[i].value===undefined)  continue
						code+=bigest.parentNode.attributes[i].name+"='"+bigest.parentNode.attributes[i].value+"' "
					}
					code+=">"+bigest.parentNode.innerHTML+"</"+bigest.parentNode.nodeName+">"
				}else{
					code="<"+bigest.nodeName+" "
					for (i in bigest.attributes){
						if(bigest.attributes[i].value===undefined) continue
						code+=bigest.attributes[i].name+"='"+bigest.attributes[i].value+"' "
					}
					code+="/>"
				}
				code=code.replace(/(embed|autoPlay|autoplay|autoStart|autostart|isAutoPlay)=1/g,"$1=0")
				code=code.replace(/(embed|autoPlay|autoplay|autoStart|autostart|isAutoPlay)=true/g,"$1=false")
				self.embedCode=code
			}

		}

	};

	this.grabCode=function(doc){
		var embedId=embedIds[self.site]
		if(typeof(embedId)!='undefined' && embedId!=null){
			 var e=doc.getElementById(embedId);
			 if(e){
				self.embedCode=e.value
				return true
			 }else{
				return false
			 }
		}else{
			return false
		}
	};

	this.setWindow=function(win){
		if(win){
			self.pageUrl=(win) ? win.document.location.href : self.referrer;
			self.title=(win) ? win.document.title : "No title";			
			 _favicon=getFaviconUrl(win.document);
			self.updateEmbed(win.document);
			if(self.site=="youtube") _youtubeQualityUrls=getOtherYoutubeQualities(win.document);
		}
		//win.addEventListener("close",com.netvideohunter.downloader.Main.onWindowClose,false)
		
	};

	this.generateId=function(){
		if(!_url || !self.size) return null;
		var videoid=_url;
		var r;
		if(_url.indexOf("youtube")!=-1 || _url.indexOf(".google")!=-1){
		  r=new RegExp("id=([^\&]+)","gi");
		  var m = r.exec(_url);
		  if (m != null) {
			videoid="__youtube__"+m[1];
		  }else{
			r=new RegExp("docid=([^\&]+)","gi");
			m = r.exec(_url);
			if (m != null) {
			  videoid="__googlevideo__"+m[1];
			}
		  }
		  
		}else if(_url.indexOf("dailymotion.com")!=-1){
			r=new RegExp("\/([^\.]+)\.mp4","gi");
			m = r.exec(_url);
			if (m != null) {
			  videoid="__dailymotion__"+m[1];
			}
		}else if(_url.indexOf("trilulilu.ro")!=-1){
			r=new RegExp("hash=([^&]+)&username=","gi");
			m = r.exec(_url);
			if (m != null) {
			  videoid="__trilulilu__"+m[1]+self.size;
			}
		}else if(_url.indexOf("grooveshark.")!=-1){
			videoid=_url+self.size;
		}else{
			videoid=videoid+"_"+self.size;
		}
		r=new RegExp("__gda__=[^&]+","gi");
		videoid=videoid.replace(r,"");		
		self.id=videoid;

	}

	this.getVideoUrlByQuality=function(quality){		
		for(var i = 0;i < _youtubeQualityUrls.length; i++){
			if(_youtubeQualityUrls[i].qualityName==quality){
				return _youtubeQualityUrls[i];
			}
		}           
	}

	function getFaviconUrl(doc){
		var linkTags=doc.getElementsByTagName("link")
		var iconUrl=null;
		for(var i=0;i<linkTags.length;i++){
			var item=linkTags[i];
			if(item.hasAttribute("rel") 
				&& item.getAttribute("rel").indexOf("icon")!=-1
				&& item.hasAttribute("href")){
				iconUrl=item.getAttribute("href");
			} 
		}
		if(iconUrl){
			var urlStart=iconUrl.indexOf("//");
			if(urlStart!==-1){
				if(urlStart>1){
					return iconUrl;
				}else{
					var path=iconUrl.substr(urlStart+2);
					return "http://"+path;
				}				
			}else{
				if(iconUrl.substr(0,1)!="/") iconUrl="/"+iconUrl;
				return "http://"+_host+iconUrl;
			}
		}else{
			return _host+"/favicon.ico";
		}
	};

	// !!!!!!!!!!!!

	function getOtherYoutubeQualities(doc){
		if(!doc.body) return;
		var urls=[];
		r=new RegExp("fmt_stream_map=([^\&]+)","gi");
		m = r.exec(doc.body.innerHTML);

		if (m != null) {
		  var urlData=decodeURIComponent(m[1]).split(","); 
		                     
		  for(var i = 0; i< urlData.length;i++){            
			var p=urlData[i].split("&");                        
			var foundItag=null;
			var foundUrl=null;
			for(var param in p){
			    var paramPair=p[param].split("=");
			    if(!paramPair || paramPair.length<=1) continue;
			    var key=paramPair.shift()
			    if(key=="url") foundUrl=decodeURIComponent(paramPair.join("="));
			    if(key=="itag") foundItag=paramPair.join("=");
			}
			if(foundItag===null || foundUrl===null ||!youtubeItags[foundItag]) continue;
			urls.push({url:foundUrl, qualityName:youtubeItags[foundItag].name, 
						qualityOrder:youtubeItags[foundItag].order,
						type:youtubeItags[foundItag].type});			
		  }
		  urls.sort(urlSort);
		}
		if(urls.length==0) return null;
		return urls;
	};

	function urlSort(a,b){
		return b.qualityOrder-a.qualityOrder;
	}


}