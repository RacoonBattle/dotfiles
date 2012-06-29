Components.utils.import("resource://nvhlib/com/netvideohunter/utils/EventDispatcher.js"); 
Components.utils.import("resource://nvhlib/com/netvideohunter/utils/Logger.js");

var EXPORTED_SYMBOLS = ["HashList"]; 

// === List ===

var HashList=function(hashBy,saveTo){
	EventDispatcher.call(this);
	this.saveToPreference=saveTo;
	this.saveEachChange=false;
	var self=this;
	var items=new Object();
	var hashBy=hashBy;
    var length=0;


	this.addItem=function(item){
		if(typeof(item[hashBy])=='undefined') throw new Error("item."+self.hashBy+" must not be null");		
        if(typeof(items[item[hashBy]])!='undefined'){
			this.removeItem(item[hashBy]);
		}
		items[item[hashBy]]=item;
        length++; 
		if(this.saveEachChange){
			this.save();
		}
		this.dispatchEvent({type:'itemAdded',item:item})
	}

	this.removeItem=function(id){
		if(typeof(items[id])=='undefined') return false;
        var item = items[id];
		delete items[id];
        length--;
		if(this.saveEachChange) this.save();
		this.dispatchEvent({type:'itemRemoved',item:item})
	}
    
    this.removeAll=function(){
        items=new Object();
		length=0;
		if(this.saveEachChange) this.save();
        this.dispatchEvent({type:'cleared'});
    }

	this.getItemById=function(id){
		if(typeof(items[id])=='undefined') return null;
		return items[id];
	}
    
    this.getLength=function(){
        return length;
    }

	this.getLastId=function(){
		var i=null;
		for(i in items){};
		return i;
	}
    
    this.forEach=function(func,target){
        for(var i in items){
			func.call(target,items[i],i);
		}
    }

	this.save=function(){	

		var jsonItems=JSON.stringify(items);	
		Components.classes["@mozilla.org/preferences-service;1"].getService(Components.interfaces.nsIPrefBranch).setCharPref(this.saveToPreference,jsonItems);
	}

	this.load=function(){
		this.dispatchEvent({type:'cleared'});		
		
		var jsonItems=Components.classes["@mozilla.org/preferences-service;1"].getService(Components.interfaces.nsIPrefBranch).getCharPref(this.saveToPreference);
		
		if(!jsonItems || jsonItems=='[]') return false;		
		items=JSON.parse(jsonItems);	
		for(i in items){
			this.dispatchEvent({type:'itemAdded',item:items[i]})
		}
	}
	
	if(this.saveToPreference!=null){
		this.load();
	}

	
}

// MediaList