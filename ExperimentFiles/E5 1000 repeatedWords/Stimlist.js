//list of 160 words that participants will only see in the encoding phase 

StimList=[["anger","mad","fear","hate","rage","temper","fury","ire","wrath","happy","fight","hatred","mean","calm","emotion","enrage"],
["girl","boy","dolls","female","young","dress","pretty","hair","niece","dance","beautiful","cute","date","aunt","daughter","sister"],
["black","white","dark","cat","charred","night","funeral","color","grief","blue","death","ink","bottom","coal","brown","gray"],
["high","low","clouds","up","tall","tower","jump","above","building","noon","cliff","sky","over","airplane","dive","elevate"],
["bread","butter","food","eat","sandwich","rye","jam","milk","flour","jelly","dough","crust","slice","wine","loaf","toast"],
["king","queen","England","crown","prince","George","dictator","palace","throne","chess","rule","subjects","monarch","royal","leader","reign"],
["slow","fast","lethargic","stop","listless","snail","cautious","delay","traffic","turtle","hesitant","speed","quick","sluggish","wait","molasses"],
["man","woman","husband","uncle","lady","mouse","male","father","strong","friend","beard","person","handsome","muscle","suit","old"],
["soft","hard","light","pillow","plush","loud","cotton","fur","touch","fluffy","feather","furry","downy","kitten","skin","tender"],
["cold","hot","snow","warm","winter","ice","wet","frigid","chilly","heat","weather","freeze","air","shiver","Arctic","frost"],
["spider","web","insect","bug","fright","fly","arachnid","crawl","tarantula","poison","bite","creepy","animal","ugly","feelers","small"],
["doctor","nurse","sick","lawyer","medicine","health","hospital","dentist","physician","ill","patient","office","stethoscope","surgeon","clinic","cure"],
["music","note","sound","piano","sing","radio","band","melody","horn","concert","instrument","symphony","jazz","orchestra","art","rhythm"],
["foot","shoe","hand","toe","kick","sandals","soccer","yard","walk","ankle","arm","boot","inch","sock","smell","mouth"],
["thief","steal","robber","crook","burglar","money","cop","bad","rob","jail","gun","villain","crime","bank","bandit","criminal"],
["fruit","apple","vegetable","orange","kiwi","citrus","ripe","pear","banana","berry","cherry","basket","juice","salad","bowl","cocktail"],
["river","water","stream","lake","mississippi","boat","tide","swim","flow","run","barge","creek","brook","fish","bridge","winding"],
["window","door","glass","pane","shade","ledge","sill","house","open","curtain","frame","view","breeze","sash","screen","shutter"],
["rough","smooth","bumpy","road","tough","sandpaper","jagged","ready","coarse","uneven","riders","rugged","sand","boards","ground","gravel"],
["chair","table","sit","legs","seat","couch","desk","recliner","sofa","wood","cushion","swivel","stool","sitting","rocking","bench"],
["mountain","hill","valley","climb","summit","top","molehill","peak","plain","glacier","goat","bike","climber","range","steep","ski"],
["needle","thread","pin","eye","sewing","sharp","point","prick","thimble","haystack","thorn","hurt","injection","syringe","cloth","knitting"],
["sweet","sour","candy","sugar","bitter","good","taste","tooth","nice","honey","soda","chocolate","heart","cake","tart","pie"],
["sleep","bed","rest","awake","tired","dream","wake","snooze","blanket","doze","slumber","snore","nap","peace","yawn","drowsy"]];


StimList=randomizearray(StimList);
//Make encoding Words
EncodingWords=[];
acount=-1
for(i=10;i<=14;i++){
	for(j=0;j<=15;j++){
	acount++
	EncodingWords[acount]=StimList[i][j]; 
	}
}
EncodingWords<-randomizearray(EncodingWords)


//make bowl A
//add related words
bowlA=[[]];
acount=-1
for(i=0;i<=4;i++){
	for(j=1;j<=14;j++){
	acount++
	bowlA[acount]=[StimList[i][j],StimList[i][0],"Related","Pre-Lure",j] 
	}
}
//add category words
for(i=5;i<=9;i++){
	acount++
	bowlA[acount]=[StimList[i][0],StimList[i][0],"Category","Pre-Lure",0] //this is the control group 
}

for(i=15;i<=19;i++){
	acount++
	bowlA[acount]=[StimList[i][0],StimList[i][0],"UPCategory","Pre-Lure",0] //this is the group we hope to create false positives with 
}


//make bowl B
//add related words
bowlB=[[]];
bcount=-1
for(i=5;i<=9;i++){
	for(j=1;j<=14;j++){
	bcount++
	bowlB[bcount]=[StimList[i][j],StimList[i][0],"Related","Post-Lure",j]
	}
}
//add category words
for(i=0;i<=4;i++){
	bcount++
	bowlB[bcount]=[StimList[i][0],StimList[i][0],"Category","Post-Lure",0] //this is the group we hope to create false positives with 
}
//add "unprimed" category words
for(i=15;i<=19;i++){
	bcount++
	bowlB[bcount]=[StimList[i][0],StimList[i][0],"UPCategory","Post-Lure",0] //this is the group we hope to create false positives with 
}


bowlA=randomizearray(bowlA);
bowlB=randomizearray(bowlB);

List=bowlA.concat(bowlB.concat());

//add in the new nonrelated words 


function randomizearray(t){
    var tt= t;
    var n = 0;
    var a = "";
    var b = "";
    var i = 0;
    for (i=0; i <= t.length-1; i++){
        n = Math.floor(Math.random()*t.length);
        a = tt[i];
        b = tt[n];
        tt[i] = b;
        tt[n] = a;    
    }
    return tt;
}

//The mechanism I used to solve the problem I been having involved putting each list in single brackets 
//instead of double brackets, and have the for loop go up to 160. //However, the problem with this design involves the fact 
//that what we record in our data will be quite limited. In particular, I was unable to find a way to  
//assure that the status of each word that was presented was recorded (e.g. trial 3 presented a word that was a Pre Category Word as in
//a category word that hasn't been primed by the related words. 
//Hence, lists of 5 words will prevent me from encountering the problems noted above but it is not the ideal solution either.  
//I will continue to think of a better alternative.   



var BrowserDetect = {
	init: function () {
		this.browser = this.searchString(this.dataBrowser) || "An unknown browser";
		this.version = this.searchVersion(navigator.userAgent)
			|| this.searchVersion(navigator.appVersion)
			|| "an unknown version";
		this.OS = this.searchString(this.dataOS) || "an unknown OS";
	},
	searchString: function (data) {
		for (var i=0;i<data.length;i++)	{
			var dataString = data[i].string;
			var dataProp = data[i].prop;
			this.versionSearchString = data[i].versionSearch || data[i].identity;
			if (dataString) {
				if (dataString.indexOf(data[i].subString) != -1)
					return data[i].identity;
			}
			else if (dataProp)
				return data[i].identity;
		}
	},
	searchVersion: function (dataString) {
		var index = dataString.indexOf(this.versionSearchString);
		if (index == -1) return;
		return parseFloat(dataString.substring(index+this.versionSearchString.length+1));
	},
	dataBrowser: [
		{
			string: navigator.userAgent,
			subString: "Chrome",
			identity: "Chrome"
		},
		{ 	string: navigator.userAgent,
			subString: "OmniWeb",
			versionSearch: "OmniWeb/",
			identity: "OmniWeb"
		},
		{
			string: navigator.vendor,
			subString: "Apple",
			identity: "Safari",
			versionSearch: "Version"
		},
		{
			prop: window.opera,
			identity: "Opera",
			versionSearch: "Version"
		},
		{
			string: navigator.vendor,
			subString: "iCab",
			identity: "iCab"
		},
		{
			string: navigator.vendor,
			subString: "KDE",
			identity: "Konqueror"
		},
		{
			string: navigator.userAgent,
			subString: "Firefox",
			identity: "Firefox"
		},
		{
			string: navigator.vendor,
			subString: "Camino",
			identity: "Camino"
		},
		{		// for newer Netscapes (6+)
			string: navigator.userAgent,
			subString: "Netscape",
			identity: "Netscape"
		},
		{
			string: navigator.userAgent,
			subString: "MSIE",
			identity: "Explorer",
			versionSearch: "MSIE"
		},
		{
			string: navigator.userAgent,
			subString: "Gecko",
			identity: "Mozilla",
			versionSearch: "rv"
		},
		{ 		// for older Netscapes (4-)
			string: navigator.userAgent,
			subString: "Mozilla",
			identity: "Netscape",
			versionSearch: "Mozilla"
		}
	],
	dataOS : [
		{
			string: navigator.platform,
			subString: "Win",
			identity: "Windows"
		},
		{
			string: navigator.platform,
			subString: "Mac",
			identity: "Mac"
		},
		{
			   string: navigator.userAgent,
			   subString: "iPhone",
			   identity: "iPhone/iPod"
	    },
		{
			string: navigator.platform,
			subString: "Linux",
			identity: "Linux"
		}
	]

};
BrowserDetect.init();

var BrowserInfo = [];
BrowserInfo[0] = BrowserDetect.browser;
BrowserInfo[1] = BrowserDetect.version;
BrowserInfo[2] = BrowserDetect.OS;

