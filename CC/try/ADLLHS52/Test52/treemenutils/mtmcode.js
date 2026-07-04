// Morten's JavaScript Tree Menu
// written by Morten Wang <morten@treemenu.com> (c) 1998-2000
// This is version 2.2.6++, dated 2000-03-30
// Minor revisions by Bas Joosten.

// The script is freely distributable
// It may be used (and modified) as you wish, but retain this message
// For more information about the menu visit its home page
// http://www.treemenu.com/

var MTMSubClosedXtra;
var MTMSubExpandXtra;
var MTMTrackXtra;
var MTMAhoverXtra="text-decoration=underline";

var MTMfolderopenimg="menu_folder_open.gif";
var MTMfolderclosedimg="menu_folder_closed.gif";
var MTMfolderopeningimg="menu_folder_opening.gif";
var MTMfolderclosingimg="menu_folder_closing.gif";

/* List on what I, Bas Joosten, did so far:
 * - I added some comment lines since I didn't understand all code at once. Some was allready there
 * - MTMSubClosedXtra, MTMSubExpandXtra, MTMTrackXtra, MTMAhoverXtra for in stylesheets
 * - removing nonbraking-space (for the underline feature in MTM...Xtra)
 * - Extracting MTMfolderopenimg and MTMfolderclosedimg
 * - added a bit better layout for the JS-generated code
 * - Adding MTMfolderopeningimg and MTMfolderclosingimg variables
 *
 * List of things that could be improved:
 * - add preload 
 * - add possibility for use of progressbar
 */

/******************************************************************************
* Define the MenuItem object.                                                 *
******************************************************************************/
function MTMenuItem(text, url, target, icon) {
  this.text = text;  //changing 'Null' value into empty lines, assigning variables
  this.url = url ? url : "";
  this.target =  target ? target : "";
  this.icon = icon ? icon : "";

  this.number = MTMSubNumber++;  //Auto increment

  this.submenu     = null;    //No submenu's so far
  this.expanded    = false;   //can't be expanded since all shouldn't be collapsed by default
  this.firsttime   = false;
  this.MTMakeSubmenu = MTMakeSubmenu;  //assign event 'MTMakeSubmenu'
}

function MTMakeSubmenu(menu) {
  this.submenu = menu;
}

/******************************************************************************
* Define the Menu object.                                                     *
******************************************************************************/

function MTMenu() {
  this.items   = new Array();  //All items in an array with var length
  this.MTMAddItem = MTMAddItem;  //Assign event
}

function MTMAddItem(item) {
  this.items[this.items.length] = item;   //auto-increment
}

/******************************************************************************
* Define the icon list, addIcon function and MTMIcon item.                    *
******************************************************************************/

function IconList() {
  this.items = new Array();    //List with icons
  this.addIcon = addIcon;	//event
}

function addIcon(item) {
  this.items[this.items.length] = item;   
           //auto increment, add an icon URL, no preload used i guess
}

function MTMIcon(iconfile, match, type) {
  this.file = iconfile;  //URL
  this.match = match;	 //
  this.type = type;	 //
}

/******************************************************************************
* Global variables.  Not to be altered unless you know what you're doing.     *
* User-configurable options are at the end of this document.                  *
******************************************************************************/

var MTMLoaded = false;  //Whether the menu is loaded
var MTMLevel; //no idea
var MTMBar = new Array(); // no idea
var MTMIndices = new Array(); // no idea
var MTMBrowser = null;	//Browsers used
var MTMNN3 = false; //Browsers used: Netscape 3
var MTMNN4 = false; //Netscape 4
var MTMIE4 = false; //IExplore 4&5
var MTMUseStyle = true; //Use of stylesheets, works for false and true. font will change though

//checking for browser
if(navigator.appName == "Netscape" && navigator.userAgent.indexOf("WebTV") == -1) {
  if(parseInt(navigator.appVersion) == 3 && (navigator.userAgent.indexOf("Opera") == -1)) {
    MTMBrowser = true;
    MTMNN3 = true;
    MTMUseStyle = false;
  } else if(parseInt(navigator.appVersion) >= 4) {
    MTMBrowser = true;
    MTMNN4 = true;
  }
} else if (navigator.appName == "Microsoft Internet Explorer" && parseInt(navigator.appVersion) >= 4) {
  MTMBrowser = true;
  MTMIE4 = true;
}
//checking complete

var MTMClickedItem = false; //item currently in main page
var MTMExpansion = false;  // no idea

var MTMSubNumber = 1;
var MTMTrackedItem = false;
var MTMTrack = false;

var MTMPreHREF = "";  //url currently in main page
if(MTMIE4 || MTMNN3) {
  MTMPreHREF += document.location.href.substring(0, document.location.href.lastIndexOf("/") +1);
}

var MTMFirstRun = true;
var MTMCurrentTime = 0; // for checking timeout.
var MTMUpdating = false;
var MTMWinSize, MTMyval;
var MTMOutputString = "";

/******************************************************************************
* Code that picks up frame names of frames in the parent frameset.            *
******************************************************************************/

if(MTMBrowser) {
  var MTMFrameNames = new Array();
  for(i = 0; i < parent.frames.length; i++)
    MTMFrameNames[i] = parent.frames[i].name;
}

/******************************************************************************
* Dummy function for sub-menus without URLs                                   *
* Thanks to Michel Plungjan for the advice. :)                                *
******************************************************************************/

function myVoid() { ; }

/******************************************************************************
* Functions to draw the menu.                                                 *
******************************************************************************/

function MTMSubAction(SubItem, ReturnValue) {
// Hier moet wat komen voor het .firsttime=false voor vanalles
  SubItem.expanded = (SubItem.expanded) ? false : true;

  SubItem.firsttime = true; //toegevoegd
  if(SubItem.expanded) {
    MTMExpansion = true;
  }

  MTMClickedItem = SubItem.number;

  if(MTMTrackedItem && MTMTrackedItem != SubItem.number) {
    MTMTrackedItem = false;
  }

  if(!ReturnValue) {
    setTimeout("MTMDisplayMenu()", 10);
  }

  return ReturnValue;
}

function MTMStartMenu() {
  MTMLoaded = true;
  if(MTMFirstRun) {
    MTMCurrentTime++;
    if(MTMCurrentTime == MTMTimeOut) { // call MTMDisplayMenu
      setTimeout("MTMDisplayMenu()",10);
    } else {
      setTimeout("MTMStartMenu()",100);
    }
  } 
}

function MTMDisplayMenu() {
  if(MTMBrowser && !MTMUpdating) {
    MTMUpdating = true;
    MTMFirstRun = false;

    if(MTMTrack) { MTMTrackedItem = MTMTrackExpand(menu); }

    if(MTMExpansion && MTMSubsAutoClose) { MTMCloseSubs(menu); }

    MTMLevel = 0;
    MTMDoc = parent.frames[MTMenuFrame].document
    MTMDoc.open("text/html", "replace");
    MTMOutputString = '<html><head>\n';
    if(MTMLinkedSS) {
      MTMOutputString += '<link rel="stylesheet" type="text/css" href="' + MTMPreHREF + MTMSSHREF + '">\n';
    } else if(MTMUseStyle) {
      MTMOutputString += '<style type="text/css">\n   body {color:' + MTMTextColor + ';background:';
      MTMOutputString += (MTMBackground == "") ? MTMBGColor : MTMakeBackImage(MTMBackground);
      MTMOutputString += ';} \n   #root {color:' + MTMRootColor + ';background:' + ((MTMBackground == "") ? MTMBGColor : 'transparent') + ';font-family:' + MTMRootFont + ';font-size:' + MTMRootCSSize + ';} ';
      MTMOutputString += '\n   a {font-family:' + MTMenuFont + ';font-size:' + MTMenuCSSize + ';text-decoration:none;color:' + MTMLinkColor + ';background:' + MTMakeBackground() + ';} ';
      MTMOutputString += MTMakeA('pseudo', 'hover', MTMAhoverColor, MTMAhoverXtra);
      MTMOutputString += MTMakeA('class', 'tracked', MTMTrackColor, MTMTrackXtra);
      MTMOutputString += MTMakeA('class', 'subexpanded', MTMSubExpandColor, MTMSubExpandXtra);
      MTMOutputString += MTMakeA('class', 'subclosed', MTMSubClosedColor, MTMSubClosedXtra) + '\n</style>';
    }

    MTMOutputString += '</head>\n<body ';
    if(MTMBackground != "") {
      MTMOutputString += 'background="' + MTMPreHREF + MTMenuImageDirectory + MTMBackground + '" ';
    }
    MTMOutputString += 'bgcolor="' + MTMBGColor + '" text="' + MTMTextColor + '" link="' + MTMLinkColor + '" vlink="' + MTMLinkColor + '" alink="' + MTMLinkColor + '">';
    MTMOutputString += '\n  <table border="0" cellpadding="0" cellspacing="0" width="' + MTMTableWidth + '">';
    MTMOutputString += '\n    <tr valign="top"><td nowrap><img src="' + MTMPreHREF + MTMenuImageDirectory + MTMRootIcon + '" align="left" border="0" vspace="0" hspace="0">';
    if(MTMUseStyle) {
      MTMOutputString += '\n    <span id="root">&nbsp;\n     ' + MTMenuText + '\n    </span>';
    } else {
      MTMOutputString += '\n    <font size="' + MTMRootFontSize + '" face="' + MTMRootFont + '" color="' + MTMRootColor + '">\n' + MTMenuText + '\n    </font>';
    }
    MTMDoc.writeln(MTMOutputString + '\n    </td></tr>');

    MTMListItems(menu);

    MTMDoc.writeln('\n  </table>\n</body></html>');
    MTMDoc.close();

    if((MTMClickedItem || MTMTrackedItem) && (MTMNN4 || MTMIE4) && !MTMFirstRun) {
      MTMItemName = "sub" + (MTMClickedItem ? MTMClickedItem : MTMTrackedItem);
      if(document.layers && parent.frames[MTMenuFrame].scrollbars) {    
        MTMyval = parent.frames[MTMenuFrame].document.anchors[MTMItemName].y;
        MTMWinSize = parent.frames[MTMenuFrame].innerHeight;
      } else {
        MTMyval = MTMGetPos(parent.frames[MTMenuFrame].document.all[MTMItemName]);
        MTMWinSize = parent.frames[MTMenuFrame].document.body.offsetHeight;
      }
      if(MTMyval > (MTMWinSize - 60)) {
        parent.frames[MTMenuFrame].scrollBy(0, parseInt(MTMyval - (MTMWinSize * 1/3)));
      }
    }

    MTMClickedItem = false;
    MTMExpansion = false;
    MTMTrack = false;
  }
MTMUpdating = false;
}

function MTMListItems(menu) {
  var i, isLast;
  for (i = 0; i < menu.items.length; i++) {
    MTMIndices[MTMLevel] = i;
    isLast = (i == menu.items.length -1);
    MTMDisplayItem(menu.items[i], isLast);

    if (menu.items[i].submenu && menu.items[i].expanded) {
      MTMBar[MTMLevel] = (isLast) ? false : true;
      MTMLevel++;
      MTMListItems(menu.items[i].submenu);
      MTMLevel--;
    } else {
      MTMBar[MTMLevel] = false;
    } 
  }
}

function MTMDisplayItem(item, last) {
  var i, img, more;
  var SpacesString;
  if(item.submenu) {
    var MTMouseOverText;

    var MTMClickCmd;
    var MTMDblClickCmd = false;
    var MTMfrm = "parent.frames['code']";
    var MTMref = '.menu.items[' + MTMIndices[0] + ']';

    if(MTMLevel > 0) {
      for(i = 1; i <= MTMLevel; i++) {
        MTMref += ".submenu.items[" + MTMIndices[i] + "]";
      }
    }

    if(!MTMEmulateWE && !item.expanded && (item.url != "")) {
      MTMClickCmd = "return " + MTMfrm + ".MTMSubAction(" + MTMfrm + MTMref + ",true);";
    } else {
      MTMClickCmd = "return " + MTMfrm + ".MTMSubAction(" + MTMfrm + MTMref + ",false);";
    }

    if(item.url == "") {
      MTMouseOverText = (item.text.indexOf("'") != -1) ? MTMEscapeQuotes(item.text) : item.text;
    } else {
      MTMouseOverText = "Expand/Collapse";
    }
  }

  MTMOutputString = '<tr valign="top"><td nowrap>';
  MTMSpacesString = '      ';
  if(MTMLevel > 0) {
    for (i = 0; i < MTMLevel; i++) {
	  MTMSpacesString += '    ';
    }
  }
  if(MTMLevel > 0) {
    for (i = 0; i < MTMLevel; i++) {
      MTMOutputString += '\n  ' + MTMSpacesString + ((MTMBar[i]) ? MTMakeImage("menu_bar.gif") : MTMakeImage("menu_pixel.gif"));
    }
  }

  more = false;
  if(item.submenu) {
    if(MTMSubsGetPlus || MTMEmulateWE) {
      more = true;
    } else {
      for (i = 0; i < item.submenu.items.length; i++) {
        if (item.submenu.items[i].submenu) {
          more = true;
        }
      }
    }
  }
  if(!more) {
    img = (last) ? "menu_corner.gif" : "menu_tee.gif";
  } else {
    if(item.expanded) {
      img = (last) ? "menu_corner_minus.gif" : "menu_tee_minus.gif";
    } else {
      img = (last) ? "menu_corner_plus.gif" : "menu_tee_plus.gif";
    }
    if(item.url == "" || item.expanded || MTMEmulateWE) {
      MTMOutputString += MTMakeVoid(item, MTMClickCmd, MTMouseOverText);
    } else {
      MTMOutputString += MTMakeLink(item, true)  + ' onclick="' + MTMClickCmd + '">';
    }
  }
  MTMOutputString += MTMakeImage(img);

  if(item.submenu) {
    if(MTMEmulateWE && item.url != "") {
      MTMOutputString += '</a>\n  ' + MTMSpacesString + MTMakeLink(item, false) + '>';
    }
    if (item.firsttime){
	item.firsttime=false;
	img = (item.expanded) ? MTMfolderopeningimg : MTMfolderclosingimg;
	}else{
    img = (item.expanded) ? MTMfolderopenimg : MTMfolderclosedimg;
    }
    if(!more) {
      if(item.url == "" || item.expanded) {
        MTMOutputString += MTMakeVoid(item, MTMClickCmd, MTMouseOverText);
      } else {
        MTMOutputString += MTMakeLink(item, true) + ' onclick="' + MTMClickCmd + '">';
      }
    }
    MTMOutputString += MTMakeImage(img);

  } else {
    MTMOutputString += MTMakeLink(item, true) + '>';
    img = (item.icon != "") ? item.icon : MTMFetchIcon(item.url);
    MTMOutputString += MTMakeImage(img);
  }

  if(item.submenu && (item.url != "") && (item.expanded && !MTMEmulateWE)) {
    MTMOutputString += '</a>\n  ' + MTMSpacesString + + MTMakeLink(item, false) + '>';
  }else{
    MTMOutputString += '';
  }

  if(MTMNN3 && !MTMLinkedSS) {
    var stringColor;
    if(item.submenu && (item.url == "") && (item.number == MTMClickedItem)) {
      stringColor = (item.expanded) ? MTMSubExpandColor : MTMSubClosedColor;
    } else if(MTMTrackedItem && MTMTrackedItem == item.number) {
      stringColor = MTMTrackColor;
    } else {
      stringColor = MTMLinkColor;
    }
    MTMOutputString += '<font color="' + stringColor + '" size="' + MTMenuFontSize + '" face="' + MTMenuFont + '">';
  }

  MTMOutputString += '\n  '+MTMSpacesString + item.text + ((MTMNN3 && !MTMLinkedSS) ? '</font>' : '') + '</a>' ;
  MTMDoc.writeln(MTMSpacesString + MTMOutputString + '\n'+MTMSpacesString+'</td></tr>');
}

function MTMEscapeQuotes(myString) {
  var newString = "";
  var cur_pos = myString.indexOf("'");
  var prev_pos = 0;
  while (cur_pos != -1) {
    if(cur_pos == 0) {
      newString += "\\";
    } else if(myString.charAt(cur_pos-1) != "\\") {
      newString += myString.substring(prev_pos, cur_pos) + "\\";
    } else if(myString.charAt(cur_pos-1) == "\\") {
      newString += myString.substring(prev_pos, cur_pos);
    }
    prev_pos = cur_pos++;
    cur_pos = myString.indexOf("'", cur_pos);
  }
  return(newString + myString.substring(prev_pos, myString.length));
}

function MTMTrackExpand(thisMenu) {
  var i, targetPath;
  var foundNumber = false;
  for(i = 0; i < thisMenu.items.length; i++) {
    if(thisMenu.items[i].url != "" && MTMTrackTarget(thisMenu.items[i].target)) {
      targetPath = parent.frames[thisMenu.items[i].target].location.pathname;
      if(targetPath.lastIndexOf(thisMenu.items[i].url) != -1 && (targetPath.lastIndexOf(thisMenu.items[i].url) + thisMenu.items[i].url.length) == targetPath.length) {
        return(thisMenu.items[i].number);
      }
    }
    if(thisMenu.items[i].submenu) {
      foundNumber = MTMTrackExpand(thisMenu.items[i].submenu);
      if(foundNumber) {
        if(!thisMenu.items[i].expanded) {
		  thisMenu.items[i].firsttime = thisMenu.items[i].expanded ? false : true;
          thisMenu.items[i].expanded = true;
          if(!MTMClickedItem) { MTMClickedItem = thisMenu.items[i].number; }
          MTMExpansion = true;
        }
        return(foundNumber);
      }
    }
  }
return(foundNumber);
}

function MTMCloseSubs(thisMenu) {
  var i, j;
  var foundMatch = false;
  for(i = 0; i < thisMenu.items.length; i++) {
    if(thisMenu.items[i].submenu && thisMenu.items[i].expanded) {
      if(thisMenu.items[i].number == MTMClickedItem) {
        foundMatch = true;
        for(j = 0; j < thisMenu.items[i].submenu.items.length; j++) {
          if(thisMenu.items[i].submenu.items[j].expanded) {
			thisMenu.items[i].submenu.items[j].firsttime =  thisMenu.items[i].submenu.items[j].expanded ? true : false;
            thisMenu.items[i].submenu.items[j].expanded = false;
          }
        }
      } else {
        if(foundMatch) {
		  thisMenu.items[i].firsttime = thisMenu.items[i].expanded ? true : false;
          thisMenu.items[i].expanded = false; 
        } else {
          foundMatch = MTMCloseSubs(thisMenu.items[i].submenu);
          if(!foundMatch) {
		    thisMenu.items[i].firsttime = thisMenu.items[i].expanded ? true : false;
            thisMenu.items[i].expanded = false;
          }
        }
      }
    }
  }
return(foundMatch);
}

function MTMFetchIcon(testString) {
  var i;
  for(i = 0; i < MTMIconList.items.length; i++) {
    if((MTMIconList.items[i].type == 'any') && (testString.indexOf(MTMIconList.items[i].match) != -1)) {
      return(MTMIconList.items[i].file);
    } else if((MTMIconList.items[i].type == 'pre') && (testString.indexOf(MTMIconList.items[i].match) == 0)) {
      return(MTMIconList.items[i].file);
    } else if((MTMIconList.items[i].type == 'post') && (testString.indexOf(MTMIconList.items[i].match) != -1)) {
      if((testString.lastIndexOf(MTMIconList.items[i].match) + MTMIconList.items[i].match.length) == testString.length) {
        return(MTMIconList.items[i].file);
      }
    }
  }
return("menu_link_default.gif");
}

function MTMGetPos(myObj) {
  return(myObj.offsetTop + ((myObj.offsetParent) ? MTMGetPos(myObj.offsetParent) : 0));
}

function MTMCheckURL(myURL) {
  var tempString = "";
  if((myURL.indexOf("http://") == 0) || (myURL.indexOf("https://") == 0) || (myURL.indexOf("mailto:") == 0) || (myURL.indexOf("ftp://") == 0) || (myURL.indexOf("telnet:") == 0) || (myURL.indexOf("news:") == 0) || (myURL.indexOf("gopher:") == 0) || (myURL.indexOf("nntp:") == 0) || (myURL.indexOf("javascript:") == 0)) {
    tempString += myURL;
  } else {
    tempString += MTMPreHREF + myURL;
  }
return(tempString);
}

function MTMakeVoid(thisItem, thisCmd, thisText) {
  var tempString = "";
  tempString +=  '<a name="sub' + thisItem.number + '" href="javascript:parent.frames[\'code\'].myVoid();" onclick="' + thisCmd + '" onmouseover="window.status=\'' + thisText + '\';return true;" onmouseout="window.status=\'' + window.defaultStatus + '\';return true;"';
  if(thisItem.number == MTMClickedItem) {
    var tempClass;
    tempClass = thisItem.expanded ? "subexpanded" : "subclosed";
    tempString += ' class="' + tempClass + '"';
  }
  return(tempString + '>');
}

function MTMakeLink(thisItem, addName) {
  var tempString = '<a';

  if(MTMTrackedItem && MTMTrackedItem == thisItem.number) {
    tempString += ' class="tracked"'
  }
  if(addName) {
    tempString += ' name="sub' + thisItem.number + '"';
  }
  tempString += ' href="' + MTMCheckURL(thisItem.url) + '"';
  if(thisItem.target != "") {
    tempString += ' target="' + thisItem.target + '"';
  }
return tempString;
}

function MTMakeImage(thisImage) {
  return('<img src="' + MTMPreHREF + MTMenuImageDirectory + thisImage + '" align="left" border="0" vspace="0" hspace="0" width="18" height="18">');
}

function MTMakeBackImage(thisImage) {
  var tempString = 'transparent url("' + ((MTMPreHREF == "") ? "" : MTMPreHREF);
  tempString += MTMenuImageDirectory + thisImage + '")'
  return(tempString);
}

function MTMakeA(thisType, thisText, thisColor, xtra) {
  var tempString = "";
  xtra = xtra ? ";"+xtra : "";
  tempString += 'a' + ((thisType == "pseudo") ? ':' : '.');
  return(tempString + thisText + '{color:' + thisColor + ';background:' + MTMakeBackground() + xtra + ';}');
}

function MTMakeBackground() {
  return((MTMBackground == "") ? MTMBGColor : 'transparent');
}

function MTMTrackTarget(thisTarget) {
  if(thisTarget.charAt(0) == "_") {
    return false;
  } else {
    for(i = 0; i < MTMFrameNames.length; i++) {
      if(thisTarget == MTMFrameNames[i]) {
        return true;
      }
    }
  }
  return false;
}
