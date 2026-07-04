  import Char
  import Auxiliaries
  import Classification

-- Basic HTML markup

  htmlItalic str = "<I>"++str++"</I>"
  htmlBold str   = "<B>"++str++"</B>"

  htmlFline (nm,src)
   = "<FRAME Name="++show nm++" SRC="++show (htmlname src++".html")++" NORESIZE FRAMEBORDER=\"No\">"

  htmlPage title head body
   = chain "\n" (["<HTML>"]++[htmlHead title head]++[body|not(null body)]++["</HTML>"])

  htmlHead title head
   = chain "\n"
     ([ "<HEAD>"
      , "<META name=\"author\" content=\"S.Joosten\">"
      , "<META name=\"email\" content=\"stef.joosten@ou.nl\">" ] ++
      [ "<TITLE>"++title++"</TITLE>" | not (null title) ]            ++
      [ head | not (null head) ]                                     ++
      [ "</HEAD>"])

  htmlBody body
   = chain "\n" ["<BODY bgcolor=#FFFFBB link=#AA0000 alink=#AA0000 hlink=#AA0000>",body,"</BODY>"]

  htmlImage imgname
   = "<IMG src=\""++imgname++"\" border=\"0\" vspace=\"0\" hspace=\"0\" />"

  htmlDotted ls
   = chain "\n" (["<UL>"]++["<LI>"++l|l<-ls]++["</UL>"])

  htmlNumbered ls
   = chain "\n" (["<OL>"]++["<LI>"++l|l<-ls]++["</OL>"])

  htmlValNumbered ls
   = chain "\n" (["<OL>"]++["<LI VALUE=\""++show nr++"\">"++l|(nr,l)<-ls]++["</OL>"])

  htmlAnchor url clicktext params = "<A HREF="++url++concat [" "++p|p<-params]++">"++clicktext++"</A>"

  htmlHeadinglevel n str params = "<H"++show n++concat [" "++p|p<-params]++">"++str++"</H"++show n++">"

-- Obsolete as of April 15th, 2003
--   htmlFilename str = [if isAlphaNum c then c else if c `elem` "/\\" then c else '_'| c<-str]++".html"

  htmlname str = [if isAlphaNum c then c else if c `elem` "/\\" then c else '_'| c<-str]

-- htmlDropDown creates drop-down boxes in HTML.

  htmlDropDown title params options
   = "  <SELECT NAME="++show title++ concat [" "++p|p<-params]++">"++
     concat ["\n    <OPTION "++fn++"> "++n|(fn,n)<-options]++
     "\n  </SELECT>"

-- Example:
--  htmlDropDown "SwitchPattern" ["onchange=\"SwitchPattern(this.value)\""] [("value=\"Basics_Set.html\"">, "Set")]
-- yields
--   <SELECT NAME="SwitchPattern" onChange="SwitchPattern(this.value)">
--     <OPTION value="Basics_Set.html"> Set
--   </SELECT>

{-  htmlSTable [] = ""
  htmlSTable (xs:xxs)
   = chain "\n"
     ( ["<TABLE>"]++
        [ "<TR><td width=1 bgcolor=black rowspan="++show(1+length xxs)++"><img src=\"spacer.gif\"width=1></td>"++
          concat [ "<TD width="++show p++"% bgcolor=black><b><font color=#FFFFFF>"++x++
                   "</font></b></TD><td width=1 bgcolor=black rowspan="++show(1+length xxs)++
                   "><img src=\"spacer.gif\" width=1></td>"
                 | (p,x)<-zip [40,20,20,20,20] xs]++"</TR>"]++
       ["<TR>"++concat ["<TD valign=top>"++x++"</TD>"|x<-xs]++"</TR>"|xs<-if null xxs then [] else init xxs]++
       ["<TR>"++concat ["<TD valign=top>"++x++"</TD>"|x<-if null xxs then [] else last xxs]++"</TR>"]++
       ["<TR><TD colspan="++show(1+2*length xs)++" bgcolor=black><img src=\"spacer.gif\" height=1></TD></TR>"]++["</TABLE>"])-}

  htmlTable xxs = chain "\n" (["<TABLE>"]++["<TR>"++concat ["<TD valign=top>"++x++"</TD>"|x<-xs]++"</TR>"|xs<-xxs]++["</TABLE>"])

-- Risk: the following way of avoiding JavaScript reserved words is not entirely safe! It works well, though.

  avoidJSreservedwords str = "JS_"++str
