> module Main where
>  import System
>--  import Char
>  import UU_Scanner
>  import UU_Parsing
>  import Auxiliaries
>--  import Classification
>  import Typology
>  import CC_aux
>--  import Restrictions
>--  import Html
>  import LATEXgen
>--  import Generator
>--  import RelHtml
>--  import ViolHtml
>--  import ConceptList
>--  import RelationList
>  import AGtry
>  import CC

>  testing = False
>--  textgraphs = False
>--  latexOpt sws = "-l" `elem` sws
>--  violHtml = []
>--  splitStr f (x:xs) | f x  = (x:yes, no)
>--                    | True = (yes, x:no)
>--                    where (yes,no) = splitStr f xs
>  splitStr f [] = ([],[])

>  main
>   = do { a <- getArgs
>        ; let (switches,args) = splitStr ((=="-").take 1) a
>        ; putStr ("Arguments: "++chain ", " args++"\nSwitches: "++chain ", " switches)
>        ; if length args==0 then putStr ("Please provide a filename (.adl) and a context name") else
>     do { let fn = args!!0; contextname = args!!1
>              (fnPrefix,fnSuffix) = break ('.' ==) fn
>              fnFull = if null fnSuffix then (fn ++ ".adl") else fn
>        ; inp<-readFile fnFull
>        ; putStr ("\n"++fnFull++" is read.")
>        ; slRes <- parseIO pArchitecture (scan keywordstxt keywordsops specialchars opchars fnFull initPos inp)
>        ; putStr ("\n"++fnFull++" has been parsed.")
>        ; let (contexts,errs) = sem_Architecture slRes
>        ; if null errs 
>          then (putStr ("\nNo type errors or cyclic specializations were found.\n")>>
>                if length args==1 && length contexts==1
>                then build contexts switches (name (head contexts)) else
>                if length args==1 && length contexts>1
>                then putStr ("\nPlease specify the name of a context."++
>                             "\nAvailable contexts: "++commaAnd (map name contexts)++".\n") else
>                if length args==1 && contextname `elem` map name contexts
>                then build contexts switches contextname
>                else putStr ("\nContext "++contextname++" not defined."++
>                             "\nPlease specify the name of an available context."++
>                             "\nAvailable contexts: "++commaAnd (map name contexts)++"."++
>                             "\n(Note: context names are case sensitive).\n")
>               )
>          else putStr ("\nThe type analysis of "++fnFull++" yields errors.")>>
>               putStr (concat ["\n!Error of type "++err| err<-errs])>>
>               putStr ("\nNothing generated, please correct mistake(s) first.\n")
>        }}
>      where build contexts switches contextname
>             = sequence_ 
>--                ([ anal contexts contextname| null switches || "-h" `elem` switches]++
>                 ([ zed contexts contextname| "-Z" `elem` switches]++
>                 [ glossary contexts contextname | "-g" `elem` switches]
>                )

>  glossary contexts contextname
>   = putStr ("\nGenerating Glossary for "++name context++" in the current directory.") >>
>     lglossary context
>     where
>      context  = head ([c| c<-contexts, name c==contextname]++
>                 [Ctx (contextname++" is not defined") [] empty nogE [] [] [] []])
>      Typ pths = typology (isa context)

>  zed contexts contextname
>   = putStr ("\nGenerating Zed specification for "++name context++" in the current directory.") >>
>     lprint context
>     where
>      context  = head ([c| c<-contexts, name c==contextname]++
>                 [Ctx (contextname++" is not defined") [] empty nogE [] [] [] []])
>      Typ pths = typology (isa context)

>--  anal contexts contextname
>--   = putStr "\n" >>(putStr.chain "\n".map show) (makeTrees (Typ (map reverse pths)))         >>
>--     putStr ("\nGenerating Atlas for "++name context++" in the current directory."++
>--             "\n  (current directory must already contain the directory \"treemenutils\""++
>--             " with its complete contents)")                                                 >>
>-- -- writing the main html page, containing the required frames
>--     writeFile "index.html" (indexcode (htmlNm context++".html"))                            >>
>--     putStr ("\nindex.html written")                                                         >>
>-- -- writing XML and DOT information for graphics purposes.
>--     writeFile "contexts.xml" (xmlContexts contexts contextname)                             >>
>--     putStr "\ncontexts.xml written\nConcept tree:"       >-->--
>--     writeFile "graphs.bat" ""                                                               >>
>--     writeFile "atlas.dot" (dotGraph "Concept Overview" "1.25" relationList context)         >>
>--     putStr (", atlas.dot")                                                                  >>
>-- -- writing the large rhs frame (concept) with empty contents
>--     writeFile "Concept.html" (htmlPage "Concept" leader (htmlBody introtext))               >>
>--     putStr (", and Concept.html generated.")                                                >>
>-- -- writing context switching (top left)
>--     writeFile ("CTX_"++htmlNm context++".html") (contextFrame (Cl context world))           >>
>--     putStr("\nCode for "++name context++" navigator CTX_"++htmlNm context++".html written") >>
>-- -- writing the content for all contexts in ctxTree
>--     (if testing then (writeFile "test.txt".showHS.preCl) (Cl context world) else
>--      putStr "")                                                                             >>
>--     sequence_ [navigators cTrees c| c<-rd' name (preCl (Cl context world))]
>--     where
>--      context  = head ([c| c<-contexts, name c==contextname]++
>--                 [Ctx (contextname++" is not defined") [] empty nogE [] [] [] []])
>--      cTrees   = makeTrees (Typ (map reverse pths))
>--      Typ pths = typology (isa context)
>--      gE    = genE context
>--      world = wrld context
>--      contextFrame ctxTree@(Cl context cls)
>--       = htmlPage ("Code for "++name context++" navigator")
>--                  (wrapCode (name context)
>--                            (hmenu' ctxTree))
>--                  loadcode
>--          where
>--           rec (Cl context cls)
>--            = ("<A HREF=\"JavaScript: trigger('"++name context++".html"++
>--              "');\" TITLE="++show (name context)++
>--              ">--<IMG SRC=\"treemenutils/menu_new_root.gif\" ALIGN=\"left\" BORDER=\"0\" VSPACE=\"0\" HSPACE=\"0\" />--</A>--") :
>--              concat (map rec cls)
>--  navigators :: [Classification Concept] ->-- Context ->-- IO ()
>--  navigators cTrees context
>-- -- Part 1: First generate the html code and pictures for the entire context
>--       = putStr ("\n\nAnalyzing context "++name context)                                               >-->--
>--         (if testing
>--          then putStr ("\nISA structure of "++name context++":\n"++show (isa context)++
>--                       "\nMorphs:\n"++chain "\n" (map show (mors context))++
>--                       "\nMorphisms:\n"++chain "\n" (map show (morphisms context)))
>--          else putStr ""  )                                                                            >-->--
>--         writeFile ("NAV_"++thisCtx++".html") (navCodeCtx context)                                     >-->--
>--         writeFile (thisCtx++".dot") (dotGraph thisCtx "1.0" [] context)                             >-->--
>--         writeFile (thisCtx++".html") (htmlContext thisCtx context)                                    >-->--
>--         batGraph thisCtx context                                                  >-->--
>--         putStr ("\nCode for "++name context++" navigator NAV_"++thisCtx++".html written")             >-->--
>-- -- navigator ("NAV_"++thisCtx++".html") calls each pat<-patterns context, and all viewpoints c<-concs (morphisms context)
>-- -- Generate the html code and pictures for each pattern in this context
>--         sequence_ [ writeFile (thisPat++".html") (htmlPattern context thisPat pat)   >-->--
>--                     putStr ("\nCode for "++name pat++": "++thisPat++".html written") >-->--
>--                     batGraph thisPat pat                               
>--                   | pat<-patterns context, thisPat<-[thisCtx++"_"++htmlNm pat]]                       >-->--
>-- -- Generate the html code and pictures for each viewpoint in this context
>--         sequence_ [ writeFile (thisVpt++".html")
>--                               (htmlViewpoint context thisVpt cPat (name c) (Cl c cls))>-->--
>--                     putStr ("\nCode for viewpoint "++name c++": "++thisVpt++".html written") >-->--
>--                     batGraph thisVpt cPat                                                    >-->--
>--                     writeFile ("NAV_"++thisVpt++".html") (navCodeVp context cPat)            >-->--
>--                     putStr ("\nCode for navigator NAV_"++thisVpt++".html written")
>--                   | c<-concs context, Cl c' cls<-take 1 [t| t'<-cTrees context, t<-locates c t']
>--                   , thisVpt<-[thisCtx++htmlNm c], cPat<-[viewpoint context c]]                        >-->--
>-- -- Part 2: Now generate navigators for each pattern
>--         sequence_ [ writeFile ("NAV_"++thisCtx++"_"++htmlNm pat++".html")
>--                               (navCodePat context pat)           >-->--
>--                     sequence_ [ writeFile ("NAV_"++thisCtx++"_"++htmlNm pat++htmlNm c++".html")
>--                                (navCodePat context pat)
>--                               | c<-concs (morphisms pat)]        >-->--
>--                     sequence_ [ traverse [] subtree
>--                               | subtree<-(sort' name.cTrees.morphisms) pat]
>--                   | pat<-patterns context]                                                          >-->--
>-- -- navCodePat contains an anchor for each c<-concs (morphisms pat)
>-- -- Part 3: Now generate navigators for each concept in each pattern
>--         sequence_ [ sequence_ [writeFile ("NAV_"++thisCtx++"_"++htmlNm pat++htmlNm c++".html")
>--                                          (navCodePop context pat c) |c<-concs pat]
>--                   | pat<-patterns context]                                                          >-->--
>-- -- Part 4: Generate populations
>--         sequence_ (map (genRel context) (morphisms context))
>--          where
>--           batGraph fnm b
>--            = writeFile (fnm++".dot") (dotGraph fnm "1.0" [] b)  >-->--
>--              appendFile "graphs.bat"
>--                ("neato -Tpng "++epsi++fnm++".dot"++" -o "++fnm++".png\n"++
>--                 if textgraphs
>--                 then "neato -Tplain "++epsi++fnm++".dot -o "++fnm++".txt\n"++
>--                      "g "++fnm++"\ndel "++fnm++".txt\n"
>--                 else "")
>--              where epsi = if fnm `elem` warningList then "-Gepsilon=3 " else "" --changed from: "-Gepsilon=.1 "
>--           warningList = []
>--           cTrees cs
>--            = (makeTrees . Typ)
>--               [reverse pth| Typ pths<-[typology (isa context)], pth<-pths, last pth `elem` concs cs]
>--           traverse :: [Concept] ->-- Classification Concept ->-- IO ()
>--           traverse [] c@(Cl r nav) = sequence_ [traverse [r] cl| cl<-nav]
>--           traverse (e:trace) c@(Cl r nav)
>--            = batGraph thisVpt vptPat                                                >-->--
>--              (writeFile (thisVpt++".html"). htmlPage (htmlNm r) "" . htmlBody) h    >-->--
>--              putStr ("\n"++thisVpt++".dot and "++thisVpt++".html"++" written")      >-->--
>--              sequence_ [traverse (trace++[r]) cl| cl<-nav]
>--              where fnm     = thisCtx++htmlNm e++htmlNm r
>--                    h       = htmlViewpoint context thisVpt vptPat (name e) c
>--                    vptPat  = inhViewpoint (Cl context (wrld context)) e r
>--                    thisVpt = thisCtx++htmlNm e++htmlNm r
>--           thisCtx = htmlNm context
>--           navCodeCtx :: Context ->-- String
>--           navCodeCtx context
>--            = htmlPage ("Code for "++name context++" viewpoints navigator")
>--                       ("<SCRIPT type=\"text/javascript\">--\nfunction SwitchPat(value)\n { parent.concepts.document.location.href=value+'.html'\n ; parent.menu.document.location.href='NAV_'+value+'.html'\n }\n</SCRIPT>--")
>--                       (htmlBody
>--                          ("Patterns\n<BR>--\n"++
>--                           htmlDropDown
>--                             "SwitchPattern"
>--                             ["onChange=\"SwitchPat(this.value)\""]
>--                             ( ("value="++show(htmlNm context), "All patterns"):
>--                               [("value="++show(htmlNm context++"_"++htmlNm pat), name pat)
>--                               | pat<-patterns context]
>--                             )++
>--                           "\n<P>--\nViewpoints\n<BR>--\n"++
>--                           htmlDropDown
>--                            "SwitchViewpoint"
>--                            ["onChange=\"SwitchPat(this.value)\""]
>--                            ( ("value="++show(htmlNm context), "No viewpoint")
>--                              : [("value="++show(htmlNm context++htmlNm c), name c)
>--                                | c<-(sort' name.concs.morphisms) context]
>--                            )
>--                          )
>--                       )
>--           navCodeVp context cPat
>--            = htmlPage ("Code for "++name context++" viewpoints navigator")
>--                       ("<SCRIPT type=\"text/javascript\">--\nfunction SwitchPat(value)\n { parent.concepts.document.location.href='"++thisCtx++"'+value+'.html'\n ; parent.menu.document.location.href='NAV_"++thisCtx++"'+value+'.html'\n }\nfunction SwitchView(value)\n { parent.concepts.document.location.href='"++thisCtx++"'+value+'.html' }\n</SCRIPT>--")
>--                       (htmlBody
>--                          ("Patterns\n<BR>--\n"++
>--                           htmlDropDown
>--                             "SwitchPattern"
>--                             ["onChange=\"SwitchPat(this.value)\""]
>--                             ( ("value="++show(""), "All patterns"):
>--                               [("value="++show("_"++htmlNm pat), name pat)
>--                               | pat<-patterns context]
>--                             )++
>--                           "\n<P>--\nViewpoints\n<BR>--\n"++
>--                           htmlDropDown
>--                            "SwitchViewpoint"
>--                            ["onChange=\"SwitchView(this.value)\" onClick=\"SwitchView(this.value)\""]
>--                            ( ("value="++show(""), "No viewpoint")
>--                              : [("value="++show (htmlNm c)++
>--                                  if thisPat==htmlNm c then " selected" else "", name c)
>--                                | c<-(sort' name.concs.morphisms) context]
>--                            )
>--                          )
>--                       )
>--              where thisCtx = htmlNm context
>--                    thisPat = htmlNm cPat
>--           navCodePat :: Context ->-- Spec ->-- String
>--           navCodePat context cPat
>--            = htmlPage ("Code for "++name context++" viewpoints navigator")
>--                       ("<SCRIPT type=\"text/javascript\">--\nfunction SwitchPat(value)\n { parent.concepts.document.location.href='"++thisCtx++"'+value+'.html'\n ; parent.menu.document.location.href='NAV_"++thisCtx++"'+value+'.html'\n }\nfunction SwitchView(value)\n { parent.concepts.document.location.href='"++thisCtx++"'+value+'.html' }\n"++(if null ms then "" else "function SwitchPop(value)\n { parent.concepts.document.location.href=value+'.html' }\n")++"</SCRIPT>--")
>--                       (htmlBody
>--                          ("Patterns\n<BR>--\n"++
>--                           htmlDropDown
>--                             "SwitchPattern"
>--                             ["onChange=\"SwitchPat(this.value)\""]
>--                             ( ("value="++show(""), "All patterns"):
>--                               [("value="++show("_"++htmlNm p)++
>--                                  if thisPat==htmlNm p then " selected" else "",name p)
>--                               | p<-patterns context]
>--                             )++
>--                           "\n<P>--\nViewpoints\n<BR>--\n"++
>--                           htmlDropDown
>--                            "SwitchViewpoint"
>--                            ["onChange=\"SwitchView(this.value)\" onClick=\"SwitchView(this.value)\""]
>--                            ( ("value="++show("_"++thisPat),"No viewpoint")
>--                              : [("value="++show(htmlNm c),name c)
>--                                | c<-(sort' name.concs.morphisms.grules) cPat]
>--                            ) ++
>--                           if null ms then "" else
>--                           "<P>--\nPopulations\n<BR>--\n"++
>--                           htmlDropDown "Select Population" ["onChange=\"SwitchPop(this.value)\""] ms
>--                          )
>--                       )
>--              where ms = [( "value="++show("REL_"++htmlname (name context++"_"++name m++name (source m)++name (target m)))
>--                          , name m++"["++name (source m)++"*"++name (target m)++"]"
>--                          )
>--                         | m<-morphisms (rules cPat), not (null (contents m))]
>--                    thisCtx = htmlNm context
>--                    thisPat = htmlNm cPat
>--           navCodePop :: Context ->-- Spec ->-- Concept ->-- String
>--           navCodePop context cPat c
>--            = htmlPage ("Code for "++name context++" viewpoints navigator")
>--                       ("<SCRIPT type=\"text/javascript\">--\nfunction SwitchPat(value)\n { parent.concepts.document.location.href=value+'.html'\n ; parent.menu.document.location.href='NAV_'+value+'.html'\n }\nfunction SwitchView(value)\n { parent.concepts.document.location.href=value+'.html' }\n"++(if null ms then "" else "function SwitchPop(value)\n { parent.concepts.document.location.href=value+'.html'}\n")++"</SCRIPT>--")
>--                       (htmlBody
>--                          ("Patterns\n<BR>--\n"++
>--                           htmlDropDown
>--                             "SwitchPattern"
>--                             ["onChange=\"SwitchPat(this.value)\""]
>--                             ( ("value="++show(htmlNm context), "All patterns"):
>--                               [("value="++show(htmlNm context++"_"++htmlNm p)++
>--                                  if name cPat==name p then " selected" else "",name p)
>--                               | p<-patterns context]
>--                             )++
>--                           "\n<P>--\nViewpoints\n<BR>--\n"++
>--                           htmlDropDown
>--                            "SwitchViewpoint"
>--                            ["onChange=\"SwitchPat(this.value)\" onClick=\"SwitchView(this.value)\""]
>--                            [("value="++show(htmlNm context++"_"++htmlNm cPat++htmlNm c')++
>--                              if name c==name c' then " selected" else "",name c')
>--                            | c'<-(sort' name.concs.morphisms.grules) cPat]++
>--                           if null ms then "" else
>--                           "<P>--\nPopulations\n<BR>--\n"++
>--                           htmlDropDown "Select Population" ["onChange=\"SwitchPop(this.value)\""] ms
>--                          )
>--                       )
>--              where ms = [( "value="++show("REL_"++htmlname (name context++"_"++name m++name (source m)++name (target m)))
>--                          , name m++"["++name (source m)++"*"++name (target m)++"]"
>--                          )
>--                         | m<-morphisms [r|r<-rules context, c `elem` concs r], not (null (contents m))]
>--  genRel context m
>--       | null cs   = putStr ""
>--       | otherwise = writeFile ("REL_"++htmlname (name context++"_"++name m++a++b)++".html")
>--                      (htmlPage (name m++" :: "++a++" * "++b) "" ( htmlBody (nijssenZin m ++ htmlBlueTable [a,b] cs)))
>--       where a  = name (source m)
>--             b  = name (target m)
>--             cs = contents m
>--             nijssenZin (Mor _ _ _ _ l m r [] _)
>--              = "A tuple <I>--x</I>--,<I>--y</>-- in the following table means:<BR>--"++l++"&lt;x&gt;"++m++"&lt;y&gt;"++r++".<P>--"
>--             nijssenZin (Mor _ _ _ _ l m r (c:cs) _)
>--              = "A tuple, for example <I>--("++head c++","++last c++")</I>-- in the following table means:<BR>--"++l++head c++m++last c++r++".<P>--"
>--
>--  xmlContexts contexts contextname
>--      = "<CTXS>--"++concat [ctxInfo c|c<-contexts]++"\n</CTXS>--\n"++
>--        "\n<PATS>--"++concat [ "\n <PAT NAME=\""++name pat++"\" GRPH=\""++htmlname (nm++"_"++name pat)++".xml\">--"++
>--                             "\n </PAT>--"
>--                           | Ctx nm on isa gE world dc ms cs<-contexts, pat<-dc]++
>--        "\n</PATS>--"
>--      where
>--       ctxInfo (Ctx nm on isa gE world dc ms cs)
>--        = "\n <CTX NAME=\"" ++ nm ++ "\">--" ++
>--          "\n  <GEN>--<OL>--" ++  concat["<LI NAME=\""++o++"\n />--"|(n,o)<-cTuples,n==nm] ++ "</OL>--</GEN>--" ++
>--          "\n  <SPC>--<OL>--" ++  concat["<LI NAME=\""++n++"\n />--"|(n,o)<-cTuples,o==nm] ++ "</OL>--</SPC>--" ++
>--          concat ["\n  <PAT NAME=\"" ++ name pat ++ "\" GRPH=\"" ++ htmlname (nm ++ "_" ++ name pat) ++ ".xml\" />--" | pat<-dc] ++
>--          concat [ "\n  <REL ID=\""++htmlname (nm++"_"++nm'++name a++name b)++"\" NAME=\""++name m++"\" HREF=\"REL_"++htmlname (nm++"_"++nm'++name a++name b)++".html\" />--"
>--                 | m@(Mor nm' a b props prL prM prR cs pos)<-ms, not (null cs)]++
>--          "\n </CTX>--"
>--       cTuples = [(nm,o)| Ctx nm on _ _ _ _ _ _<-contexts, o<-on]
>--       tree str shw indent (Cl r [])
>--        = ind++"<"++str++" NAME=\""++name r++"\""++if null (shw ind r)
>--                                                   then " />--"
>--                                                   else ">--"++shw ind r++ind++"</"++str++">--"
>--          where ind = indent++" "
>--       tree str shw indent (Cl r cls)
>--        = ind++"<"++str++" NAME=\""++name r++"\">--"++shw ind r++
>--          concat [tree str shw ind c|c<-cls]++
>--          ind++"</"++str++">--"
>--          where ind = indent++" "
>--       cShw indent (C _ _) = ""

>--  loadcode = "<BODY onload=\"MTMStartMenu()\" bgcolor=#FFFFBB link=#AA0000 alink=#AA0000 hlink=#AA0000>--</BODY>--"
>--  introtext
>--      = "Use the classification of concepts (on your left) to browse."
>--  leader
>--      = "<SCRIPT type=\"text/javascript\">--\n"++
>--        "  if((navigator.appName == \"Netscape\" && parseInt(navigator.appVersion) >--= 3 && navigator.userAgent.indexOf(\"Opera\") == -1 && navigator.userAgent.indexOf(\"WebTV\") == -1) || (navigator.appName == \"Microsoft Internet Explorer\" && parseInt(navigator.appVersion) >--= 4)) {\n"++
>--        "    for(i = 0; i < parent.frames.length; i++) {\n"++
>--        "      if(parent.frames[i].name == \"code\" && parent.frames[i].MTMLoaded) {\n"++
>--        "        parent.frames[i].MTMTrack = true;\n"++
>--        "        setTimeout(\"parent.frames[\" + i + \"].MTMDisplayMenu()\", 250);\n"++
>--        "        break;\n"++
>--        "      }\n"++
>--        "    }\n"++
>--        "  }\n"++
>--        "</SCRIPT>--"
>--  indexcode filename
>--      = chain "\n"
>--        [ "<HTML>--"
>--        , htmlHead ("Concepts") ""
>--        , "<SCRIPT TYPE=\"text/javascript\">--"
>--        , "var MTMUsableBrowser = false;"
>--        , "// browser sniffing routine"
>--        , "browserName = navigator.appName;"
>--        , "browserVersion = parseInt(navigator.appVersion);"
>--        , "if(browserName == \"Netscape\" && browserVersion >--= 3) {"
>--        , "  MTMUsableBrowser = (navigator.userAgent.indexOf(\"Opera\") == -1) ? true : false;"
>--        , "} else if(browserName == \"Microsoft Internet Explorer\" && browserVersion >--= 4) {"
>--        , "  MTMUsableBrowser = true;"
>--        , "};"
>--        , "if(!MTMUsableBrowser) alert('This page was not designed for your browser. Please use Netscape or Internet Explorer if you get inexplicable behaviour');"
>--        , "document.write('<FRAMESET Cols=\"1,210,*\" border=0 frameborder=\"no\" framespacing=\"0\">--');"
>--        , "document.write('  <FRAME Name=\"code\" SRC=\"CTX_"++filename++"\" NORESIZE FRAMEBORDER=\"No\">--');"
>--        , "document.write('  <FRAMESET Rows=\"150,*\" border=0 frameborder=\"no\" framespacing=\"0\">--');"
>--        , "document.write('    <FRAME Name=\"context\" SRC=\"treemenutils/conceptsmenu_empty.html\" NORESIZE FRAMEBORDER=\"No\">--');"
>--        , "document.write('    <FRAME Name=\"menu\" SRC=\"NAV_"++filename++"\" NORESIZE FRAMEBORDER=\"No\">--');"
>--        , "document.write('  </FRAMESET>--');"
>--        , "document.write('  <FRAME Name=\"concepts\" SRC=\"Concept.html\" NORESIZE FRAMEBORDER=\"No\">--');"
>--        , "document.write('</FRAMESET>--');"
>--        , "</SCRIPT>--"
>--        , htmlBody "Your browser does not support frames."
>--        , "</HTML>--"]

The following function makes a HTML page for a complete pattern.

>--  htmlPattern :: Context ->-- String ->-- Spec ->-- String
>--  htmlPattern ctx fnm pat@(Pat nm rs parChds pms cs)
>--   = htmlPage ("Code for "++nm) ""
>--                   (htmlBody (htmlValNumbered [(nr r,explain r) | r<-rules pat]++"\n"++
>--                              htmlImage (fnm++".png")++"\n"++htmlGlossary ctx pat))
>--  htmlContext :: String ->-- Context ->-- String
>--  htmlContext fnm ctx
>--   = htmlPage ("Code for "++nm) ("<SCRIPT type=\"text/javascript\">--\nparent.menu.document.location.href='NAV_"++fnm++".html'\n</SCRIPT>--")
>--                   (htmlBody (htmlValNumbered [(nr r,explain r) | r<-rs]++"\n"++htmlImage (fnm++".png")))
>--     where Pat nm rs parChds pms cs = foldr1 union (patterns ctx)

>--  htmlGlossary ctx@(Ctx cnm on isa gE world dc ms cds) pat
>--   = htmlHeadinglevel 1 "Glossary" []++
>--     htmlTable [[c,cdef]| Cd _ c cdef _<-cds, C c [] `elem` concs pat]

The following function makes a HTML page for one particular concept c, interpreted in the context of world.
This page is mounted in the contents frame of the architecture page, to which the navigator (left hand side of 
the screen) points.

 Pre: c<-concs (morphisms context)

>--  viewpoint :: Context ->-- Concept ->-- Spec
>--  viewpoint context c
>--   = Pat (name c)
>--         rulesV
>--         (clearG [G g s| Isa ts ss<-[isa context], (g,s)<-ts, g `elem` concsV, s `elem` concsV])
>--         (morphisms rulesV)
>--         [c| c@(Cd pos nm def ref)<-conceptdefs context, C nm [] `elem` concsV]
>--     where rulesV = [r| r<-rules context, c `elem` concs r]
>--           concsV = concs rulesV

>--  inhViewpoint :: Classification Context ->-- Concept ->-- Concept ->-- Spec
>--  inhViewpoint (Cl context world) specific gen
>--   = Pat ("Viewpoint "++name specific++" inherited from "++name gen)
>--         (rs++[s| s<-sc, Isa ts ss<-[isa s], and[b `elem` concs rs| (a,b)<-ts]])
>--         (clearG [G g s| Isa ts ss<-[isa context], (g,s)<-ts, g==gen, s `elem` concs rs])
>--         (morphisms rs)
>--         [c| c@(Cd pos nm def ref)<-conceptdefs context, C nm [] `elem` rd [c|r<-rs, c<-concs r]]
>--     where
>--       rs     = rd [sr | r<-rules world, gen `elem` concs r, s<-sc
>--                       ,sr<-[subsR gE s r], specific `elem` concs sr]
>--       sc     = [s| s<-specs context, gen `elem` concs s]
>--       gE     = genE context
>--     rulesG = rd [ subsC gE s r | r<-rules world, gen `elem` concs r, s<-substns r]
>--     substns r
>--      = (foldr cp [[]].map (map single).eqCl fst.clear)
>--          [(g,s)| c<-concs r, Isa ts ss<-[isa context], (g,s)<-ts, c==g]
>--        where cp as bs = [a++b|a<-as,b<-bs] ; single x = [x]

>--  htmlViewpoint :: Context ->-- String ->-- Spec ->-- String ->-- Classification Concept ->-- String
>--  htmlViewpoint context@(Ctx cnm on isa _ world dc ms cds) fnm pat c (Cl r cls)
>--   = (htmlPage c "" . htmlBody)
>--     (vptTitle c                                                                             ++
>--      "Properties of concept "++anchorlink c++" in context "++thisCtx                        ++
>--      (if c==name r then "" else " emerging from properties of "++name r)                    ++
>--      (if null cls then "" else
>--        "\n<BR>--inheriting properties from "++
>--        commaAnd [htmlAnchor (thisCtx++c++name g++".html") (name g) []|Cl g cls'<-cls]++".") ++
>--      concat ["\n<P>--\nDefinition of "++htmlItalic c'++"\n<BR>--\n"++cdef| Cd _ c' cdef _<-cds, c'==c]   ++
>--      "\n<FONT color=\"#AA0000\">--<HR color=#AA0000>--</FONT>--\n"++tests                         ++
>--      htmlValNumbered (map (hgenR gE) (rules pat++rules (specs pat)))++htmlImage (fnm++".png"))
>--     where
>--       thisCtx = htmlNm context
>--       gE = genE context
>--       gen = name r
>--       inheriting =  [G g s|G g s<-parChds, name s==c]
>--       tests
>--        = if testing
>--          then "Testing:"++
>--               "<BR>--cs = "++show (concs pat)++
>--               "<BR>--rs = "++show (rules pat)
>--          else ""

>--  hgenR gE r
>--   = (nr r, ruleviol (nr r)++(hshow.assemble gE.normRule (lub gE)) r++"\n<BR>--\n"++explain r++"\n</P>--")
>--     where
>--      ruleviol :: Int ->-- String
>--      ruleviol i
>--       | i `elem` violHtml = "Click "++htmlAnchor ("Atlas"++show i++"Viol.html") "here" []++" to see violations.\n<BR>--\n"
>--       | otherwise         = ""

>--  vptTitle c = "<A TITLE=\"IEEE 1471-2000: A viewpoint is a specification of the conventions for constructing and using a view. A pattern or template from which to develop individual views by establishing the purposes and audience for a view and the techniques for its creation and analysis.\">--Viewpoint: "++c++"</A>--\n<P>--\n"

>--{-  htmlConcept :: GenR ->-- String ->-- Classification Context ->-- [Concept] ->-- Classification Concept ->-- (String,String)
>--  htmlConcept gE fnm (Cl context world) trace t@(Cl c cls)
>--   | c `elem` concG
>--      = (vptTitle c'++"Properties of "++name c'++" emerging from properties of "++name c++
>--         ".\n<FONT color=\"#AA0000\">--<HR color=#AA0000>--</FONT>--\n"++tests++
>--         htmlValNumbered (map (hgenR gE) [r|r<-ruleG, c' `elem` concs r])++
>--         htmlImage (fnm++".png")
>--        ,graph (name c') ruleG)
>--   | c `elem` concS
>--      = (vptTitle c++"Properties of concept "++anchorlink(name c)++" in context "++name context++
>--         (if null cls then "" else
>--          "<BR>--inheriting properties from "++
>--          commaAnd [htmlAnchor (name context++(if null trace then name c else name (head trace))++name e++".html") (name e) []|e<-cls]++".")++
>--         "\n<FONT color=\"#AA0000\">--<HR color=#AA0000>--</FONT>--\n"++tests++
>--         htmlValNumbered (map (hgenR gE) ruleS)++htmlImage (fnm++".png")
>--        ,graph (name c) ruleS)
>--   | otherwise = ("Empty viewpoint"++
>--                  if testing then "\n<BR>--\nconcS = "++show (map name concS)++"\n<BR>--\nconcG = "++show (map name concG) else ""
>--                 ,dotGraph (name c) "1.0" relationList (Id posNone [] c))
>--   where
>--       c'    = if null trace then c else head trace
>--       concS = concs context
>--       ruleS = [r| r<-rules context, c `elem` concs r]
>--       concG = concs world
>--     ruleG = rd [ subsC gE s r | r<-rules world, c `elem` concs r, s<-substns r]
>--       ruleG = rd [ foldr (.) id [subsR gE s| s<-sc] r | r<-rules world] where sc=specs context
>--       graph nm rs = dotGraph nm "1.0" relationList (Pat (name c) rs [] [] [] [])
>--       trs = if null trc then "concept "++name c else
>--             (if length trc>--1 then "concepts " else "concept ")++chain "," trc
>--             where trc = [anchorlink(name c)|c<-trace `isc` concG]
>--       substns r = (foldr cp [[]].map (map single).eqCl fst.clear) [(g,c)| g<-concs r, c<-concS, g `gE` c]
>--                   where cp as bs = [a++b|a<-as,b<-bs] ; single x = [x]
>--       subst tuples c = head ([c'| (C p _,C c' _)<-tuples, p==c]++[c])
>--       vptTitle c = "<A TITLE=\"IEEE 1471-2000: A viewpoint is a specification of the conventions for constructing and using a view. A pattern or template from which to develop individual views by establishing the purposes and audience for a view and the techniques for its creation and analysis.\">--Viewpoint: "++(name c)++"</A>--\n<P>--\n"
>--       ruleviol :: Int ->-- String
>--       ruleviol i
>--        | i `elem` violHtml = "Click "++htmlAnchor ("Atlas"++show i++"Viol.html") "here" []++" to see violations.\n<BR>--\n"
>--        | otherwise         = ""
>--       tests
>--        = if testing
>--          then "Testing:<BR>--concS = "++show concS++
>--               "<BR>--concG = "++show concG++
>--               "<BR>--ruleS = "++show ruleS++
>--               "<BR>--ruleG = "++show ruleG++
>--               "<BR>--rules world = "++show (rules world)++
>--               "<BR>--specs context = "++show (specs context)
>--          else ""-}

>--  class HTML a where
>--   hshow :: a ->-- String

>--  instance HTML Concept where
>--   hshow (C nm _) = htmlAnchor (htmlname nm++".html") nm []

>--  instance HTML Morph where
>--   hshow (Id pos atts nm) = "="
>--   hshow m                = if generated m 
>--                            then htmlAnchor (mor2filename m) (mor2name m) []
>--                            else mor2name m

>--  anchorlink nm = if nm `elem` conceptList then htmlAnchor (htmlname ("Atlas"++conceptForm nm++".html")) nm [] else nm
>--  generated m = mor2 m `elem` relHtml

  conceptList = rd [e| [r,d,c]<-relHtml,e<-[d,c]] `isc` rConceptList

>--  instance HTML Term where
>--   hshow t       = (dePoint.mumble.name) t

  hsign (Sg a b) ps
        | m Uni && m Tot && m Inj && m Sur = a++" <->-- "++b
        | m Uni && m Tot = a++" -->-- "++b
        | m Sur && m Inj = a++" <-- "++b
        | m Uni          = a++" |->-- "++b
        | m Inj          = a++" <-| "++b 
        | otherwise      = "("++a++","++b++")"
        where m e = e `elem` ps

>--  hmenu :: String ->-- Classification Concept ->-- String
>--  hmenu ctxname (Cl r cls)
>--    = chain "\n" (["\n//menu menu\n\nvar menu = null;\nmenu = new MTMenu();"] ++
>--          [menuline "menu" (htmlname (ctxname++htmlNm (root cl)++".html")) cl| cl<-cls] ++
>--          concat [recur "menu" (sh r) (show i) (Cl r cls) [r]| (i, Cl r cls)<-zip [0..] cls, not (null cls)])
>--      where
>--       recur nm rootname cNr (Cl r cls) trace
>--        = ["\n//"++rootname++" menu\n\nvar "++varname++" = null;\n"++varname++" = new MTMenu();"] ++
>--          [menuline varname (fnm (root cl)) cl| cl<-cls] ++
>--          [nm++".items["++cNr++"].MTMakeSubmenu("++varname++");"] ++
>--          concat [recur varname (sh r) (show i) (Cl r cls) (trace++[r])| (i, Cl r cls)<-zip [0..] cls, not (null cls)]
>--          where fnm r = htmlname (ctxname++name r++if null trace then "" else name (head trace))++".html"
>--                varname = avoidJSreservedwords rootname
>--       menuline rn fnm (Cl r cls)
>--        = rn++".MTMAddItem(new MTMenuItem(\""++htmlNm r++"\", \""++fnm++"\", \"concepts\"));"
>--       sh root = [if isUpper c then chr(ord c - ord 'A' + ord 'a') else c| c<-mumble (name root)]

>--  hmenu' :: Classification Context ->-- String
>--  hmenu' (Cl r cls)
>--    = chain "\n" (["\n//menu menu\n\nvar menu = null;\nmenu = new MTMenu();"] ++
>--                  [menuline "menu" (htmlNm r)]   ++
>--                  recur "menu" (sh r) "0" (Cl r cls) [])
>--      where
>--       recur nm rootname cNr (Cl r cls) trace
>--        = ["\n//"++rootname++" menu\n\nvar "++varname++" = null;\n"++varname++" = new MTMenu();"] ++
>--          [menuline varname (htmlNm r)| (Cl r cls')<-cls] ++
>--          [nm++".items["++cNr++"].MTMakeSubmenu("++varname++");"] ++
>--          concat [recur varname (sh r) (show i) (Cl r cls) (trace++[r])| (i, Cl r cls)<-zip [0..] cls, not (null cls)]
>--          where varname = avoidJSreservedwords rootname
>--       menuline rn fnm
>--        = rn++".MTMAddItem(new MTMenuItem(\""++fnm++"\", \""++fnm++".html"++"\", \"concepts\"));"
>--       sh root = [if isUpper c then chr(ord c - ord 'A' + ord 'a') else c| c<-mumble (name root)]

>--  wrapCode ctxName lines
>--   = chain "\n"
>--     ([ "<SCRIPT type=\"text/javascript\" src=\"treemenutils/mtmcode.js\">--</SCRIPT>--"
>--      , "<SCRIPT type=\"text/javascript\">--"
>--      , "// Morten's JavaScript Tree Menu"
>--      , "// written by Morten Wang <morten@treemenu.com>-- (c) 1998-2000"
>--      , "// This is version 2.2.6, dated 2000-03-30"
>--      , "// The script is freely distributable"
>--      , "// It may be used (and modified) as you wish, but retain this message"
>--      , "// For more information about the menu visit its home page"
>--      , "// http://www.treemenu.com/"
>--      , "var MTMTableWidth = \"100%\";"
>--      , "var MTMenuFrame = \"context\";"
>--      , "var MTMSubsGetPlus = false;"
>--      , "var MTMEmulateWE = true;"
>--      , "var MTMenuImageDirectory = \"treemenutils/\";"
>--      , "var MTMBGColor = \"#FFFFBB\";"
>--      , "var MTMBackground = \"\";"
>--      , "var MTMTextColor = \"#000000\";"
>--      , "var MTMLinkColor = \"#AA0000\";"
>--      , "var MTMAhoverColor = \"#000000\";"
>--      , "var MTMTrackColor =\"#000000\";"
>--      , "var MTMSubExpandColor = \"#666699\";"
>--      , "var MTMSubClosedColor = \"#666699\";"
>--      , "var MTMRootIcon = \"menu_new_root.gif\";"
>--      , "var MTMenuText = \"Context tree\";"
>--      , "var MTMRootColor = \"#000000\";"
>--      , "var MTMRootFont = \"Arial, Helvetica, sans-serif\";"
>--      , "var MTMRootCSSize = \"84%\";"
>--      , "var MTMRootFontSize = \"-1\";"
>--      , "var MTMenuFont = \"Arial, Helvetica, sans-serif\";"
>--      , "var MTMenuCSSize = \"84%\";"
>--      , "var MTMenuFontSize = \"-1\";"
>--      , "var MTMLinkedSS = false;"
>--      , "var MTMSSHREF = \"menu.css\";"
>--      , "var MTMSubsAutoClose = false;"
>--      , "var MTMTimeOut = 15;"
>--      , "var MTMIconList = null;"
>--      , "MTMIconList = new IconList();"
>--      , "MTMIconList.addIcon(new MTMIcon(\"menu_link_external.gif\", \"http://\", \"pre\"));"
>--      , "MTMIconList.addIcon(new MTMIcon(\"menu_link_pdf.gif\", \".pdf\", \"post\"));"
>--      , lines
>--      , "</SCRIPT>--"])

>--  instance HTML Restriction where
>--    hshow (Forall vars restr)
>--     = charVars "&forall;" {- "<IMG SRC=\"treemenutils/forall.gif\" ALT=\"For all \">--" -} vars ++ hshow restr
>--    hshow (Exists vars restr)
>--     = charVars "&exist;" {- "<IMG SRC=\"treemenutils/exists.gif\" ALT=\"Exists \">--" -} vars  ++ hshow restr
>--    hshow (Implies antc cons)
>--     = "<BR>--\n"++hshow antc++" <IMG SRC=\"treemenutils/arright.gif\" ALT=\"implies \">-- "  ++ hshow cons
>--    hshow (Equiv lhs rhs)
>--     = "<BR>--\n"++hshow lhs++" <IMG SRC=\"treemenutils/arboth.gif\" ALT=\"Equivalent \">-- "   ++ hshow rhs
>--    hshow (Conj [r]) = hshow r
>--    hshow (Conj rs)
>--     = if null rs then "" else
>--       chain " &and; " {- " <IMG SRC=\"treemenutils/and.gif\" ALT=\"and \">-- " -} (map hsh rs)
>--    hshow (Disj [r]) = hshow r
>--    hshow (Disj rs)
>--     = if null rs then "" else
>--       chain " &or; " {- " <IMG SRC=\"treemenutils/or.gif\" ALT=\"or \">-- " -} (map hsh rs)
>--    hshow (Not rs) = " &not; " {- "<IMG SRC=\"treemenutils/not.gif\" ALT=\"not \">-- " -} ++hshow rs
>--    hshow (Rel lhs (Tm m sgn) rhs d c)
>--     = "<A TITLE=\""++helptext d left m c right++"\">--"++
>--       hshow (Funs d left)++" "++hshow m++" "++hshow (Funs c right)++"</A>--"
>--       where left = [m| t<-lhs, m<-mors t]; right = [m| t<-reverse rhs, m<-mors t]
>--    hshow (Rel lhs (Tf m sgn) rhs d c)
>--     = "<A TITLE=\""++helptext c left m d right++"\">--"++
>--       hshow (Funs c left)++" "++hshow m++" "++hshow (Funs d right)++"</A>--"
>--       where right = [m| t<-lhs, m<-mors t]; left = [m| t<-reverse rhs, m<-mors t]
>--    hshow (Rel lhs (Tc f sgn) rhs d c)
>--     = error "Rel lhs (Tc f sgn) rhs"
>--    hshow (Funs x [])         = x
>--    hshow (Funs x (Id _ _ _:ms)) = hshow (Funs x ms)
>--    hshow (Funs x (m:ms))        = hshow (Funs (hshow m++"("++x++")") ms)

>--  hsh r@(Exists vars restr)
>--   = "("++hshow r++")"
>--  hsh r@(Forall vars restr)
>--   = "("++hshow r++")"
>--  hsh r@(Conj [r']) = hshow r'
>--  hsh r@(Conj restr)
>--   = "("++hshow r++")"
>--  hsh r@(Disj [r']) = hshow r'
>--  hsh r@(Disj restr)
>--   = "("++hshow r++")"
>--  hsh r = hshow r

>--  helptext :: String ->-- [Morph] ->-- Morph ->-- String ->-- [Morph] ->-- String
>--  helptext a ams (Id pos atts nm)  b bms = funnotate a ams ++ " = " ++ funnotate b bms
>--  helptext a ams (Mph _ _ _ _ mor) b bms = applyM mor (funnotate a ams) (funnotate b bms)++shCard mor
>--   where
>--     shCard mor
>--      | null (cards mor) = ""
>--      | otherwise        = "\n(Properties: "++(chain ", " . map show ) (cards mor)++")"

>--  funnotate x      []       = x
>--  funnotate x (Id _ _ _:ms) = funnotate x ms
>--  funnotate x (m:ms)        = funnotate (hshow m++"("++x++")") ms

>--  charVars q vs
>--   = if null vs then "" else
>--     q++" "++chain "; " [chain ", " (map fst vs')++"::"++name (snd (head vs')) | vs'<-eqCl snd vs]++": "

>--  htmlNm :: Identified a =>-- a->--String
>--  htmlNm = htmlname.name