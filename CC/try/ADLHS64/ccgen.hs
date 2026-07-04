 module Main where
  import System
  import Char
  import UU_Scanner
  import UU_Parsing
  import Auxiliaries
  import Classification
  import Typology
  import CC_aux
  import Html
  import Restrictions
  import LATEXgen
  import ConceptList
  import RelationList
  import AGtry
  import CC
  import Calc

  testing = False
  textgraphs = False
  latexOpt sws = "-l" `elem` sws
  violHtml = []
  splitStr f (x:xs) | f x  = (x:yes, no)
                    | True = (yes, x:no)
                    where (yes,no) = splitStr f xs
  splitStr f [] = ([],[])

  main
   = do { a <- getArgs
        ; let (switches,args) = splitStr ((=="-").take 1) a
        ; putStr ("Arguments: "++chain ", " args++"\nSwitches: "++chain ", " switches)
        ; if length args==0 then putStr ("Please provide a filename (.adl) and a context name") else
     do { let fn = args!!0; contextname = args!!1
              (fnPrefix,fnSuffix) = break ('.' ==) fn
              fnFull = if null fnSuffix then (fn ++ ".adl") else fn
        ; inp<-readFile fnFull
        ; putStr ("\n"++fnFull++" is read.")
        ; slRes <- parseIO pArchitecture (scan keywordstxt keywordsops specialchars opchars fnFull initPos inp)
        ; putStr ("\n"++fnFull++" has been parsed.")
        ; let (contexts,errs) = sem_Architecture slRes
-- The following is very dangerous to add:
--   if a source.adl contains errors, the evaluation of contexts may run into failures before errors are being reported.
--   This can cause needless debugging and hence cost bundles of time. (Don't gloat . . .)
--        ; if testing then (writeFile "parsed.txt".showHS) contexts else putStr ""
        ; let Typ pths = if null contexts then Typ [] else typology (isa (head contexts))
        ; putStr "\nConcepts:\n" >>(putStr.chain "\n".map show) (makeTrees (Typ (map reverse pths)))
        ; if null errs 
          then (putStr ("\nNo type errors or cyclic specializations were found.\n")>>
                if length args==1 && length contexts==1
                then build contexts switches (name (head contexts)) else
                if length args==1 && length contexts>1
                then putStr ("\nPlease specify the name of a context."++
                             "\nAvailable contexts: "++commaAnd (map name contexts)++".\n") else
                if length args>1 && contextname `elem` map name contexts
                then build contexts switches contextname
                else putStr ("\nContext "++contextname++" not defined."++
                             "\nPlease specify the name of an available context."++
                             "\nAvailable contexts: "++commaAnd (map name contexts)++"."++
                             "\n(Note: context names are case sensitive).\n")
               )
          else putStr ("\nThe type analysis of "++fnFull++" yields errors.\n")>>
               putStr (concat ["!Error of type "++err| err<-errs])>>
               putStr ("Nothing generated, please correct mistake(s) first.\n")
        }}
      where build contexts switches contextname
             = sequence_ 
                ([ anal contexts contextname ("-p" `elem` switches)
                 | null switches || "-h" `elem` switches || "-p" `elem` switches]++
                 [ calcRules contexts contextname | "-R" `elem` switches]++
                 [ zed contexts contextname| "-Z" `elem` switches]++
                 [ glossary contexts contextname | "-g" `elem` switches]
                )

  glossary contexts contextname
   = putStr ("\nGenerating Glossary for "++name context++" in the current directory.") >>
     lglossary context
     where
      context  = head ([c| c<-contexts, name c==contextname]++
                 [Ctx (contextname++" is not defined") [] empty [] [] [] []])

--        Typ pths = typology (isa context)

  calcRules contexts contextname
   = putStr ("\nCalculation rules for "++name context++"\n"++
            (chain "\n\n" . map shR . rules) context)
     where
      context  = head ([c| c<-contexts, name c==contextname]++
                 [Ctx (contextname++" is not defined") [] empty [] [] [] []])
      shR r = showADL r++"\n  "++chain "\n  " (map showADL (insertRules [r]++deleteRules [r]))

  zed contexts contextname
   = putStr ("\nGenerating Zed specification for "++name context++" in the current directory.") >>
     lprint context
     where
      context  = head ([c| c<-contexts, name c==contextname]++
                 [Ctx (contextname++" is not defined") [] empty [] [] [] []])

  anal contexts contextname predLogic
   = putStr ("\nGenerating Atlas for "++name context++" in the current directory."++
             "\n  (current directory must already contain the directory \"treemenutils\""++
             " with its complete contents)")                                                 >>
-- writing the main html page, containing the required frames
     writeFile "index.html" (indexcode (htmlNm context++".html"))                            >>
     putStr ("\nindex.html written")                                                         >>
-- writing XML and DOT information for graphics purposes.
--     writeFile "contexts.xml" (xmlContexts contexts contextname)                             >>
--     putStr "\ncontexts.xml written\nConcept tree:"       >>
     writeFile "graphs.bat" ""                                                               >>
-- writing the large rhs frame (concept) with empty contents
     writeFile "Concept.html" (htmlPage "Concept" leader (htmlBody introtext))               >>
     putStr (", and Concept.html generated.")                                                >>
-- writing context switching (top left)
     writeFile ("CTX_"++htmlNm context++".html") (contextFrame (Cl context world))           >>
     putStr("\nHTML code for context tree CTX_"++htmlNm context++".html written") >>
-- writing the content for all contexts in ctxTree
     (if testing then (writeFile "test.txt".showHS.preCl) (Cl context world) else
      putStr "")                                                                             >>
     sequence_ [navigators cTrees c predLogic| c<-preCl (Cl context world)]                  >>
     sequence_ [genAnalysis c predLogic| c<-preCl (Cl context world)]
     where
      context  = recalc (head ([c| c<-contexts, name c==contextname]++
                               [Ctx (contextname++" is not defined") [] empty [] [] [] []]))
      cTrees   = makeTrees (Typ (map reverse pths))
      Typ pths = typology (isa context)
      gE    = genE context
      world = wrld context
      contextFrame ctxTree@(Cl context cls)
       = htmlPage ("Code for "++name context++" navigator")
                  (wrapCode (name context)
                            (hmenu' ctxTree))
                  loadcode
          where
           rec (Cl context cls)
            = ("<A HREF=\"JavaScript: trigger('"++name context++".html"++
              "');\" TITLE="++show (name context)++
              "><IMG SRC=\"treemenutils/menu_new_root.gif\" ALIGN=\"left\" BORDER=\"0\" VSPACE=\"0\" HSPACE=\"0\" /></A>") :
              concat (map rec cls)
  navigators :: [Classification Concept] -> Context -> Bool -> IO ()
  navigators cTrees context predLogic
 -- Part 1: First generate the html code and pictures for the entire context
       = putStr ("\n\nAnalyzing context "++name context)                                               >>
         (if testing
          then putStr ("\nISA structure of "++name context++":\n"++show (isa context)++
                       "\nMorphisms:\n"++chain "\n" (map show (mors context))++
                       "\nSignatures:\n"++chain "\n" (map show (signatures context)))
          else putStr ""  )                                                                            >>
         writeFile ("NAV_"++thisCtx++".html") (navCodeCtx context)                                     >>
--         writeFile (thisCtx++".dot") (dotGraph thisCtx "1.0" [] context)                             >>
         writeFile (thisCtx++".html") (htmlContext thisCtx context)                                    >>
         batGraph thisCtx context                                                                      >>
         putStr ("\nHTML code for "++name context++" navigator NAV_"++thisCtx++".html written")             >>
-- navigator ("NAV_"++thisCtx++".html") calls each pat<-patterns context, and all concepts c<-concs (signatures context)
-- Generate the html code and pictures for each pattern in this context
         sequence_ [ writeFile (thisPat++".html") (htmlPattern context thisPat pat)   >>
                     putStr ("\nHTML code for "++name pat++": "++thisPat++".html written") >>
                     batGraph thisPat pat                               
                   | pat<-patterns context, thisPat<-[thisCtx++"_"++htmlNm pat]]                       >>
-- Now generate navigators for each pattern
         sequence_ [ writeFile ("NAV_"++thisCtx++"_"++htmlNm pat++".html")
                               (navCodePat context pat)           >>
                     sequence_ [ writeFile ("NAV_"++thisCtx++"_"++htmlNm pat++htmlNm c++".html")
                                (navCodePat context pat)
                               | c<-concs (signatures pat)]        >>
                     sequence_ [ traverse [] subtree
                               | subtree<-(sort' name.cTrees.signatures) pat]
                   | pat<-patterns context]                                                          >>
-- Now generate html code and pictures for each rule in this context
         sequence_ [ writeFile (htmlNm context++"_Rule_"++show (nr r)++".html") (htmlRule context r predLogic)   >>
                     putStr ("\nHTML code for Rule "++show (nr r)++": "++htmlNm context++"_Rule_"++show (nr r)++".html written") >>
                     batGraph (htmlNm context++"_Rule_"++show (nr r)) (Pat ("Rule "++show (nr r)) [r] [] [] [])                               
                   | r<-rules context++rules (specs context)]              >>
-- Part 2: Generate the html code and pictures for each concept in this context
         sequence_ [ writeFile (thisVpt++".html")
                               (htmlViewpoint context thisVpt cPat c (Cl c cls) predLogic)>>
                     putStr ("\nHTML code for concept "++name c++": "++thisVpt++".html written") >>
                     batGraph thisVpt cPat                                                    >>
                     writeFile ("NAV_"++thisVpt++".html") (navCodeVp context cPat)            >>
                     putStr ("\nHTML code for navigator NAV_"++thisVpt++".html written")
                   | c<-concs context, Cl c' cls<-take 1 [t| t'<-cTrees context, t<-locates c t']
                   , thisVpt<-[thisCtx++htmlNm c], cPat<-[viewpoint context c]]                        >>
-- navCodePat contains an anchor for each c<-concs (signatures pat)
-- Part 3: Now generate navigators for each concept in each pattern
         sequence_ [ sequence_ [writeFile ("NAV_"++thisCtx++"_"++htmlNm pat++htmlNm c++".html")
                                          (navCodePop context pat c) |c<-concs pat]
                   | pat<-patterns context]
          where
           batGraph fnm b
            = writeFile (fnm++".dot") (dotGraph fnm "1.0" [] b)  >>
              appendFile "graphs.bat"
                ("neato -Tpng "++fnm++".dot"++" -o "++fnm++".png\n"++
                 if textgraphs
                 then "neato -Tplain "++fnm++".dot -o "++fnm++".txt\n"++
                      "g "++fnm++"\ndel "++fnm++".txt\n"
                 else "")
           epsi = "-Gepsilon=3"
           cTrees cs
            = (makeTrees . Typ)
               [reverse pth| Typ pths<-[typology (isa context)], pth<-pths, last pth `elem` concs cs]
           traverse :: [Concept] -> Classification Concept -> IO ()
           traverse [] c@(Cl r nav) = sequence_ [traverse [r] cl| cl<-nav]
           traverse (e:trace) c@(Cl r nav)
            = batGraph thisVpt vptPat                                                >>
              (writeFile (thisVpt++".html"). htmlPage (htmlNm r) "" . htmlBody) h    >>
              putStr ("\n"++thisVpt++".dot and "++thisVpt++".html"++" written")      >>
              sequence_ [traverse (trace++[r]) cl| cl<-nav]
              where fnm     = thisCtx++htmlNm e++htmlNm r
                    h       = htmlViewpoint context thisVpt vptPat e c predLogic
                    vptPat  = inhViewpoint (Cl context (wrld context)) e r
                    thisVpt = thisCtx++htmlNm e++htmlNm r
           thisCtx = htmlNm context
           navCodeCtx :: Context -> String
           navCodeCtx context
            = htmlPage ("Code for "++name context++" concepts navigator")
                       ("<SCRIPT type=\"text/javascript\">\nfunction SwitchPat(value)\n { parent.concepts.document.location.href=value+'.html'\n ; parent.menu.document.location.href='NAV_'+value+'.html'\n }\n"++(if null ms then "" else "function SwitchPop(value)\n { parent.concepts.document.location.href=value+'.html' }\n")++"</SCRIPT>")
                       (htmlBody
                          ("Patterns\n<BR>\n"++
                           htmlDropDown
                             "SwitchPattern"
                             ["onChange=\"SwitchPat(this.value)\""]
                             ( ("value="++show(htmlNm context), "All patterns"):
                               [("value="++show(htmlNm context++"_"++htmlNm pat), name pat)
                               | pat<-patterns context]
                             )++
                           if null ms then "" else
                           "\n<P>\nRelations\n<BR />\n"++
                           htmlDropDown "Select Relation" ["onChange=\"SwitchPop(this.value)\""] (("value=\""++htmlname (name context)++"\"","---"):ms)++
                           "\n<P>\nConcepts\n<BR />\n"++
                           htmlDropDown
                            "SwitchConcept"
                            ["onChange=\"SwitchPat(this.value)\""]
                            ( ("value="++show(htmlNm context), "---")
                              : [("value="++show(htmlNm context++htmlNm c), name c)
                                | c<-(sort' name.concs.signatures) context]
                            )++
                           "\n<P>"++htmlAnchor ("ANAL"++htmlname (name context))  "Analysis" ["TARGET=concepts"]++"\n"
                          )
                       )
              where ms = [( "value="++show("REL_"++htmlname (name context++"_"++name m++name (source m)++name (target m)))
                          , name m++"["++name (source m)++"*"++name (target m)++"]"
                          )
                         | m<-sord' name (signatures (rules context)++signatures context)]
                    thisCtx = htmlNm context
           navCodeVp context cPat
            = htmlPage ("Code for "++name context++" concepts navigator")
                       ("<SCRIPT type=\"text/javascript\">\nfunction SwitchPat(value)\n { parent.concepts.document.location.href='"++thisCtx++"'+value+'.html'\n ; parent.menu.document.location.href='NAV_"++thisCtx++"'+value+'.html'\n }\nfunction SwitchView(value)\n { parent.concepts.document.location.href='"++thisCtx++"'+value+'.html' }\n"++(if null ms then "" else "function SwitchPop(value)\n { parent.concepts.document.location.href=value+'.html' }\n")++"</SCRIPT>")
                       (htmlBody
                          ("Patterns\n<BR />\n"++
                           htmlDropDown
                             "SwitchPattern"
                             ["onChange=\"SwitchPat(this.value)\""]
                             ( ("value="++show(""), "All patterns"):
                               [("value="++show("_"++htmlNm pat), name pat)
                               | pat<-patterns context]
                             )++
                           if null ms then "" else
                           "\n<P>\nRelations\n<BR />\n"++
                           htmlDropDown "Select Relation" ["onChange=\"SwitchPop(this.value)\""] (("value=\""++htmlname (name context)++"_"++thisPat++"\"","---"):ms)++
                           "\n<P>\nConcepts\n<BR />\n"++
                           htmlDropDown
                            "SwitchConcept"
                            ["onChange=\"SwitchView(this.value)\" onClick=\"SwitchView(this.value)\""]
                            ( ("value="++show(""), "---")
                              : [("value="++show (htmlNm c)++
                                  if thisPat==htmlNm c then " selected" else "", name c)
                                | c<-(sort' name.concs.signatures) context]
                            )++
                           "\n<P>\n"++htmlAnchor ("ANAL"++htmlname (name context))  "Analysis" ["TARGET=concepts"]++"\n"
                          )
                       )
              where ms = [( "value="++show("REL_"++htmlname (name context++"_"++name m++name (source m)++name (target m)))
                          , name m++"["++name (source m)++"*"++name (target m)++"]"
                          )
                         | m<-sord' name (signatures (rules cPat)++signatures cPat)]
                    thisCtx = htmlNm context
                    thisPat = htmlNm cPat
           navCodePat :: Context -> Pattern -> String
           navCodePat context cPat
            = htmlPage ("Code for "++name context++" concepts navigator")
                       ("<SCRIPT type=\"text/javascript\">\nfunction SwitchPat(value)\n { parent.concepts.document.location.href='"++thisCtx++"'+value+'.html'\n ; parent.menu.document.location.href='NAV_"++thisCtx++"'+value+'.html'\n }\nfunction SwitchView(value)\n { parent.concepts.document.location.href='"++thisCtx++"'+value+'.html' }\n"++(if null ms then "" else "function SwitchPop(value)\n { parent.concepts.document.location.href=value+'.html' }\n")++"</SCRIPT>")
                       (htmlBody
                          ("Patterns\n<BR />\n"++
                           htmlDropDown
                             "SwitchPattern"
                             ["onChange=\"SwitchPat(this.value)\""]
                             ( ("value="++show(""), "All patterns"):
                               [("value="++show("_"++htmlNm p)++
                                  if thisPat==htmlNm p then " selected" else "",name p)
                               | p<-patterns context]
                             ) ++
                           if null ms then "" else
                           "\n<P>\nRelations\n<BR />\n"++
                           htmlDropDown "Select Relation" ["onChange=\"SwitchPop(this.value)\""] (("value=\""++htmlname (name context)++"_"++thisPat++"\"","---"):ms)++
                           "\n<P>\nConcepts\n<BR />\n"++
                           htmlDropDown
                            "SwitchConcept"
                            ["onChange=\"SwitchView(this.value)\" onClick=\"SwitchView(this.value)\""]
                            ( ("value="++show("_"++thisPat),"---")
                              : [("value="++show(htmlNm c),name c)
                                | c<-(sort' name.concs.signatures.grules) cPat]
                            )++
                           "\n<P>\n"++htmlAnchor ("ANAL"++htmlname (name context))  "Analysis" ["TARGET=concepts"]++"\n"
                          )
                       )  
              where ms = [( "value="++show("REL_"++htmlname (name context++"_"++name m++name (source m)++name (target m)))
                          , name m++"["++name (source m)++"*"++name (target m)++"]"
                          )
                         | m<-sord' name (signatures (rules cPat)++signatures cPat)]
                    thisCtx = htmlNm context
                    thisPat = htmlNm cPat
           navCodePop :: Context -> Pattern -> Concept -> String
           navCodePop context cPat c
            = htmlPage ("Code for "++name context++" concepts navigator")
                       ("<SCRIPT type=\"text/javascript\">\nfunction SwitchPat(value)\n { parent.concepts.document.location.href=value+'.html'\n ; parent.menu.document.location.href='NAV_'+value+'.html'\n }\nfunction SwitchView(value)\n { parent.concepts.document.location.href=value+'.html' }\n"++(if null ms then "" else "function SwitchPop(value)\n { parent.concepts.document.location.href=value+'.html'}\n")++"</SCRIPT>")
                       (htmlBody
                          ("Patterns\n<BR />\n"++
                           htmlDropDown
                             "SwitchPattern"
                             ["onChange=\"SwitchPat(this.value)\""]
                             ( ("value="++show(htmlNm context), "All patterns"):
                               [("value="++show(htmlNm context++"_"++htmlNm p)++
                                  if name cPat==name p then " selected" else "",name p)
                               | p<-patterns context]
                             )++
                           if null ms then "" else
                           "\n<P>\nRelations\n<BR />\n"++
                           htmlDropDown "Select Relation" ["onChange=\"SwitchPop(this.value)\""] ms++
                           "\n<P>\nConcepts\n<BR />\n"++
                           htmlDropDown
                            "SwitchConcept"
                            ["onChange=\"SwitchPat(this.value)\" onClick=\"SwitchView(this.value)\""]
                            [("value="++show(htmlNm context++"_"++htmlNm cPat++htmlNm c')++
                              if name c==name c' then " selected" else "",name c')
                            | c'<-(sort' name.concs.signatures.grules) cPat]
                          )++
                          "\n<P>\n"++htmlAnchor ("ANAL"++htmlname (name context))  "Analysis" ["TARGET=concepts"]++"\n"
                       )
              where ms = [( "value="++show("REL_"++htmlname (name context++"_"++name m++name (source m)++name (target m)))
                          , name m++"["++name (source m)++"*"++name (target m)++"]"
                          )
                         | m<-(sord' name.signatures) [r|r<-rules context, c `elem` concs r]]
  genAnalysis context predLogic
   = sequence_ (map (genRel context) (signatures context))>>
     writeFile ("ANAL"++htmlname (name context))
               (htmlPage ("Analysis of "++name context) ""
                         ( htmlBody (htmlHeadinglevel 1 ("Analysis of "++name context) []++"\n"++
                                     htmlSignatures context (signatures context)++"\n<P>\n"++
                                     if null violations then "" else
                                     htmlHeadinglevel 2 ("Violations found in "++name context) []++"\n"++
                                     violations
               )         )          )
   where
     violations = (concat . map hv . rules) context++if null viol then "" else "\n<P>\n"++viol

--      tabulate (Fu fs) = (rd.concat.map tabulate) fs
--      tabulate (Fi fs) = (foldr1 isc.map tabulate) fs
--                         where   xs `isc` ys = [y| y<-ys, head y `elem` map head xs && last y `elem` map last xs]
--      tabulate (F [Tc f _ _]   ) = tabulate f
--      tabulate (F  [t]         ) = [[x,name t,y]    |[x,y]<-contents t]
--      tabulate (F (Tc f _ _:ts)) = [t         ++rest|t<-tabulate f,    rest<-tabulate (F ts)]
--      tabulate (F (t:ts)       ) = [[x,name t]++rest|[x,y]<-contents t,rest<-tabulate (F ts)]

     hv r = if null ruleviol then "" else
            htmlAnchor (htmlNm context++"_Rule_"++show (nr r)++".html") ("Rule") []++
            ": \'"++htmlBold (explain r)++"\' "++
            " is violated in the following cases:\n<BR />"++
            ruleviol++"<BR />\n"
            where ruleviol = htmlViolations r
     viol = chain "\n\n"
              [ "Relation "++
                htmlAnchor (show("REL_"++htmlname (name context++"_"++name s++name (source s)++name (target s))++".html"))
                 (name s) []++" :: "++htmlAnchor (htmlNm context++htmlname a++".html") a []++" * "++
                                                      htmlAnchor (htmlNm context++htmlname a++".html") b []++
                 " yields "++show (length mp)++" violation"++(if length mp>1 then "s" else "")++",<BR />\nbecause "++
                sentence s a b p++htmlBlueTable "red"
                                  [htmlAnchor (htmlNm context++htmlname a++".html")
                                              ("<FONT COLOR=white>"++a++"</FONT>") []
                                  ,htmlAnchor (htmlNm context++htmlname b++".html")
                                              ("<FONT COLOR=white>"++b++"</FONT>") []
                                  ] mp
              | s<-signatures context, a<-[name (source s)], b<-[name (target s)]
              , p<-[Uni,Tot,Sur,Inj], p `elem` multiplicities s, mp<-[multViolations s p], not (null mp)]
     sentence s a b Uni = if a==b
                          then "each "++a++" in the left hand column may not have more than one "++b++" on the right. (This relation is univalent)"
                          else "each "++a++" may not have more than one "++b++". (This relation is univalent)"
     sentence s a b Tot = "there is a missing "++b++" for each of the following "++plural a++". (This relation is total)"
     sentence s a b Sur = "there is a missing "++a++" for each of the following "++plural b++". (This relation is surjective)"
     sentence s a b Inj = if a==b
                          then "each "++b++" in the right hand column may not have more than one "++b++" to its left. (This relation is injective)"
                          else "each "++b++" may not have more than one "++b++". (This relation is injective)"
     
  genRel context s
       | otherwise = writeFile ("REL_"++htmlname (name context++"_"++name s++a++b)++".html")
                      (htmlPage (name s++" :: "++a++" * "++b) ""
                                ( htmlBody (htmlHeadinglevel 2 ("Relation: "++name s++" :: "++
                                                                htmlAnchor (htmlNm context++htmlname a++".html") a []++" * "++
                                                                htmlAnchor (htmlNm context++htmlname a++".html") b []) []++
                                            nijssenZin s++
                                            (if null cs then "This relation is empty." else tabl) ++ viol
                      )         )          )>>
                     putStr ("\nHTML code for REL_"++htmlname (name context++"_"++name s++a++b)++".html written")
       where a  = name (source s)
             b  = name (target s)
             cs = contents s
             nijssenZin (Sgn _ _ _ _ [] [] [] _ _) = "No natural language meaning is assigned to this relation.<P>"
             nijssenZin (Sgn _ _ _ _ l s r [] _)
              = "A tuple <I>x</I>,<I>y</> in the following table means:<BR />"++l++"&lt;x&gt;"++s++"&lt;y&gt;"++r++".<P>"
             nijssenZin (Sgn _ _ _ _ l s r (c:cs) _)
              = "A tuple, for example <I>("++head c++","++last c++")</I> in the following table means:<BR />"++l++head c++s++last c++r++".<P>"
             tabl = "\nThe following table displays the contents of this relation."++
                    htmlBlueTable "darkred" [a,b] cs++"\n\n"
             viol = chain "\n\n"
                      [if null mp then "" 
                       else "\n<P>\n"++
                            (if length mp>1 then "There are "++show (length mp)++" violations, " else
                             "There is a violation, ") ++
                            sent p++htmlBlueTable "red" [a,b] mp
                      |p<-[Uni,Tot,Sur,Inj], p `elem` multiplicities s, mp<-[multViolations s p]]
             sent Uni = "because each "++a++" in the left hand column may not have more than one "++b++" on the right. (The relation "++htmlItalic (name s)++" is univalent.)"
             sent Tot = "because there is a missing "++b++" for each of the following "++plural a++". (The relation "++htmlItalic (name s)++" is total.)"
             sent Sur = "because there is a missing "++a++" for each of the following "++plural b++". (The relation "++htmlItalic (name s)++" is surjective.)"
             sent Inj = "because each "++b++" in the right hand column may not have more than one "++a++" to its left. (The relation "++htmlItalic (name s)++" is injective.)"
  noun Uni  = "Univalence"
  noun Tot  = "Totality"
  noun Sur  = "Surjectivity"
  noun Inj  = "Injectivity"

  xmlContexts contexts contextname
      = "<CTXS>"++concat [ctxInfo c|c<-contexts]++"\n</CTXS>\n"++
        "\n<PATS>"++concat [ "\n <PAT NAME=\""++name pat++"\" GRPH=\""++htmlname (nm++"_"++name pat)++".xml\">"++
                             "\n </PAT>"
                           | Ctx nm on isa world dc ms cs<-contexts, pat<-dc]++
        "\n</PATS>"
      where
       ctxInfo (Ctx nm on isa world dc ms cs)
        = "\n <CTX NAME=\"" ++ nm ++ "\">" ++
          "\n  <GEN><OL>" ++  concat["<LI NAME=\""++o++"\" />"|(n,o)<-cTuples,n==nm] ++ "</OL></GEN>" ++
          "\n  <SPC><OL>" ++  concat["<LI NAME=\""++n++"\" />"|(n,o)<-cTuples,o==nm] ++ "</OL></SPC>" ++
          concat ["\n  <PAT NAME=\"" ++ name pat ++ "\" GRPH=\"" ++ htmlname (nm ++ "_" ++ name pat) ++ ".xml\" />" | pat<-dc] ++
          concat [ "\n  <REL ID=\""++htmlname (nm++"_"++nm'++name a++name b)++"\" NAME=\""++name m++"\" HREF=\"REL_"++htmlname (nm++"_"++nm'++name a++name b)++".html\" />"
                 | m@(Sgn nm' a b props prL prM prR cs pos)<-ms, not (null cs)]++
          "\n </CTX>"
       cTuples = [(nm,o)| Ctx nm on _ _ _ _ _<-contexts, o<-on]
       tree str shw indent (Cl r [])
        = ind++"<"++str++" NAME=\""++name r++"\""++if null (shw ind r)
                                                   then " />"
                                                   else ">"++shw ind r++ind++"</"++str++">"
          where ind = indent++" "
       tree str shw indent (Cl r cls)
        = ind++"<"++str++" NAME=\""++name r++"\">"++shw ind r++
          concat [tree str shw ind c|c<-cls]++
          ind++"</"++str++">"
          where ind = indent++" "
       cShw indent (C _ _ _) = ""

  loadcode = "<BODY onload=\"MTMStartMenu()\" bgcolor=#FFFFBB link=#AA0000 alink=#AA0000 hlink=#AA0000></BODY>"
  introtext
      = "Use the classification of concepts (on your left) to browse."
  leader
      = "<SCRIPT type=\"text/javascript\">\n"++
        "  if((navigator.appName == \"Netscape\" && parseInt(navigator.appVersion) >= 3 && navigator.userAgent.indexOf(\"Opera\") == -1 && navigator.userAgent.indexOf(\"WebTV\") == -1) || (navigator.appName == \"Microsoft Internet Explorer\" && parseInt(navigator.appVersion) >= 4)) {\n"++
        "    for(i = 0; i < parent.frames.length; i++) {\n"++
        "      if(parent.frames[i].name == \"code\" && parent.frames[i].MTMLoaded) {\n"++
        "        parent.frames[i].MTMTrack = true;\n"++
        "        setTimeout(\"parent.frames[\" + i + \"].MTMDisplayMenu()\", 250);\n"++
        "        break;\n"++
        "      }\n"++
        "    }\n"++
        "  }\n"++
        "</SCRIPT>"
  indexcode filename
      = chain "\n"
        [ "<HTML>"
        , htmlHead ("Concepts") ""
        , "<SCRIPT TYPE=\"text/javascript\">"
        , "var MTMUsableBrowser = false;"
        , "// browser sniffing routine"
        , "browserName = navigator.appName;"
        , "browserVersion = parseInt(navigator.appVersion);"
        , "if(browserName == \"Netscape\" && browserVersion >= 3) {"
        , "  MTMUsableBrowser = (navigator.userAgent.indexOf(\"Opera\") == -1) ? true : false;"
        , "} else if(browserName == \"Microsoft Internet Explorer\" && browserVersion >= 4) {"
        , "  MTMUsableBrowser = true;"
        , "};"
        , "if(!MTMUsableBrowser) alert('This page was not designed for your browser. Please use Netscape or Internet Explorer if you get inexplicable behaviour');"
        , "document.write('<FRAMESET Cols=\"1,210,*\" border=0 frameborder=\"no\" framespacing=\"0\">');"
        , "document.write('  <FRAME Name=\"code\" SRC=\"CTX_"++filename++"\" NORESIZE FRAMEBORDER=\"No\">');"
        , "document.write('  <FRAMESET Rows=\"150,*\" border=0 frameborder=\"no\" framespacing=\"0\">');"
        , "document.write('    <FRAME Name=\"context\" SRC=\"treemenutils/conceptsmenu_empty.html\" NORESIZE FRAMEBORDER=\"No\">');"
        , "document.write('    <FRAME Name=\"menu\" SRC=\"NAV_"++filename++"\" NORESIZE FRAMEBORDER=\"No\">');"
        , "document.write('  </FRAMESET>');"
        , "document.write('  <FRAME Name=\"concepts\" SRC=\"Concept.html\" NORESIZE FRAMEBORDER=\"No\">');"
        , "document.write('</FRAMESET>');"
        , "</SCRIPT>"
        , htmlBody "Your browser does not support frames."
        , "</HTML>"]

-- The following function makes a HTML page for a complete pattern.

  htmlRule :: Context -> Rule -> Bool -> String
  htmlRule context@(Ctx nm on isa world dc ms cs) r predLogic
   = htmlPage ("Code for Rule "++show (nr r)) ""
                   (htmlBody ("<A HREF=#REF2Glossary>Glossary</A> <A HREF=#REF2Relations>Relations</A>\n"++
                              (if length (rules context) <=1 then "" else
                               "<A HREF=\""++htmlNm context++"_Rule_"++show (if nr r==length (rules context) then 1 else nr r+1)++".html\">Next rule</A>\n"++
                               "<A HREF=\""++htmlNm context++"_Rule_"++show (if nr r==1 then length (rules context) else nr r-1)++".html\">Previous rule</A>\n")++
                              htmlHeadinglevel 3 ("Rule "++show (nr r)++": "++explain r) []++"\n<P>\n"++
                              (if predLogic
                               then "ADL representation<BR />\n<BLOCKQUOTE>"++showADL r++"</BLOCKQUOTE>\n<P>\n"++
                                    "Predicate logic representation<BR />\n<BLOCKQUOTE>"++
                                    (hshow.assemble.normRule lub) r++"</BLOCKQUOTE>\n<P>\n"
                               else "("++showADL r++")\n<P>\n") ++
                              (if testing then "Test\n<P>\n"++showHS r++"\n<P>\n"++show r++"\n<P>\n" else "")++
                              htmlImage (htmlNm context++"_Rule_"++show (nr r)++".png")++"\n<P>\n"++
                              (if null ruleviol then "" else
                               htmlHeadinglevel 2 ("Violations in rule "++show (nr r)) []++"\n"++ruleviol++"\n<P>\n"++
                               (if length (violations r)>1 then "Analysis of the first violation of this rule:<BR />\n" else "")++
                               (if null antcStrands
                                then (if length consStrands ==1
                                      then htmlItalic "Since "++commaEng (htmlItalic "and") [x,y]++htmlItalic " are linked through  "++showADL cons++htmlItalic ", rule "
                                      else commaEng (htmlItalic "and") [x,y]++htmlItalic " are linked through "++showADL antc++htmlItalic " as follows."++"<BR />\n"++htmlTable antcStrands []++"\n"++htmlItalic "Rule "
                                     )++htmlItalic (show (nr r)++" prescribes that ")++commaEng (htmlItalic "and") [x,y]++htmlItalic " must be linked through "++showADL antc++". \n"++htmlItalic "However, this is not the case.\n"++
                                     (if nullN antcNostrds then "" else
                                      htmlItalic "Here is how far we got with the available population."++"<BR />"++
                                      htmlTable antcNostrds [])
                                else (if length antcStrands ==1
                                      then htmlItalic "Since "++commaEng (htmlItalic "and") [x,y]++htmlItalic " are linked through "++showADL antc++htmlItalic ", rule "
                                      else commaEng (htmlItalic "and") [x,y]++htmlItalic " are linked through "++showADL cons++htmlItalic " as follows."++"<BR />\n"++htmlTable antcStrands []++"\n"++htmlItalic "Rule "
                                     )++htmlItalic (show (nr r)++" prescribes that ")++commaEng (htmlItalic "and") [x,y]++htmlItalic " must be linked through "++showADL cons++". \n"++htmlItalic "However, this is not the case.\n"++
                                     (if nullN consNostrds then "" else
                                      htmlItalic "Here is how far we got with the available population."++"<BR />"++
                                      htmlTable consNostrds [])
                               )++"\n"
                              )++
                              htmlHeadinglevel 2 "Relations" []++"\n"++
                              htmlSignatures context (signatures r)++"\n<P>\n"++
                              (if predLogic
                               then if null (irs++drs) then "" else
                                    htmlHeadinglevel 2 "Derived rules" []++"\n"++
                                    htmlTable ([[show (nr r)++[i,' '],(hshow.assemble) r]| (i,r)<-zip ['a'..'z'] irs]++[["","---"]]++
                                               [[show (nr r)++[i,' '],(hshow.assemble) r]| (i,r)<-zip (drop (length irs) ['a'..'z']) drs]) []++"\n<P>\n"
                               else ""
                              )++
                              htmlGlossary context (Pat ("Rule "++show (nr r)) [r] [] [] [])
                   ))
   where ruleviol    = htmlViolations r
         [x,y]       = head (violations r)
         irs         = insertRules [r]
         drs         = deleteRules [r]
         antcStrands = strands (C a gE [x],C b gE [y]) antc
         consStrands = strands (C a gE [x],C b gE [y]) cons
         antcNostrds = nostrds (C a gE [x],C b gE [y]) antc
         consNostrds = nostrds (C a gE [x],C b gE [y]) cons
         nullN (x:xs)= length [e| e<-x, not (null e)]>1
         gE          = genE r
         Ru c antc pos cons expla (C a _ _,C b _ _) n = r
  htmlViolations :: Rule -> String
  htmlViolations r@(Ru c antc pos cons expla (a,b) n)
   = if null vs then "" else -- No violations detected within this rule.\n
     htmlBlueTable "red" [show a,show b] vs++"\n\n"
     where vs = violations r
  violations :: Rule -> [Link]
  violations r@(Ru c antc pos cons expla (a,b) n)
    = [l| l<-contents antc, not (l `elem` contents cons)]++
      if c=='E' then [l| l<-contents cons, not (l `elem` contents antc)] else []

  htmlSignatures :: Context -> [Signature] -> String
  htmlSignatures context ss 
   = htmlTable ((["","",""]++map show pTitle):[row s| s<-ss]) "BORDER=1 CELLSPACING=0"
     where
      row s
       = [ if null (contents s) then name s else
           htmlAnchor (show("REL_"++htmlname (name context++"_"++name s++name (source s)++name (target s))++".html"))
                      (name s) []
         ,"::"
         , "["++htmlAnchor (htmlname (name context++name (source s))++".html") (name (source s)) []++
           "*"++htmlAnchor (htmlname (name context++name (target s))++".html") (name (target s)) []++
           "]"
         ] ++
         [if p `elem` multiplicities s
          then (if null (multViolations s p) then "R"
                else htmlAnchor (show("REL_"++htmlname (name context++"_"++name s++name (source s)++name (target s))++".html#REF2"++show len++" violation"++(if len==1 then "" else "s")++" of "++noun p))
                                (show len) [])
          else "-"
         | p<-[Inj,Sur,Uni,Tot], len<-[length (multViolations s p)]] ++
         [if p `elem` multiplicities s then "R" else "-"| homogeneous s, p<-[Rfx,Trn,Sym,Asy]]
      pTitle = [Inj,Sur,Uni,Tot]++if or (map homogeneous ss) then [Rfx,Trn,Sym,Asy] else []
      homogeneous s = source s == target s
  multViolations :: Signature -> Prop -> Links
  multViolations s Uni = [rec| cl<-eqCl head (contents s), length cl>1, rec<-cl]
  multViolations s Inj = [rec| cl<-eqCl last (contents s), length cl>1, rec<-cl]
  multViolations s Tot = [[e,""]| e<-rd (conts (source s))>-dom s]
  multViolations s Sur = [["",e]| e<-rd (conts (target s))>-cod s]

  htmlPattern :: Context -> String -> Pattern -> String
  htmlPattern context fnm pat@(Pat nm rs parChds pms cs)
   = htmlPage ("Code for "++nm) ""
                   (htmlBody (htmlHeadinglevel 1 ("Pattern: "++nm) []++"\n"++
                              htmlValNumbered [(nr r,explainverder (htmlNm context) r) | r<-rules pat]++"\n"++
                              htmlImage (fnm++".png")++"\n"++htmlGlossary context pat))
  htmlContext :: String -> Context -> String
  htmlContext fnm context
   = htmlPage ("Code for "++name context) ("<SCRIPT type=\"text/javascript\">\nparent.menu.document.location.href='NAV_"++fnm++".html'\n</SCRIPT>")
                   (htmlBody (htmlValNumbered [(nr r,explainverder (htmlNm context) r) | r<-rules context]++"\n"++htmlImage (fnm++".png")))

--      where Pat nm rs parChds pms cs = foldr1 union (patterns context)

  htmlGlossary context@(Ctx cnm on isa world dc ms cds) pat
   = htmlHeadinglevel 2 "Glossary" []++
     htmlTable [[c,cdef]| Cd _ c cdef _<-cds, C c (==) [] `elem` concs pat] ""

-- The following function makes a HTML page for one particular concept c, interpreted in the context of world.
-- This page is mounted in the contents frame of the architecture page, to which the navigator (left hand side of 
-- the screen) points.

--  Pre: c<-concs (signatures context)

  viewpoint :: Context -> Concept -> Pattern
  viewpoint context c
   = Pat (name c)
         rulesV
         (clearG [G g s| Isa ts ss<-[isa context], (g,s)<-ts, g `elem` concsV, s `elem` concsV])
         (signatures rulesV)
         [c| c@(Cd pos nm def ref)<-conceptdefs context, C nm (==) [] `elem` concsV]
     where rulesV = [r| r<-rules context, c `elem` concs r]
           concsV = concs rulesV

  inhViewpoint :: Classification Context -> Concept -> Concept -> Pattern
  inhViewpoint (Cl context world) specific gen
   = Pat ("Concept "++name specific++" inherited from "++name gen)
         (rs++[s| s<-sc, Isa ts ss<-[isa s], and[b `elem` concs rs| (a,b)<-ts]])
         (clearG [G g s| Isa ts ss<-[isa context], (g,s)<-ts, g==gen, s `elem` concs rs])
         (signatures rs)
         [c| c@(Cd pos nm def ref)<-conceptdefs context, C nm (==) [] `elem` rd [c|r<-rs, c<-concs r]]
     where
       rs     = rd [sr | r<-rules world, gen `elem` concs r, s<-sc
                       ,sr<-[subsR s r], specific `elem` concs sr]
       sc     = [s| s<-specs context, gen `elem` concs s]
--     rulesG = rd [ subsC s r | r<-rules world, gen `elem` concs r, s<-substns r]
--     substns r
--      = (foldr cp [[]].map (map single).eqCl fst.clear)
--          [(g,s)| c<-concs r, Isa ts ss<-[isa context], (g,s)<-ts, c==g]
--        where cp as bs = [a++b|a<-as,b<-bs] ; single x = [x]

  htmlViewpoint :: Context -> String -> Pattern -> Concept -> Classification Concept -> Bool -> String
  htmlViewpoint context@(Ctx cnm on isa world dc ms cds) fnm pat c (Cl r cls) predLogic
   = (htmlPage (name c) "" . htmlBody)
     (vptTitle (name c)                                                                            ++

-- Traceability (removed for now)
--       "Properties of concept "++name c++" in context "++thisCtx                                     ++
--       (if c==r then "" else " emerging from properties of "++name r)                                ++
--       (if null cls then "" else
--         "\n<BR /inheriting properties from "++
--         commaAnd [htmlAnchor (thisCtx++name c++name g++".html") (name g) []|Cl g cls'<-cls]++".")   ++

     concat ["\n<P>\n"++htmlBold "Definition"++"\n<BR />\n"++cdef| Cd _ c' cdef _<-cds, c'==name c] ++
      (if null atoms then "" else if length atoms==1
       then "\n<P>\n"++htmlBold ("This concept contains one atom: "++show (head atoms))
       else "\n<P>\n"++htmlBold ("This concept contains "++show (length atoms)++" atoms.")++"<BR />\n"++
            htmlTable [[a]| a<-atoms] [])  ++
      "\n<FONT color=\"#AA0000\"><HR color=#AA0000></FONT>\n"++tests                               ++
      htmlHeadinglevel 2 ("Rules applicable to "++show c) []++"\n"++
      htmlValNumbered (map (hgenR (htmlNm context) predLogic) rs)++htmlImage (fnm++".png"))        ++
     (if predLogic
      then (if null (irs++drs) then "" else
           "\n<P>\n"++htmlHeadinglevel 2 "Derivable Functionality" []++
           chain "\n<P>\n"
             [dbShow cl|cl<-eqCl frMorph (dbIns rs++dbDel rs)
                       , r@(DBR _ _ m _)<-take 1 cl, c==source m || c==target m  ])
      else "")
     where
       rs = rules pat++rules (specs pat)
       irs = insertRules rs
       drs = deleteRules rs
       C nm gE atoms = c
       thisCtx = htmlNm context
       gen = name r
--       inheriting =  [G g s|G g s<-parChds, s==c]
       tests
        = if testing
          then "Testing:"++
               "<BR />cs = "++show (concs pat)++
               "<BR />rs = "++show (rules pat)
          else ""

  explainverder thisCtx r = explain r++" "++htmlAnchor (thisCtx++"_Rule_"++show (nr r)++".html") "More..." []
  hgenR thisCtx predLogic r
   = (nr r, ruleviol (nr r)++(if predLogic then (hshow.assemble.normRule lub) r++"\n<BR />\n" else "")++explainverder thisCtx r++"\n")
     where
      ruleviol :: Int -> String
      ruleviol i
       | i `elem` violHtml = "Click "++htmlAnchor ("Atlas"++show i++"Viol.html") "here" []++" to see violations.\n<BR />\n"
       | otherwise         = ""

  vptTitle c = htmlHeadinglevel 2 ("Concept: "++c) []++"\n"

{-  htmlConcept :: String -> Classification Context -> [Concept] -> Classification Concept -> Bool -> (String,String)
  htmlConcept fnm (Cl context world) trace t@(Cl c cls) predLogic
   | c `elem` concG
      = (vptTitle c'++"Properties of "++name c'++" emerging from properties of "++name c++
         ".\n<FONT color=\"#AA0000\"><HR color=#AA0000></FONT>\n"++tests++
         htmlHeadinglevel 2 ("Rules applicable to "++show c) []++"\n"++
         htmlValNumbered (map (hgenR (htmlNm context) predLogic) [r|r<-ruleG, c' `elem` concs r])++
         htmlImage (fnm++".png")
        ,graph (name c') ruleG)
   | c `elem` concS
      = (vptTitle c++"Properties of concept "++name c++" in context "++name context++
         (if null cls then "" else
          "<BR />inheriting properties from "++
          commaAnd [htmlAnchor (name context++(if null trace then name c else name (head trace))++name e++".html") (name e) []|e<-cls]++".")++
         "\n<FONT color=\"#AA0000\"><HR color=#AA0000></FONT>\n"++tests++
         htmlHeadinglevel 2 ("Rules applicable to "++show c) []++"\n"++
         htmlValNumbered (map (hgenR (htmlNm context) predLogic) ruleS)++htmlImage (fnm++".png")
        ,graph (name c) ruleS)
   | otherwise = ("Empty concept page"++
                  if testing then "\n<BR />\nconcS = "++show (map name concS)++"\n<BR />\nconcG = "++show (map name concG) else ""
                 ,dotGraph (name c) "1.0" relationList (Id posNone [] c))
   where
       c'    = if null trace then c else head trace
       concS = concs context
       ruleS = [r| r<-rules context, c `elem` concs r]
       concG = concs world
--     ruleG = rd [ subsC s r | r<-rules world, c `elem` concs r, s<-substns r]
       ruleG = rd [ foldr (.) id [subsR s| s<-sc] r | r<-rules world] where sc=specs context
       graph nm rs = dotGraph nm "1.0" relationList (Pat (name c) rs [] [] [] [])
       trs = if null trc then "concept "++name c else
             (if length trc>1 then "concepts " else "concept ")++chain "," trc
             where trc = [name c|c<-trace `isc` concG]
--     substns r = (foldr cp [[]].map (map single).eqCl fst.clear) [(g,c)| g<-concs r, c<-concS, g `gEq` c]
--                 where cp as bs = [a++b|a<-as,b<-bs] ; single x = [x]
--     subst tuples c = head ([c'| (C p _ _,C c' _ _)<-tuples, p==c]++[c])
       vptTitle c = "Concept: "++(name c)++"\n<P>\n"
       ruleviol :: Int -> String
       ruleviol i
        | i `elem` violHtml = "Click "++htmlAnchor ("Atlas"++show i++"Viol.html") "here" []++" to see violations.\n<BR />\n"
        | otherwise         = ""
       tests
        = if testing
          then "Testing:<BR />concS = "++show concS++
               "<BR />concG = "++show concG++
               "<BR />ruleS = "++show ruleS++
               "<BR />ruleG = "++show ruleG++
               "<BR />rules world = "++show (rules world)++
               "<BR />specs context = "++show (specs context)
          else ""-}

-- test:  recalc context@(Ctx nm on isa world dc ms cs) = Ctx (error (testC++"\n\n"++testD)) on isa world dc ms cs

  recalc :: Context -> Context
  recalc context = -- Testing: if error (chain "\n------------\n" [testA, testB, testC, testD]) then context else
               update (foldr subst (signatures context) calcOrder) context
   where
-- The function sweep removes all leaves from the tree.
    sweep (Cl (s,r) cls) = Cl (s,head [r|Cl (s,r) _<-cls]) [sweep cl| cl@(Cl x xs)<-cls, not (null xs)]
-- Select Horn Clauses from rules context
    calcrules = [r | r@(Ru 'I' antc pos cons@(F [t]) expla (a,b) nr)
                      <-insertRules (rules context)]
    testA = chain "\n\n" (map showADL calcrules)
-- Construct the dependency graph:
    rs  = [ ((s,r),(s',r))
          | r@(Ru _ antc pos cons expla (a,b) nr)<-calcrules
          , s<-rd (signatures cons), s'<-rd (signatures antc)]
    testB = htmlTable [[name s,showADL r,name s', showADL r']|((s,r),(s',r'))<-rs] []
-- Construct the calculation order:
    cls  = map sweep (makeClassificationsF fst rs)
    testC = chain "\n\n" (map (show.mapCl f) cls)
            where f (s,r) = name s ++ "  -->  " ++ showADL r
    calcOrder = rs'++(calcrules>-rs') where rs' = (reverse . map snd . concat . map preCl) cls
    testD = chain "\n\n" (map showADL calcOrder)
-- Recompute the contents
    ss' = foldr subst (signatures context) calcOrder
    subst (Ru c antc pos cons@(F [Tm m _]) expla (a,b) nr) ss
     = [ if [s]==signatures cons then insert (calc antc ss) s else s | s<-ss ]
    subst (Ru c antc pos cons@(F [Tf m _]) expla (a,b) nr) ss
     = [ if [s]==signatures cons then insert (map reverse (calc antc ss)) s else s | s<-ss ]
    subst r ss = error ("Unexpected compute error in :"++showADL r)

  insert :: Links -> Signature -> Signature
  insert ls (Sgn nm a b props prL prM prR cs pos) = Sgn nm a b props prL prM prR (ls `uni` cs) pos
  insert ls id                                    = id


--   hsign (Sg a b) ps
--         | m Uni && m Tot && m Inj && m Sur = a++" <- "++b
--         | m Uni && m Tot = a++" -- "++b
--         | m Sur && m Inj = a++" <-- "++b
--         | m Uni          = a++" |- "++b
--         | m Inj          = a++" <-| "++b 
--         | otherwise      = "("++a++","++b++")"
--         where m e = e `elem` ps

  hmenu :: String -> Classification Concept -> String
  hmenu ctxname (Cl r cls)
    = chain "\n" (["\n//menu menu\n\nvar menu = null;\nmenu = new MTMenu();"] ++
          [menuline "menu" (htmlname (ctxname++htmlNm (root cl)++".html")) cl| cl<-cls] ++
          concat [recur "menu" (sh r) (show i) (Cl r cls) [r]| (i, Cl r cls)<-zip [0..] cls, not (null cls)])
      where
       recur nm rootname cNr (Cl r cls) trace
        = ["\n//"++rootname++" menu\n\nvar "++varname++" = null;\n"++varname++" = new MTMenu();"] ++
          [menuline varname (fnm (root cl)) cl| cl<-cls] ++
          [nm++".items["++cNr++"].MTMakeSubmenu("++varname++");"] ++
          concat [recur varname (sh r) (show i) (Cl r cls) (trace++[r])| (i, Cl r cls)<-zip [0..] cls, not (null cls)]
          where fnm r = htmlname (ctxname++name r++if null trace then "" else name (head trace))++".html"
                varname = avoidJSreservedwords rootname
       menuline rn fnm (Cl r cls)
        = rn++".MTMAddItem(new MTMenuItem(\""++htmlNm r++"\", \""++fnm++"\", \"concepts\"));"
       sh root = [if isUpper c then chr(ord c - ord 'A' + ord 'a') else c| c<-mumble (name root)]

  hmenu' :: Classification Context -> String
  hmenu' (Cl r cls)
    = chain "\n" (["\n//menu menu\n\nvar menu = null;\nmenu = new MTMenu();"] ++
                  [menuline "menu" (htmlNm r)]   ++
                  recur "menu" (sh r) "0" (Cl r cls) [])
      where
       recur nm rootname cNr (Cl r cls) trace
        = ["\n//"++rootname++" menu\n\nvar "++varname++" = null;\n"++varname++" = new MTMenu();"] ++
          [menuline varname (htmlNm r)| (Cl r cls')<-cls] ++
          [nm++".items["++cNr++"].MTMakeSubmenu("++varname++");"] ++
          concat [recur varname (sh r) (show i) (Cl r cls) (trace++[r])| (i, Cl r cls)<-zip [0..] cls, not (null cls)]
          where varname = avoidJSreservedwords rootname
       menuline rn fnm
        = rn++".MTMAddItem(new MTMenuItem(\""++fnm++"\", \""++fnm++".html"++"\", \"concepts\"));"
       sh root = [if isUpper c then chr(ord c - ord 'A' + ord 'a') else c| c<-mumble (name root)]

  wrapCode ctxName lines
   = chain "\n"
     ([ "<SCRIPT type=\"text/javascript\" src=\"treemenutils/mtmcode.js\"></SCRIPT>"
      , "<SCRIPT type=\"text/javascript\">"
      , "// Morten's JavaScript Tree Menu"
      , "// written by Morten Wang <morten@treemenu.com> (c) 1998-2000"
      , "// This is version 2.2.6, dated 2000-03-30"
      , "// The script is freely distributable"
      , "// It may be used (and modified) as you wish, but retain this message"
      , "// For more information about the menu visit its home page"
      , "// http://www.treemenu.com/"
      , "var MTMTableWidth = \"100%\";"
      , "var MTMenuFrame = \"context\";"
      , "var MTMSubsGetPlus = false;"
      , "var MTMEmulateWE = true;"
      , "var MTMenuImageDirectory = \"treemenutils/\";"
      , "var MTMBGColor = \"#FFFFBB\";"
      , "var MTMBackground = \"\";"
      , "var MTMTextColor = \"#000000\";"
      , "var MTMLinkColor = \"#AA0000\";"
      , "var MTMAhoverColor = \"#000000\";"
      , "var MTMTrackColor =\"#000000\";"
      , "var MTMSubExpandColor = \"#666699\";"
      , "var MTMSubClosedColor = \"#666699\";"
      , "var MTMRootIcon = \"menu_new_root.gif\";"
      , "var MTMenuText = \"Context tree\";"
      , "var MTMRootColor = \"#000000\";"
      , "var MTMRootFont = \"Arial, Helvetica, sans-serif\";"
      , "var MTMRootCSSize = \"84%\";"
      , "var MTMRootFontSize = \"-1\";"
      , "var MTMenuFont = \"Arial, Helvetica, sans-serif\";"
      , "var MTMenuCSSize = \"84%\";"
      , "var MTMenuFontSize = \"-1\";"
      , "var MTMLinkedSS = false;"
      , "var MTMSSHREF = \"menu.css\";"
      , "var MTMSubsAutoClose = false;"
      , "var MTMTimeOut = 15;"
      , "var MTMIconList = null;"
      , "MTMIconList = new IconList();"
      , "MTMIconList.addIcon(new MTMIcon(\"menu_link_external.gif\", \"http://\", \"pre\"));"
      , "MTMIconList.addIcon(new MTMIcon(\"menu_link_pdf.gif\", \".pdf\", \"post\"));"
      , lines
      , "</SCRIPT>"])

  charVars q vs
   = if null vs then "" else
     q++" "++chain "; " [chain ", " (map fst vs')++"::"++name (snd (head vs')) | vs'<-eqCl snd vs]++": "

  htmlNm :: Identified a => a->String
  htmlNm = htmlname.name
