>-- LATEX generator

>  module LATEXgen where

>  import Char
>  import Auxiliaries
>  import Classification
>  import Typology
>  import CC_aux
>  import Restrictions

>  data Language = Dutch | English deriving Eq
>  language = Dutch

>  class LATEX a where
>   lshow :: a -> String
>--   lshow = show
>   ltshow :: String -> Typology Concept -> a -> String
>   ltshow nm typ = lshow

>  instance LATEX Context where
>   lshow ctx = ltshow (name ctx) (typology (isa ctx)) ctx
>   ltshow cname typ ctx@(Ctx nm on isa gE world dc ms cs)
>    = (chain "\n". filter (not.null))
>      (if language==Dutch then 
>      [{- "\\title{Ontwerpstudie voor "++cname++"}"
>      , "\\maketitle"
>      , "\\tableofcontents"
>      , latexChapter "Inleiding" "Inleiding"
>      , latexSection ("Afbakening") ("scope"++cname)
>      , latexSubsection ("Identificatie") ("identification"++cname)
>      , latexSubsection ("Overzicht") ("overview"++cname)
>      , latexSubsection ("Documentatie") ("documentation"++cname)
>     -} latexChapter "Ontwerpkeuzes" "Ontwerpkeuzes"
>      , "Dit hoofdstuk bespreekt verschillende ontwerpkeuzes en is consistent met de uitwerking in hoofdstuk \\ref{chp:Deelstudies}."
>--      , "Wanneer hierover overeenstemming is tussen de belanghebbenden, dan is hoofdstuk \\ref{chp:Deelstudies} bruikbaar als definierend document in het ontwerp."
>      , chain "\n\n" [latexSection ("Ontwerpkeuzes over "++nm) ("Ontwerpkeuzes"++cname++"Pat"++nm) ++ chain "\n\n" [explain r|r<-rs]
>                     | Pat nm rs gen pms cs<-dc]
>      , latexChapter "Deelstudies" "Deelstudies"
>      , showPats dc []
>      , latexChapter "Terminologie" ("typology"++cname)
>--      , "De terminologie is ontleend aan het Divisie Informatieplan \\cite{TPDI},"
>--      , "de begrippenlijst voor \\mulF{} \\cite{MultiFit} en de begrippen gebruikt in de voorstudie SBD \\cite{SBD}."
>--      , "In geval van conflicten gaan begrippen uit \\cite{TPDI} v\\'o\\'or \\cite{MultiFit} en begrippen uit \\cite{MultiFit} v\\'o\\'or \\cite{SBD}."
>      , if null cs then "" else
>        latex "longtable" ["{|p{4cm}|p{10cm}|}\\hline"]
>             (chain "\n" ["\\textbf{"++nm++"} & "++def++
>                          (if null ref then "" else "~\\cite{"++ref++"}")++
>                          "\\\\\n\\hline"
>                         | Cd pos nm def ref<-conceptdefs ctx, C nm [] `elem` concs ctx])
>      , if null cList then "" else
>        if length cList==1 then "\tHet concept "++idName(head cList)++" heeft geen tekstuele definitie in sectie \\ref{typology"++cname++"}." else
>        "\tDe volgende concepten zijn niet opgenomen in de woordenlijst: "++commaEn (map idName (sord' name cList))++"."
>      , "\\bibliography{"++lname ctx++"}"
>      , "\\label{bibliography"++lname ctx++"}"
>      , "\\bibliographystyle{plain}"
>      ] else if language==English then
>      [{- "\\title{Design of "++cname++"}"
>      , "\\maketitle"
>      , "\\tableofcontents"
>      , latexChapter "Introduction" "Introduction"
>      , latexSection ("Scope") ("scope"++cname)
>     -} latexChapter "Principles" "Principles"
>      , "This chapter introduces guiding principles of "++cname++". These principles are consistent with the formal descriptions in chapter \\ref{chp:Detailed descriptions}."
>      , chain "\n\n" [latexSection ("Design choices about "++nm) ("DesignChoices"++cname++"Pat"++nm) ++ chain "\n\n" [explain r|r<-rs]
>                     | Pat nm rs gen pms cs<-dc]
>      , latexChapter "Detailed descriptions" "Detailed descriptions"
>      , showPats dc []
>      , latexChapter "Glossary" ("typology"++cname)
>      , if null cs then "" else
>        latex "longtable" ["{|p{4cm}|p{10cm}|}\\hline"] (chain "\n" ["\\textbf{"++nm++"} & "++def++"~\\cite{"++ref++"}\\\\\n\\hline"
>                          | Cd pos nm def ref<-conceptdefs ctx, C nm [] `elem` concs ctx])
>      , if null cList then "" else
>        if length cList==1 then "\tThe concept "++idName(head cList)++" has no textual definition in the glossary (section \\ref{typology"++cname++"})." else
>        "\tThe following concepts are not described in the glossary: "++commaAnd (map idName (sord' name cList))++"."
>      , "\\bibliography{"++lname ctx++"}"
>      , "\\label{bibliography"++lname ctx++"}"
>      , "\\bibliographystyle{plain}"
>      ] else [] )
>      where
>       cList = concs ctx>-rd [C nm []| Cd pos nm def ref<-conceptdefs ctx]
>--       nav :: Classification Concept
>--       nav  = sortCl before (Cl (C "Anything" []) (makeTrees typ))
>       mms  = morphisms ctx
>--       degree c = length [m | m<-mms, source m==c || target m==c]
>--       c `before` c' = degree c > degree c'
>--       caps :: Classification Concept -> [String]
>--       caps (Cl c cls) = ["\\disjn{"++chain ", " (map (lshow.root) cls)++"}"| length cls>1] ++ (concat . map capss) cls ++
>--                         concat (map caps cls)
>--       capss (Cl g cls) = ["\\super{"++lshow g++"}{"++lshow s++"}" | s<-map root cls]

>       showPats [] dejavu
>        = if null (mms>-dejavu) 
>          then (if language==English
>                then "All relations that are defined in this context are used in one or more rules."
>                else if language==Dutch
>                then "Alle relaties van deze context worden gebruikt in \'e\'en of meer regels."
>                else "")
>          else latexSection (if language==Dutch then "Ongebruikte relaties" else "Unused relations") ("unused in "++nm) ++
>               chain "\\\\\n" (map lshow (mms>-dejavu))
>       showPats (pat@(Pat nm rs gen pms cs):dc) dejavu
>        = if language==Dutch then
>           latexSection (nm) (nnm)         ++
>           latexFigure (latexCenter ("  \\includegraphics[scale=.3]{"++cname++"_"++nnm++".png}")++
>                        "\n\\caption{Structuur van "++nm++"}\n\\label{fig:"++nnm++"}")        ++ "\n" ++
>           (if null (rules pat) then "Geen regels voor "++nm++"." else
>            showRestrictions (rules pat) 1 dejavu                                          ++ "\n") ++
>           (if null (specs pat) then "" else
>            lschema cname typ (Pat (nm++" "++show (length (rules pat)+1))
>                               [Dc m posNone expr "" sgn nr| Gc pos m expr sgn nr<-specs pat] [] [] []) ++ "\n\n") ++
>           showPats dc (dejavu++morphisms (rules pat))
>          else if language==English then
>           latexSection (nm) (nnm)         ++
>           latexFigure (latexCenter ("  \\includegraphics[scale=.3]{"++cname++"_"++nnm++".png}")++
>                        "\n\\caption{Structure of "++nm++"}\n\\label{fig:"++nnm++"}")        ++ "\n" ++
>           (if null (rules pat) then "No rules for "++nm++"." else
>            showRestrictions (rules pat) 1 dejavu                                          ++ "\n") ++
>           (if null (specs pat) then "" else
>            lschema cname typ (Pat (nm++" "++show (length (rules pat)+1))
>                               [Dc m posNone expr "" sgn nr| Gc pos m expr sgn nr<-specs pat] [] [] []) ++ "\n\n") ++
>           showPats dc (dejavu++morphisms (rules pat))
>          else ""
>          where
>           nnm = clname nm
>           showRestrictions [] i dejavu = ""
>           showRestrictions (r:rs) i dejavu
>            = chain "\n\n" (map lshow ms) ++
>              "\n\\label{rel:"++nnm++" "++show i++"}\n" ++
>              lschema cname typ (Pat (nm++" "++show i) [normRule (lub (genEq typ)) r] [] [] []) ++
>              (if language==Dutch then
>                (if null refsDut then "" else "\nDe definitie "++ commaEn refsDut++".") ++ "\n\n" ++
>                showRestrictions rs (i+1) (dejavu++ms)
>               else if language==English then
>                (if null refsEng then "" else "\nThe definition "++ commaAnd refsEng++".") ++ "\n\n" ++
>                showRestrictions rs (i+1) (dejavu++ms)
>               else "")
>              where ms = sort' name (rd [m|m<-morphisms r, not (m `elem` dejavu)])
>                    refsDut = [ "van "++idName m++" staat in sectie \\ref{rel:"++lname m++lname (source m)++lname (target m)++"} (pg. \\pageref{rel:"++lname m++lname (source m)++lname (target m)++"})"
>                              | m<-morphisms r, m `elem` dejavu]
>                    refsEng = [ "of "++idName m++" is given in section \\ref{rel:"++lname m++lname (source m)++lname (target m)++"} (pg. \\pageref{rel:"++lname m++lname (source m)++lname (target m)++"})"
>                              | m<-morphisms r, m `elem` dejavu]

>  lglos ctx = ltglos (name ctx) (typology (isa ctx)) ctx
>  ltglos cname typ ctx@(Ctx nm on isa gE world dc ms cs)
>    = (chain "\n". filter (not.null))
>      (if language==Dutch then 
>      [ "\\newcommand{\\mulF}{MultiF{\\it it}}"
>      , "\\title{Woordenlijst voor "++cname++"}"
>      , "\\maketitle"
>      , "De terminologie is ontleend aan het Divisie Informatieplan \\cite{TPDI},"
>      , "de begrippenlijst voor \\mulF{} \\cite{MultiFit} en de begrippen gebruikt in de voorstudie SBD \\cite{SBD}."
>      , "In geval van conflicten gaan begrippen uit \\cite{TPDI} v\\'o\\'or \\cite{MultiFit} en begrippen uit \\cite{MultiFit} v\\'o\\'or \\cite{SBD}."
>      , if null cs then "" else
>        latex "longtable" ["{|p{4cm}|p{10cm}|}\\hline"]
>             (chain "\n" ["\\bf "++nm++" & "++def++
>                          (if null ref then "" else "~\\cite{"++ref++"}")++
>                          "\\\\\n\\hline"
>                         | Cd pos nm def ref<-conceptdefs ctx])
>      , if null cList then "" else
>        if length cList==1 then "\tHet concept "++idName(head cList)++" heeft geen tekstuele definitie in sectie \\ref{typology"++cname++"}." else
>        "\tDe volgende concepten zijn niet opgenomen in de woordenlijst: "++commaEn (map idName (sord' name cList))++"."
>      , "\\bibliography{"++lname ctx++"}"
>      , "\\label{bibliography"++lname ctx++"}"
>      , "\\bibliographystyle{plain}"
>      ] else if language==English then
>      [ "\\title{Design of "++cname++"}"
>      , "\\maketitle"
>      , latexChapter "Glossary" ("typology"++cname)
>      , if null cs then "" else
>        latex "longtable" ["{|p{4cm}|p{10cm}|}\\hline"] (chain "\n" ["\\bf "++nm++" & "++def++"~\\cite{"++ref++"}\\\\\n\\hline"
>                          | Cd pos nm def ref<-conceptdefs ctx])
>      , if null cList then "" else
>        if length cList==1 then "\tThe concept "++idName(head cList)++" has no textual definition in the glossary (section \\ref{typology"++cname++"})." else
>        "\tThe following concepts are not described in the glossary: "++commaAnd (map idName (sord' name cList))++"."
>      , "\\bibliography{"++lname ctx++"}"
>      , "\\label{bibliography"++lname ctx++"}"
>      , "\\bibliographystyle{plain}"
>      ] else [] )
>      where
>       cList = concs ctx>-rd [C nm []| Cd pos nm def ref<-conceptdefs ctx]
>--       nav :: Classification Concept
>--       nav  = sortCl before (Cl (C "Anything" []) (makeTrees typ))
>       mms  = morphisms ctx
>--       degree c = length [m | m<-mms, source m==c || target m==c]
>--       c `before` c' = degree c > degree c'
>--       caps :: Classification Concept -> [String]
>--       caps (Cl c cls) = ["\\disjn{"++chain ", " (map (lshow.root) cls)++"}"| length cls>1] ++ (concat . map capss) cls ++
>--                         concat (map caps cls)
>--       capss (Cl g cls) = ["\\super{"++lshow g++"}{"++lshow s++"}" | s<-map root cls]


>  instance Identified (Typology a) where
>   name typ = ""

>-- lname and clname clean strings
>  lname :: Identified a => a -> String
>  lname  = clname . name
>  clname str = [if isAlphaNum c then c else if c `elem` "/\\" then c else '_'| c<- str]

>  lglossary :: Context -> IO ()
>  lglossary context
>    = putStr ("Creating glossary for "++name context++" towards LaTeX.\n")   >>
>      writeFile ("gloss"++lname context++".tex")
>       (chain "\n"
>        [ "\\documentclass[11pt,a4paper]{report}"
>        , if language==Dutch then "\\usepackage[dutch]{babel}" else ""
>        , "\\parskip 10pt plus 2.5pt minus 4pt  % Extra vertical space between paragraphs."
>        , "\\parindent 0em                      % Width of paragraph indentation."
>        , "\\usepackage{graphicx}"
>        , "\\usepackage{zed-csp}"
>        , "\\usepackage{longtable}"
>        , "\\def\\id#1{\\mbox{\\em #1\\/}}"
>        , "\\def\\define#1{\\label{dfn:#1}{\\em #1}}"

>--         , "\\newcommand{\\source}[1]{\\hbox{\\id{source}(#1)}}}"
>--         , "\\newcommand{\\target}[1]{\\id{target}(#1)}"
>--         , "\\newcommand{\\functional}[1]{\\id{univalent}(#1)}"
>--         , "\\newcommand{\\total}[1]{\\id{total}(#1)}"
>--         , "\\newcommand{\\surjective}[1]{\\id{surjective}(#1)}"
>--         , "\\newcommand{\\injective}[1]{\\id{surjective}(#1)}"
>--         , "\\newcommand{\\symmetric}[1]{\\id{symmetric}(#1)}"
>--         , "\\newcommand{\\antisymmetric}[1]{\\id{antisymmetric}(#1)}"
>--         , "\\newcommand{\\transitive}[1]{\\id{transitive}(#1)}"
>--         , "\\newcommand{\\reflexive}[1]{\\id{reflexive}(#1)}"

>        , "\\newcommand{\\flip}[1]{\\overline{#1}}"
>        , "\\newcommand{\\disjn}[1]{\\id{disjoint}(#1)}"
>        , "\\newcommand{\\super}[2]{\\{\\id{cast}_{#1}(x)| x\\in #2\\}\\ \\subseteq\\ #1}"
>        , "\\begin{document}"
>        , concat [lglos c| c<-(rd' name . preCl . Cl context . wrld) context]
>        , "\\end{document}"
>        ])   >>
>      putStr ("done writing gloss"++lname context++".tex in the current directory.\n")



>  lprint :: Context -> IO ()
>  lprint context
>    = putStr ("Analyzing "++name context++" towards LaTeX.\n")   >>
>      writeFile (lname context++".tex")
>       (chain "\n"
>        [ "\\documentclass[11pt,a4paper]{report}"
>        , if language==Dutch then "\\usepackage[dutch]{babel}" else ""
>        , "\\parskip 10pt plus 2.5pt minus 4pt  % Extra vertical space between paragraphs."
>        , "\\parindent 0em                      % Width of paragraph indentation."
>        , "\\usepackage{graphicx}"
>        , "\\usepackage{zed-csp}"
>        , "\\usepackage{longtable}"
>        , "\\def\\id#1{\\mbox{\\em #1\\/}}"
>        , "\\def\\define#1{\\label{dfn:#1}{\\em #1}}"

>--         , "\\newcommand{\\source}[1]{\\hbox{\\id{source}(#1)}}}"
>--         , "\\newcommand{\\target}[1]{\\id{target}(#1)}"
>--         , "\\newcommand{\\functional}[1]{\\id{univalent}(#1)}"
>--         , "\\newcommand{\\total}[1]{\\id{total}(#1)}"
>--         , "\\newcommand{\\surjective}[1]{\\id{surjective}(#1)}"
>--         , "\\newcommand{\\injective}[1]{\\id{surjective}(#1)}"
>--         , "\\newcommand{\\symmetric}[1]{\\id{symmetric}(#1)}"
>--         , "\\newcommand{\\antisymmetric}[1]{\\id{antisymmetric}(#1)}"
>--         , "\\newcommand{\\transitive}[1]{\\id{transitive}(#1)}"
>--         , "\\newcommand{\\reflexive}[1]{\\id{reflexive}(#1)}"

>        , "\\newcommand{\\flip}[1]{\\overline{#1}}"
>        , "\\newcommand{\\disjn}[1]{\\id{disjoint}(#1)}"
>        , "\\newcommand{\\super}[2]{\\{\\id{cast}_{#1}(x)| x\\in #2\\}\\ \\subseteq\\ #1}"
>        , "\\begin{document}"
>        , concat [lshow c| c<-(rd' name . preCl . Cl context . wrld) context]
>        , "\\end{document}"
>        ])   >>
>      putStr ("done writing "++lname context++".tex in the current directory.\n")



>-- Latex markup for Z-schema:

>-- \begin{schema}{Workflow}
>--   \concept{Procedure}:\power\concept{Object}\\
>--   \concept{Step}:\power\concept{Object}\\
>--   \concept{Group}:\power\concept{Object}\\
>--   \morphism{in}:\concept{Step}\fun\concept{Procedure}\\
>--   \morphism{follows}:\concept{Step}\rel\concept{Step}\\
>--   \morphism{performs}:\concept{Group}\pinj\concept{Step}\\
>-- \where
>--   \morphism{follows}\comp\morphism\subseteq\morphism{in}
>-- flip is \inv
>-- \end{schema}

>-- lpattern gets the complete typology of the context, in order to produce the right generic concepts (powersets).

>  latexSchema :: String -> [String] -> String
>  latexSchema nm body = latex "schema" ["{"++nm++"}"] (chain "\\\\\n" body)

>  lschema cname (Typ pths) pat@(Pat nm rs gen pms cs)
>   = if null rs then "void" else
>     (latexSchema nm . filter (not.null))
>     [ {- "  "++cname++" Concepts"
>     , "\\where"
>     , -} chain "\n" ["  "++ltshow cname (Typ pths) r++"\\\\" | r <- rules pat]
>     , chain "\n" ["  "++ltshow cname (Typ pths) s++"\\\\" | s <- specs pat]
>     ] ++ "\n" ++
>     (chain "\n\n" . map explain . rules) pat
>     where
>      parents c = f [C (name p) []| [p,s]<-pths, name s==name c]
>      f []  = "Anything"
>      f [c] = concat [if c==' ' then "\\ " else [c]| c<-name c]
>      f cs  = "("++chain "\\cup" [concat [if c==' ' then "\\ " else [c]| c<-name c] | c<-cs]++")"

>  instance LATEX Spec where
>   lshow pat = ltshow (name pat) (typology (isa pat)) pat
>   ltshow cname typ pat@(Pat nm rs gen pms cs)
>    = if language==Dutch then
>       latexSection (nm) (filter isAlphaNum nm)                                ++
>       latexFigure (latexCenter ("  \\includegraphics[scale=.3]{"++cname++"_"++nnm++".png}")++
>                    "\n\\caption{Structuur van "++nm++"}\n\\label{fig:"++nnm++"}")                ++
>       latexSubsection ("Regels in " ++ nm) ("restrictions"++nnm)                          ++
>          chain "\n\n" [lschema cname typ (Pat (nm++" "++show i) [normRule (lub (genEq typ)) r] [] [] [])| (i,r)<-zip [1..] (rules pat)] ++
>       latexSubsection ("Context definities in " ++ nm) ("defs"++nnm)                           ++
>          lschema cname typ (Pat (nm++" "++show (length (rules pat)+1)) [Dc m pos expr "" sgn nr| Gc pos m expr sgn nr<-specs pat] [] [] [])
>      else if language==English then
>       latexSection (nm) (filter isAlphaNum nm)                                ++
>       latexFigure (latexCenter ("  \\includegraphics[scale=.3]{"++cname++"_"++nnm++".png}")++
>                    "\n\\caption{Structure of "++nm++"}\n\\label{fig:"++nnm++"}")                ++
>       latexSubsection ("Rules in " ++ nm) ("restrictions"++nnm)                          ++
>          chain "\n\n" [lschema cname typ (Pat (nm++" "++show i) [normRule (lub (genEq typ)) r] [] [] [])| (i,r)<-zip [1..] (rules pat)] ++
>       latexSubsection ("Context definitions in " ++ nm) ("defs"++nnm)                           ++
>          lschema cname typ (Pat (nm++" "++show (length (rules pat)+1)) [Dc m pos expr "" sgn nr| Gc pos m expr sgn nr<-specs pat] [] [] [])
>      else ""
>      where nnm = clname nm

>  instance LATEX Prop where
>   lshow Uni = if language==Dutch then "univalent"    else "univalent"
>   lshow Inj = if language==Dutch then "injectief"    else "injective"
>   lshow Sur = if language==Dutch then "surjectief"   else "surjective"
>   lshow Tot = if language==Dutch then "totaal"       else "total"
>   lshow Sym = if language==Dutch then "symmetrisch"  else "symmetric"
>   lshow Asy = if language==Dutch then "antisymmetrisch" else "antisymmetric"
>   lshow Trn = if language==Dutch then "transitief"   else "transitive"
>   lshow Rfx = if language==Dutch then "reflexief"    else "reflexive"   

>  instance (Identified a,LATEX a) => LATEX [a] where
>   lshow xs = chain "\n\n" [lshow x| x<-xs]

>  idName c = "\\id{"++concat [if c `elem` [' ','_'] then "\\ " else [c]| c<-name c]++"}"

>  instance LATEX Morphism where
>   lshow mm@(Mor _ _ _ _ _ _ _ _ _)
>    = if language==Dutch then
>       "\\label{rel:"++lname mm++lname (source mm)++lname (target mm)++"}\n"++
>              wrapMath(idName mm++":: "++ lsign mm)++"\\\\\n"++
>              (if null (cards mm) then "" else "Deze relatie is " ++ commaEn [lshow p| p<-cards mm]++".\\\\\n") ++
>              (if null cs
>               then "In natuurlijke taal: "++applyM mm ("$\\langle"++lshow(source mm)++"\\rangle$") ("$\\langle"++lshow(target mm)++"\\rangle$")
>               else "Als bijvoorbeeld (`"++head(head cs)++"', `"++last(head cs)++"') in de relatie "++idName mm++" voorkomt, dan betekent dit: "++applyM mm ("`"++head(head cs)++"'") ("`"++last(head cs)++"'"))
>      else if language==English then
>       "\\label{rel:"++lname mm++lname (source mm)++lname (target mm)++"}\n"++
>              wrapMath(idName mm++":: "++ lsign mm)++"\\\\\n"++
>              (if null (cards mm) then "" else "This relation is " ++ commaAnd [lshow p| p<-cards mm]++".\\\\\n") ++
>              (if null cs
>               then "In natural language: "++applyM mm ("$\\langle"++lshow(source mm)++"\\rangle$") ("$\\langle"++lshow(target mm)++"\\rangle$")
>               else "If, for example (`"++head(head cs)++"', `"++last(head cs)++"') occurs in relation "++idName mm++", this means: "++applyM mm ("`"++head(head cs)++"'") ("`"++last(head cs)++"'"))
>      else ""
>      where cs=contents mm
>   lshow mm@(Idn _ _) = ""

>  instance LATEX Concept where
>   lshow c = idName c

>  instance LATEX Morph where
>   lshow (Id pos atts nm) = "="
>   lshow mm = idName mm

>  lsign (Mor nm d c ps _ _ _ _ _)
>                      | m Uni&& m Inj && m Sur && m Inj = a++"\\rel"++b
>                      | m Uni&& m Inj                   = a++"\\fun"++b
>                      | otherwise                       = a++"\\rel"++b
>        where m e = e `elem` ps; a=idName d; b=idName c

>{- if used in Zed context, more specific arrows are:
>                      | not (m Uni)         = a++"\\rel"++b
>                      | m Inj&&m Tot&&m Sur = a++"\\bij"++b
>                      | m Inj&&m Tot        = a++"\\inj"++b
>                      | m Inj               = a++"\\pinj"++b
>                      |        m Tot&&m Sur = a++"\\surj"++b
>                      |        m Tot        = a++"\\fun"++b
>                      |               m Sur = a++"\\psurj"++b
>                      | otherwise           = a++"\\pfun"++b
>        where m e = e `elem` ps; a=idName d; b=idName c -}

>--   instance (Eq a, Identified a, LATEX a) => LATEX (Typology a) where
>--    lshow ts = lshow (Cl none (makeTrees ts))

>  instance (Eq a, Show a, Identified a) => LATEX (Classification a) where
>   lshow cl
>    = recur cl
>      where
>       recur (Cl r cls)
>        = name r++(latexDotted . map recur . sort' name) cls

>--        sort [] = []
>--        sort (e:cls) = sort [c| c<-cls, name c<name e] ++ [e] ++
>--                       sort [c| c<-cls, name c>=name e]

>  instance LATEX Term where
>   lshow (Tm m sgn) = idName m
>   lshow (Tf m sgn) = "\\inv"++idName m

>  instance LATEX Factor where
>   lshow (F ts sgn)  = chain "\\compose" (map (lshow) ts)
>   lshow (Fu fs sgn) = chain "\\cup" (map (lshow) fs)
>   lshow (Fi fs sgn) = chain "\\cap" (map (lshow) fs)

>  instance LATEX Rule where
>   ltshow cname typ (Gc pos m expr sgn nr) = ltshow cname typ (Dc m pos expr "" sgn nr)
>   ltshow cname typ r = ltshow cname typ (assemble (genEq typ) r)
>   lshow (Hc antc pos cons expla sgn nr) | fEmpty antc = lshow cons++"\\\\\n"++expla
>                                         | fEmpty cons = "\\not("++lshow antc++")"++"\\\\\n"++expla
>                                         | otherwise = lshow antc++"\\subseteq"++lshow cons++"\\\\\n"++expla
>   lshow (Dc defd pos expr expla sgn nr) = name defd++"="++lshow expr++"\\\\\n"++expla
>   lshow (Gc pos m expr sgn nr)          = name m++"="++lshow expr

>--   instance LATEX PthExpr where
>--    lshow (E ms)     = chain "\\compose" (map (lshow) ms)
>--    lshow (A ms ms') = lshow ms ++ "\\cap" ++ lshow ms'

>  instance LATEX Restriction where
>    lshow (Forall vars restr)
>     = charVarsL "\\forall " vars ++ lshow restr
>    lshow (Exists vars restr)
>     = charVarsL "\\exists " vars  ++ lshow restr
>    lshow (Implies antc conseq)
>     = "\\\\\n  "++lshow antc++"\\ \\Rightarrow\\ "  ++ lshow conseq
>    lshow (Equiv lhs rhs)
>     = "\\\\\n  "++lshow lhs++"\\ \\Leftrightarrow\\ "   ++ lshow rhs
>    lshow (Conj rs)
>     = if null rs then "" else
>       chain " \\wedge " (map (lshow) rs)
>    lshow (Disj rs)
>     = if null rs then "" else
>       chain " \\vee " (map (lshow) rs)
>    lshow (Not rs) = "not "++lshow rs
>    lshow (Rel lhs (Tm m sgn) rhs d c) = lshow (Funs d left)++"\\ "++lshow m++"\\ "++lshow (Funs c right)
>                                         where left = [m| t<-lhs, m<-mors t]; right = [m| t<-reverse rhs, m<-mors t]
>    lshow (Rel lhs (Tf m sgn) rhs d c) = lshow (Funs c left)++"\\ "++lshow m++"\\ "++lshow (Funs d right)
>                                         where right = [m| t<-lhs, m<-mors t]; left = [m| t<-reverse rhs, m<-mors t]
>    lshow (Funs x [])         = x
>    lshow (Funs x (Id _ _ _:ms)) = lshow (Funs x ms)
>    lshow (Funs x (m:ms))        = lshow (Funs (idName m++"("++x++")") ms)

>  charVarsL q vs
>   = if null vs then "" else
>     q++" "++chain "; " [chain ", " (map fst vs')++":: "++idName (snd (head vs')) | vs'<-eqCl snd vs]++" :"
>--  for Z, this should be:
>--     q++" "++chain "; " [chain ", " (map fst vs')++": "++idName (snd (head vs')) | vs'<-eqCl snd vs]++" @"

>-- Basic LATEX markup

>  wrapMath str = "$"++str++"$"

>  latexCenter = latex "center" []
>  latexFigure = latex "figure" ["[htb]"]

>  latex :: String -> [String] -> String -> String
>  latex command params content
>   = "\\begin{"++command++"}"++chain "," params++"\n"++content++"\n\\end{"++command++"}"

>  latexSubsection title reference
>   = "\n\\subsection{"++title++"}\n" ++"\\label{ssct:"++reference++"}\n"

>  latexSection title reference
>   = "\n\\section{"++title++"}\n" ++"\\label{sct:"++reference++"}\n"

>  latexChapter title reference
>   = "\n\\chapter{"++title++"}\n" ++"\\label{chp:"++reference++"}\n"

>  latexDotted ls
>   = if null ls then "" else
>     "\n"++chain "\n" (["\\begin{itemize}"]++["\\item "++l|l<-ls]++["\\end{itemize}"])

>  latexEnumerate ls
>   = if null ls then "" else
>     chain "\n" (["\\begin{enumerate}"]++["\\item "++l|l<-ls]++["\\end{enumerate}"])
