module Main where
import System
--import Char
import UU_Scanner
import UU_Parsing
import Auxiliaries
-- import Classification
import Typology
import CC_aux
-- import Restrictions
-- import Html
import LATEXgen
-- import Generator
-- import RelHtml
--  import ViolHtml
-- import ConceptList
-- import RelationList
import AGtry
import CC

testing = False
--  textgraphs = False
--  latexOpt sws = "-l" `elem` sws
--  violHtml = []
--  splitStr f (x:xs) | f x  = (x:yes, no)
--                    | True = (yes, x:no)
--                    where (yes,no) = splitStr f xs
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
          else putStr ("\nThe type analysis of "++fnFull++" yields errors.")>>
               putStr (concat ["\n!Error of type "++err| err<-errs])>>
               putStr ("\nNothing generated, please correct mistake(s) first.\n")
        }}
      where build contexts switches contextname
             = sequence_ 
                 ([ zed contexts contextname| "-Z" `elem` switches]++
                 [ glossary contexts contextname | "-g" `elem` switches]
                )

glossary contexts contextname
   = putStr ("\nGenerating Glossary for "++name context++" in the current directory.") >>
     lglossary context
     where
      context  = head ([c| c<-contexts, name c==contextname]++
                 [Ctx (contextname++" is not defined") [] empty nogE [] [] [] []])
      Typ pths = typology (isa context)

zed contexts contextname
   = putStr ("\nGenerating Zed specification for "++name context++" in the current directory.") >>
     lprint context
     where
      context  = head ([c| c<-contexts, name c==contextname]++
                 [Ctx (contextname++" is not defined") [] empty nogE [] [] [] []])
      Typ pths = typology (isa context)
