 module Main where
  import System
  import Char
  import Auxiliaries

  data Token = Scsv String | Ccsv Char deriving Show
  parseCSV :: String -> [[String]]
  parseCSV = filter (not.null).map (map untok.splits ';').splits '\n'.tokens
   where
    tokens :: String -> [Token]
    tokens ""            = []
    tokens ('"': xs)     = Scsv r: tokens rest
     where (r, rest) = str xs
           str ('"':'"':xs) = ('"':r,rs) where (r,rs) = str xs
           str ('"':xs) = ("",xs)
           str (c:xs) = (c:r,rs) where (r,rs) = str xs
           str [] = ("","")
    tokens (x:xs) = Ccsv x:tokens xs
 
    splits :: Char -> [Token] -> [[Token]]
    splits c [] = []
    splits c [Scsv str]    = [[Scsv str]]
    splits c [Ccsv e] | c==e = [[],[]]
                      | True = [[Ccsv e]]
    splits c (Scsv str:ts) = [Scsv str]:splits c ts
    splits c (Ccsv e:ts) | c==e = []:splits c ts
                         | True = (Ccsv e:r):rs where r:rs = splits c ts

  untok [Scsv str] = str
  untok cs = [c| Ccsv c<-cs]

  makeRelation table a b
   = " ::"++x++"*"++y++"[UNI]\n  = [ "++chain "\n    ; " (map show (sord rs))++"\n    ].\n"
     where
      (x,y):rs = [(row!!s,row!!t)|row<-table,s<length row, t<length row,not (null (row!!s)),not (null (row!!t))]
      s = head [i| (x,i)<-zip (head table) [0..], x==a]
      t = head [i| (x,i)<-zip (head table) [0..], x==b]

  main
   = do { a <- getArgs
        ; txt<-readFile (head a)
        ; (putStr.chain "\n".map (makeRelation (parseCSV txt) "Interface"))
           ["Actueel/Vervallen","Aard vd data in de uitwisseling","Requester","Provider","Direct/ESB","Aard Interface","Type communicatie","Service"]
        }
