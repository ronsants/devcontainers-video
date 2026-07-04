 module Auxiliaries where
  import Char

  chop [x]    = []
  chop (x:xs) = ([x],xs): [(x:l, r)| (l,r)<-chop xs]
  chop []     = []

(l,r) `elem` chop xs = not (null l) && not (null r)
Proof:
a) The theorem holds for all xs of length <= 1.
b) Suppose the theorem holds for all xs of length <= n, with xs::[C]
   The induction hypothesis is therefore:
   b.0) Assume (induction hypothesis): (l,r) `elem` chop xs && length xs<=n = not (null l) && not (null r)
   b.1) let x::C
   b.2)      (l,r)<-chop xs
        implies    {semantics of list comprehension}
             (l,r) `elem` chop xs
        implies    {let length xs<=n and induction hypothesis}
             not (null l) && not (null r)
   b.3)
             chop (x:xs) = ([x],xs): [(x:l, r)| (l,r)<-chop xs]
        implies    {assume length xs<=n, then by b.2}
             not (null l) && not (null r)
        implies    {The first clause in the definition of chop ensures: not (null xs)}
             not (null (x:l)) && not (null r) && not (null [x]) && not (null xs)
   b.4) TODO: finish proof.

  class Identified a where
   name   :: a->String

  mor2name :: Identified a => a -> String
  mor2name       = dePoint.mumble.name

  mumble, dePoint, strip :: String -> String
  mumble str  = concat [if c==' ' then "_" else [c]| c<-str]
  dePoint str = if head str=='.' then tail str else
                if last str=='.' then init str else str
  strip = filter isAlphaNum
  conceptForm (c:cs) = strip (toUpper c: map toLower cs)
  conceptForm "" = ""

  plural str
   | last str=='y' = init str++"ies"
   | last str=='s' = str++"es"
   | last str=='x' = str++"es"
   | last str=='f' = init str++"ves"
   | otherwise     = head ([p|(s,p)<-exceptions, s==str]++[str++"s"])
   where exceptions = [("mouse","mice"),("sheep","sheep"),("Mouse","Mice"),("Sheep","Sheep")]

  infixl 5  >-

  class Collection a where
   eleM     :: Eq b => b -> a b -> Bool
   uni, isc :: Eq b => a b -> a b -> a b
   (>-)     :: Eq b => a b -> a b -> a b
   empty    :: Eq b => a b
   elems    :: Eq b => a b -> [b]

  instance Collection [] where
   eleM        = any . (==)
   xs `uni` ys = xs++(ys>-xs)
   xs `isc` ys = [y| y<-ys, y `elem` xs]
   xs >- ys    = [x|x<-xs, not (x `elem` ys)]
   empty       = []
   elems       = id

  chain str [] = []
  chain str (x:xs) = foldl f x xs where f x y = x++str++y

  commaAnd [a,b,c]= a++", "++b++", and "++c
  commaAnd [a,b]  = a++" and "++b
  commaAnd [a]    = a
  commaAnd (a:as) = a++", "++commaAnd as
  commaAnd []     = ""

  commaEng str [a,b,c]= a++", "++b++", "++str++" "++c
  commaEng str [a,b]  = a++" "++str++" "++b
  commaEng str [a]    = a
  commaEng str (a:as) = a++", "++commaEng str as
  commaEng str []     = ""

  commaEn [a,b,c]= a++", "++b++" en "++c
  commaEn [a,b]  = a++" en "++b
  commaEn [a]    = a
  commaEn (a:as) = a++", "++commaEn as
  commaEn []     = ""

  enumerate [] = []
  enumerate [x]= x
  enumerate xs = chain ", " (init xs)++" and "++last xs

  eqClass f [] = []
  eqClass f (x:xs) = (x:[e|e<-xs, f x e]) : eqClass f [e|e<-xs, not (f x e)]

  eqCl f [] = []
  eqCl f (x:xs) = (x:[e|e<-xs, f x==f e]) : eqCl f [e|e<-xs, f x/=f e]

  eqCls f [] = []
  eqCls f (x:xs) = eqCls f [e|e<-xs, f x<f e] ++ [x:[e|e<-xs, f x==f e]] ++ eqCls f [e|e<-xs, f x>f e]

  rd [] = []
  rd (x:xs) = x: rd [e|e<-xs, e/=x]

  rd' f [] = []
  rd' f (x:xs) = x: rd' f [e|e<-xs, f e /= f x]

  elem' eq e xs = not (null [x|x<-xs, eq e x])

  fst3 (a,b,c) = a
  snd3 (a,b,c) = b
  thd3 (a,b,c) = c

  sort [] = []
  sort (x:xs) = sort [e|e<-xs, e<x] ++ [x] ++ sort [e|e<-xs, e>=x]

sort = sort' id

  sort' f [] = []
  sort' f (x:xs) = sort' f [e|e<-xs, f e<f x] ++ [x] ++ sort' f [e|e<-xs, f e>=f x]

sord = sort.rd

  sord [] = []
  sord (x:xs) = sord [e|e<-xs, e<x] ++ [x] ++ sord [e|e<-xs, e>x]

Oppassen met sord', want er geldt niet  (voor alle f: sord' f = sort' f.rd)   !!!

  sord' f [] = []
  sord' f (x:xs) = sord' f [e|e<-xs, f e<f x] ++ [x] ++ sord' f [e|e<-xs, f e>f x]
