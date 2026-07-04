  import Char

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

-- sort = sort' id

  sort' f [] = []
  sort' f (x:xs) = sort' f [e|e<-xs, f e<f x] ++ [x] ++ sort' f [e|e<-xs, f e>=f x]

-- sord = sort.rd

  sord [] = []
  sord (x:xs) = sord [e|e<-xs, e<x] ++ [x] ++ sord [e|e<-xs, e>x]

-- Oppassen met sord', want er geldt niet  (voor alle f: sord' f = sort' f.rd)   !!!

  sord' f [] = []
  sord' f (x:xs) = sord' f [e|e<-xs, f e<f x] ++ [x] ++ sord' f [e|e<-xs, f e>f x]
