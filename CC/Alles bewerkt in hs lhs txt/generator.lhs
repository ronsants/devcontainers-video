> module Generator where
>  import Char
>  import Auxiliaries
>  import CC_aux
>  import Html

>  defRelation :: Morph -> [[String]] -> String
>  defRelation m ls
>   = "module Table"++fn++" where\n import Auxiliaries\n import Generator"++
>         "\n\n tbl"++fn++"0\n  = "++chain "\n    " ["["++chain "," (map show l)++"] :"| l<-sort' (!!0) ls] ++ " []"++
>         "\n\n tbl"++fn++"1\n  = "++chain "\n    " ["["++chain "," (map show l)++"] :"| l<-sort' (!!1) ls] ++ " []"
>     where fn = relName [r,d,c]
>           [r,d',c'] = mor2 m
>           d=conceptForm d'; c=conceptForm c'

>  genRelation :: Morph -> [[String]] -> IO ()
>  genRelation m []
>   = writeFile (fn++".html")
>      (htmlPage ("Empty relation: "++mor2name m) ""
>                (htmlBody ("Relation "++mor2name m++" is empty.\n</P>\n"++pragmatics m++violLink m)))>>
>     appendFile "relHtml.hs" (show [r,d,c]++" :\n    ") >> putStr (fn++".html written")
>     where fn = "Atlas"++relName [r,d,c]
>           [r,d,c] = mor2 m
>  genRelation m ls
>   = writeFile (fn++".html")
>               (htmlPage ("Atlas "++r++" : "++d++" x "++c) r (htmlBody (body (-1)))) >>
>     sequence [ writeFile (fn++show i++".html")
>                (htmlPage ("Atlas "++r++" : "++d++" x "++c) r (htmlBody (body i)))
>              | i<-[0..2]] >>
>     appendFile "relHtml.hs" (show [r,d,c]++" :\n    ") >> putStr (fn++".html written")
>     where fn = "Atlas"++relName [r,d,c]
>           [r,d,c] = mor2 m
>           body i = pragmatics m++violLink m++genR i ls
>           genR i ls = htmlSortTable fn i [name (source m),name (target m),"Origin"]
>                                     (rd [[a,b,src]| [a,b,src]<-if i<0 then ls else sort' (!!i) ls])

>  genConcept :: String -> [[String]] -> IO ()
>  genConcept t cs@((t':c:rest):cs')
>   = (writeFile (fn++".html") . htmlPage ("Concept listing for "++conceptForm t) t . htmlBody . body) (-1) >>
>      sequence [ writeFile (fn++show i++".html")
>                 (htmlPage ("Concept listing for "++conceptForm t) "" (htmlBody (body i)))
>               | i<-[0..1]] >>
>      putStr (fn++".html written")
>     where fn        = "Atlas"++conceptForm t
>           body i    = genR i ls
>           ls        = [[c!!3, c!!2]| c<-cs]
>           genR i [] = "This concept is empty."
>           genR i ls = htmlSortTable fn i [conceptForm t, "Origin"]
>                                     (rd [[c,src]| [c,src]<-if i<0 then ls else sort' (!!i) ls])

>  pragmatics m@(Mph nm pos atts sgn (Mor nm' a b props prL prM prR cs pos'))
>   | a==b = "<P>Meaning of "++r++":<BR>\n    "++
>            "if "++dv++" and "++cv++" (both "++plural d++") are related through "++r++", this means \""++prL++" "++dv++" "++prM++" "++cv++" "++prR++".\"</P>"
>   | a/=b = "<P>Meaning of "++r++":<BR>\n    "++
>            "if "++dv++" ("++d++") and "++cv++" ("++c++") are related through "++r++", this means \""++prL++" "++dv++" "++prM++" "++cv++" "++prR++".\"</P>"
>   where dv=[toLower (head (name a))]
>         cn=toLower (head (name b))
>         cv=[cn]++['\''| [cn]==dv]
>         [r,d,c] = mor2 m
>--  pragmatics (I nm) = "<P>Meaning of the identity relation of "++nm++":<BR>\n    "++"(x,x) belongs to this relation and (x,y) doesn't if x unequal to y.</P>"

>-- htmlPage "Rule Violations" "" . htmlBody . htmlBlueTable ttl . rd

>  genViolation i ttl []
>   = writeFile (fn++".html")
>      (htmlPage ("No violations found") ""
>                (htmlBody ("No violations found in this rule.")))>>
>     putStr (fn++".html written")
>     where fn = "Atlas"++show i++"Viol"
>  genViolation i ttl ls
>   = writeFile (fn++".html")
>               (htmlPage ("Violations") "" (htmlBody (body (-1)))) >>
>     sequence [writeFile (fn++show i++".html")
>               (htmlPage ("Violations") "" (htmlBody (body i)))
>              | i<-[0..length ttl-1]] >>
>     appendFile "violHtml.hs" (show i++":") >> putStr (fn++".html written")
>     where fn = "Atlas"++show i++"Viol"
>           body i  = htmlSortTable fn i ttl (if i<0 then ls else sort' (!!i) ls)

>  violLink m
>   | null (cards m) = ""
>   | otherwise = "<P>\nClick "++htmlAnchor ("AtlasViol"++relName [r,d,c]++".html") "here" []++" to see violations with respect to "++r++".</P>"
>   where [r,d,c] = mor2 m

>  checkCards :: [Violation] -> String
>  checkCards vs
>   | null vs   = "No cardinality related violations detected in this relation."
>   | otherwise = (chain "\n<P><HR color=#AA0000></P>\n" . filter (not.null) . map htmlViol) vs

>  plural str
>   | last str=='y' = init str++"ies"
>   | last str=='s' = str++"es"
>   | last str=='f' = init str++"ves"
>   | otherwise     = head ([p|(s,p)<-exceptions, s==str]++[str++"s"])
>   where exceptions = [("mouse","mice"),("sheep","sheep")]

>  data Violation
>   = ViolM Morph Prop [[String]] |
>     ViolR Rule [[String]]
>
>  class Populated a where
>   violations :: a -> [[String]] -> [[[String]]] -> [Violation]

>--     population :: a -> [[String]]

>  instance Populated Morph where
>   violations m r [source,target]
>    = [ViolM m p rs| c<-cards m, ViolM m p rs<-[chck c m r source target], not (null rs)]
>      where
>       chck Uni m r source target = ViolM m Uni [rec| cl<-eqCl (!!0) r,length cl>1,rec<-cl]
>       chck Inj m r source target = ViolM m Inj [rec| cl<-eqCl (!!1) r,length cl>1,rec<-cl]
>       chck Tot m r source target = ViolM m Tot [[e]| e<-rd [e|[d,e]<-source]>-rdom r]
>       chck Sur m r source target = ViolM m Sur [[e]| e<-rd [e|[d,e]<-target]>-rcod r]

>--   instance Populated Rule where
>--    violations rule@(Hc antc pos cons expla sgn nr) r [source,target]
>--     = ViolR rule [r| r<-population antc, not (r `elem` population cons)]

>  htmlViol :: Violation -> String
>  htmlViol (ViolR (Hc antc pos cons expla (a,b) nr) xxs)
>    = htmlBlueTable [name a, name b] xxs
>  htmlViol (ViolM m Uni rs)
>    = "Relation "++htmlAnchor ("Atlas"++relName(mor2 m)++".html") (mor2name m) []++
>      " contains multiple "++plural (name (target m))++" per "++name (source m)++" in the following instances:"++
>      htmlBlueTable [name (source m), name (target m)] (map (take 2.drop 3) rs)
>  htmlViol (ViolM m Inj rs)
>    = "Relation "++htmlAnchor ("Atlas"++relName(mor2 m)++".html") (mor2name m) []++
>      " contains multiple "++plural (name (source m))++" per "++name (target m)++" in the following instances:"++
>      htmlBlueTable [name (source m), name (target m)] (map (take 2.drop 3) rs)
>  htmlViol (ViolM m Tot rs)
>    | length rs==1 = "The "++name (source m)++" "++head(head rs)++" is missing in relation "++htmlAnchor ("Atlas"++relName(mor2 m)++".html") (mor2name m) []++"."
>    | otherwise    = "Relation "++htmlAnchor ("Atlas"++relName(mor2 m)++".html") (mor2name m) []++
>                     " contains no "++name (target m)++" for each of the following "++plural (name (source m))++":"++
>                     htmlBlueTable [name (source m)] rs
>  htmlViol (ViolM m Sur rs)
>    | length rs==1 = "The "++name (target m)++" "++head(head rs)++" is missing in relation "++htmlAnchor ("Atlas"++relName(mor2 m)++".html") (mor2name m) []++"."
>    | otherwise    = "Relation "++htmlAnchor ("Atlas"++relName(mor2 m)++".html") (mor2name m) []++
>                     " contains no "++name (source m)++" for each of the following "++plural (name (target m))++":"++
>                     htmlBlueTable [name (target m)] rs

>{-  checkFun fn rn r source target
>   | null list      = ("", True)
>   | otherwise      = ("Relation "++htmlAnchor (fn++".html") rn []++
>                       " contains multiple "++plural codNm++" per "++domNm++" in the following instances:"++
>                       htmlBlueTable (attrs r) (map (take 2.drop 3) list),False)
>   where domNm = head source!!0; codNm = head target!!0
>         list  = [rab| cl<-eqCl (!!3) r,length cl>1,rab<-cl]
>         attrs ((r:d:c:rest):ls) = [d,c]

>  checkInj fn rn r source target
>   = checkFun fn rn (map fflip r) target source
>     where fflip (r:d:c:a:b:rest) = r:c:d:b:a:rest

>  checkTot fn rn r source target
>   | null list      = ("", True)
>   | length list==1 = ("The "++domNm++" "++head list++" is missing in relation "++htmlAnchor (fn++".html") rn []++".",False)
>   | otherwise      = ("Relation "++htmlAnchor (fn++".html") rn []++
>                      " contains no "++codNm++" for each of the following "++plural domNm++":"++
>                      htmlTable' domNm list,False)
>   where domNm = head source!!0; codNm = head target!!0
>         list = rd [e|[d,e]<-source]>-rdom r

>  checkSur fn rn r source target
>   = checkTot fn rn (map fflip r) target source
>     where fflip (r:d:c:a:b:rest) = r:c:d:b:a:rest-}

>  rdom ls = sord [a| [a,b,src]<-ls]
>  rcod ls = sord [b| [a,b,src]<-ls]

>  strp :: Eq a => [[a]]->[[a]]
>  strp   = rd . map (take 2)
>  swap r = [a:b:rest|b:a:rest<-r]

>-- If lr==0, cleanUp cleans up the domain to ensure it contains defined entries only. If lr==1, it cleans up the codomain.

>  cleanUp lr cs r
>   = merge cs (sort' (!!lr) r)
>     where
>      merge c@([q,t,src,nm]:cs) r@([a,b,rsrc]:rs)
>       | q<a  = merge cs r
>       | q>a  = let (rs', undef) = merge c rs in (rs', [a,b,rsrc]:undef)
>       | True = let (rs', undef) = merge cs (dropWhile ((==q).(!!lr)) r) in (takeWhile ((==q).(!!lr)) r ++ rs', undef)
>      merge cs rs = ([],rs)

>{-  jn ls rs = f (sort' last ls) (sort' head rs)
>   where
>    f [] rs = []
>    f ls [] = []
>    f ls@(l:lrest) rs@(r:rrest)
>     | ll<hr     = f lrest rs
>     | ll>hr     = f ls rrest
>     | otherwise = rd [l++tail r| l<-takeWhile ((hr==).last) ls, r<-takeWhile ((hr==).head) rs]++
>                   f                (dropWhile ((hr==).last) ls)   (dropWhile ((hr==).head) rs)
>     where
>      hr = head r
>      ll = last l-}

>-- Pre: ls==sort' (!!1) ls  &&  rs==sort' (!!0) rs)

>  jn' ls rs = f ls rs
>   where
>    f [] rs = []
>    f ls [] = []
>    f ls@(l:lrest) rs@(r:rrest)
>     | ll<hr     = f lrest rs
>     | ll>hr     = f ls rrest
>     | otherwise = rd [ init l++tail (init r)++[last l++";"++last r]
>                      | l<-takeWhile ((hr==).(!!1).reverse) ls, r<-takeWhile ((hr==).head) rs]++
>                   f      (dropWhile ((hr==).(!!1).reverse) ls)   (dropWhile ((hr==).head) rs)
>     where
>      hr = head r
>      ll = reverse l!!1

>  join' = jn'

>  {-join nm as [] = []
>  join nm [] bs = []
>  join nm as bs
>   = rd(merge (sort' (!!1) as) (sort' (!!0) bs))
>     where
>      merge [] ys = []
>      merge xs [] = []
>      merge x@([xa,xb,xsrc]:xs) y@([ya,yb,ysrc]:ys)
>       | xb<ya = merge xs y
>       | xb>ya = merge x ys
>       | True  = [xa,yb,xsrc++", "++ysrc]: merge xs ys-}

>  minispace x = if null x || and(map isSpace x) then "<BR>" else x

>  htmlSortTable fn i xs xxs
>   = chain "\n"
>     ([ "<TABLE WIDTH=\"75%\" BORDER=1 CELLSPACING=1 CELLPADDING=1 name=\"rsTable\" id=rsTable  cols="++show(length xs)++">"]++
>      [ "<TR bgcolor=mediumblue>" ]++
>      [ "  <TD>"++(if i==c then attr else htmlAnchor (fn++show c++".html") attr [])++"</TD>"
>      | (c,a)<-zip [0..] xs, attr<-["<FONT color=white>"++htmlBold a++"</FONT>"]]++
>      [ "</TR>" ]++
>      [ "<TR>"++concat ["<TD>"++minispace x++"</TD>"|x<-xs]++"</TR>" | xs<-xxs ]++
>      [ "</TABLE>" ])

>  htmlBlueTable as xxs
>   = chain "\n"
>     ([ "<TABLE WIDTH=\"75%\" BORDER=1 CELLSPACING=1 CELLPADDING=1 name=\"rsTable\" id=rsTable  cols="++show(length as)++">"]++
>      [ "<TR bgcolor=mediumblue>" ]++
>      [ "  <TD><FONT color=white>"++htmlBold a++"</FONT></TD>" | a<-as]++
>      [ "</TR>" ]++
>      [ "<TR>"++concat ["<TD>"++minispace x++"</TD>"|x<-xs]++"</TR>" | xs<-xxs ]++
>      [ "</TABLE>" ])

>  htmlTable' t xs
>   = chain "\n"
>     ([ "<TABLE WIDTH=\"75%\" BORDER=1 CELLSPACING=1 CELLPADDING=1 name=\"rsTable\" id=rsTable  cols=1>"]++
>      [ "<TR bgcolor=mediumblue><TD><FONT color=white>"++htmlBold t++"</FONT></TD></TR>" ]++
>      [ "<TR><TD>"++minispace x++"</TD></TR>" | x<-xs ]++
>      [ "</TABLE>" ])