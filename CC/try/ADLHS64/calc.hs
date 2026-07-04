 module Calc (DBrule(DBR), frMorph, toMorph, dbIns, dbDel, dbShow, insertRules, deleteRules) where  -- commented modules are required for testing
--  import System
--  import UU_Scanner
--  import UU_Parsing
  import Auxiliaries
  import CC_aux
  import Html -- only for htmlTable
--  import AGtry
--  import CC

  nrmFi (F [Tc f c]) = nrmFi f
  nrmFi (F [t]     ) = Fi [Fu [F [t]]]
  nrmFi (F (t:ts)  ) = Fi [ Fu [F (ts++ts')| F ts<-is, F ts'<-is']
                          | Fu is<-es, Fu is'<-es']
                       where
                         Fi es  = nrmFi (F [t])
                         Fi es' = nrmFi (F ts)
  nrmFi (Fu [f]    ) = nrmFi f
  nrmFi (Fu (f:fs) ) = Fi [ Fu (is++is')
                          | Fu is<-es, Fu is'<-es']
                       where
                         Fi es  = nrmFi f
                         Fi es' = nrmFi (Fu fs)
  nrmFi (Fi [f]    ) = nrmFi f
  nrmFi (Fi (f:fs) ) = Fi (map nrmFu (es++es'))
                       where
                         Fi es  = nrmFi f
                         Fi es' = nrmFi (Fi fs)

  nrmFu (Fu [f]    ) = nrmFu f
  nrmFu (Fu (f:fs) ) = Fu (es++es')
                        where
                          Fu es  = nrmFu f
                          Fu es' = nrmFu (Fu fs)
  nrmFu (F  [t]    ) = Fu [F [t]]
  nrmFu (F  (t:ts) ) = Fu [F (ts++ts')| F ts<-is, F ts'<-is']
                        where
                          Fu is  = nrmFu (F [t])
                          Fu is' = nrmFu (F ts)

-- The following functions, normFu and normFi, are used to normalize an Expression into the form Fu [Fi [F [...]]].
-- A factor (normFu f) is not always equivalent to f.
-- However, in all cases (normFu f) follows logically from f (that is: f = normFu f)

  normFu (F [Tc f c]) = normFu f
  normFu (F [t]     ) = Fu [Fi [F [t]]]
  normFu (F (t:ts)  ) = Fu [ Fi [F (ts++ts')| F ts<-is, F ts'<-is']
                            | Fi is<-es, Fi is'<-es']
                         where
                           Fu es  = normFu (F [t])
                           Fu es' = normFu (F ts)
  normFu (Fi [f]    ) = normFu f
  normFu (Fi (f:fs) ) = Fu [ Fi (is++is')
                           | Fi is<-es, Fi is'<-es']
                        where
                          Fu es  = normFu f
                          Fu es' = normFu (Fi fs)
  normFu (Fu [f]    ) = normFu f
  normFu (Fu (f:fs) ) = Fu (map normFi (es++es'))
                        where
                          Fu es  = normFu f
                          Fu es' = normFu (Fu fs)
  normFu x = error ("Fatal: normFu ("++showHS x++")\n"++showADL x)
                     
  normFi (Fi [f]    ) = normFi f
  normFi (Fi (f:fs) ) = Fi (es++es')
                        where
                          Fi es  = normFi f
                          Fi es' = normFi (Fi fs)
  normFi (F  [t]    ) = Fi [F [t]]
  normFi (F  (t:ts) ) = Fi [F (ts++ts')| F ts<-is, F ts'<-is']
                        where
                          Fi is  = normFi (F [t])
                          Fi is' = normFi (F ts)

  normR (Ru c antc pos cons expla sgn nr) = Ru c (normFu antc) pos (normFu cons) expla sgn nr
  normR (Gc pos m expr sgn nr)            = Gc pos m (normFu expr) sgn nr

-- In order to make 'calculation rules', the following derivations are used:

-- r = p;(s \/ t);q   -  { r -: p;s;q\/p;t;q }
-- r -: p;(s /\ t);q  -  { r-:p;s;q, r-:p;t;q }
-- r = p \/ q         -  { p -: r, q -: r, r -: p\/q }
-- r = s              -  { r -: s, s -: r }
-- r -: s;f~          -  if functional(f) then { r -: s;f~ } else { r -: s;f~, r;f  -: s }
-- r -: f;s           -  if functional(f) then { r -: f;s  } else { r -: f;s , f~;r -: s }
-- r;f  -: s          -  if functional(f) then { r;f  -: s } else { r;f  -: s, r -: s;f~ }
-- f~;r -: s          -  if functional(f) then { f~;r -: s } else { f~;r -: s, r -: f;s  }

  shiftLeft :: Rule -> [Rule]
  shiftLeft r@(Ru 'I' a@(F ants) pos c@(F cons) expla sgn nr)
   | and(map isIdent(mors ants)) = [Ru 'I' (F [Tm (Id pos [] idA) True]) pos c expla (idC,idC) nr]
   | otherwise                   = [Ru 'I' (F as) pos (F cs) expla sgn nr| (as,cs)<-rd(move ants cons), sgn<-signs (as,cs)]
   where
    idC = if length (eqClass order [source c,target c,idA])>1 then error ("Fatal: shiftLeft ("++showHS r++")\n"++showADL r++"\nin calculation of idC\n"++show (eqClass order [source c,target c,idA])) else
          source c `lub` target c `lub` idA
    idA = if length (eqClass order (target a:map source ants))>1 then error ("Fatal: shiftLeft ("++showHS r++")\n"++showADL r++"\nin calculation of idA\n"++show (eqClass order (target a:map source ants))) else
          foldr lub (target a) (map source ants)
    signs (as,cs) = [( -- if null cs then source (head as) else
                       if source (head cs) `order` source (head as)
                       then source (head cs) `lub` source (head as)
                       else error ("Fatal: shiftLeft ("++showHS r++")\n"++showADL r++"\nin calculation of source (head cs) `lub` source (head as) with cs="++show (map showS cs)++" and as="++show (map showS as)++"\n")
                     , -- if null cs then target (last as) else
                       if target (last cs) `order` target (last as)
                       then target (last cs) `lub` target (last as)
                       else error ("Fatal: shiftLeft ("++showHS r++")\n"++showADL r++"\nin calculation of target (head cs) `lub` target (head as) with cs="++show (map showS cs)++" and as="++show (map showS as)++"\n")
                     )]
    move as [] 
     = [(init as,[flp l])| sur (multiplicities l) && inj (multiplicities l)] ++
       [(tail as,[flp h])| fun (multiplicities h) && tot (multiplicities h)]
       where h=head as; l=last as
    move as [c] = [(as,[c])]
    move as cs
     = [ts| sur (multiplicities h) && inj (multiplicities h), ts<-move ([flp h]++as) (tail cs)]++
       [ts| fun (multiplicities l) && tot (multiplicities l), ts<-move (as++[flp l]) (init cs)]
       where h=head cs; l=last cs
  shiftLeft r = [r]
  shiftRight :: Rule -> [Rule]
  shiftRight r@(Ru 'I' a@(F ants) pos c@(F cons) expla sgn nr)
   | and(map isIdent(mors ants)) = [Ru 'I' (F [Tm (Id pos [] idA) True]) pos (F cons) expla (idC,idC) nr]
   | otherwise                   = [Ru 'I' (F as) pos (F cs) expla sgn nr| (as,cs)<-rd(move ants cons), sgn<-signs (as,cs)]
   where
    idC = if length (eqClass order [source c,target c,idA])>1 then error ("Fatal: shiftRight ("++showHS r++")\n"++showADL r++"\nin calculation of idC\n"++show (eqClass order [source c,target c,idA])) else
          source c `lub` target c `lub` idA
    idA = if length (eqClass order (target a:map source ants))>1 then error ("Fatal: shiftRight ("++showHS r++")\n"++showADL r++"\nin calculation of idA\n"++show (eqClass order (target a:map source ants))) else
          foldr lub (target a) (map source ants)
    signs (as,cs) = [( -- if null as then source (head cs) else
                       if source (head cs) `order` source (head as)
                       then source (head cs) `lub` source (head as)
                       else error ("Fatal: shiftRight ("++showHS r++")\n"++showADL r++"\nin calculation of source (head cs) `lub` source (head as) with cs="++show (map showS cs)++" and as="++show (map showS as)++"\n")
                     , -- if null as then target (last cs) else
                       if target (last cs) `order` target (last as)
                       then target (last cs) `lub` target (last as)
                       else error ("Fatal: shiftRight ("++showHS r++")\n"++showADL r++"\nin calculation of target (head cs) `lub` target (head as) with cs="++show (map showS cs)++" and as="++show (map showS as)++"\n")
                     ) ]
    move []  cs 
     = [([flp l],init cs)| sur (multiplicities l) && inj (multiplicities l)] ++
       [([flp h],tail cs)| fun (multiplicities h) && tot (multiplicities h)]
       where h=head cs; l=last cs
    move [a] cs = [([a],cs)]
    move as cs
     = [ts| sur (multiplicities h) && inj (multiplicities h), ts<-move (tail as) ([flp h]++cs)]++
       [ts| fun (multiplicities l) && tot (multiplicities l), ts<-move (init as) (cs++[flp l])]
       where h=head as; l=last as
  shiftRight r = [r]

  shrink (Fu [f]) = shrink f
  shrink (Fi [f]) = shrink f
  shrink f        = f

  insertRules, deleteRules :: [Rule] -> [Rule]
  insertRules rs = [ r | rule<-rs, r@(Ru c antc pos (F [t]) expla sgn nr)<-calcul shiftLeft rule]
  deleteRules rs = [ r | rule<-rs, r@(Ru c (F [t]) pos cons expla sgn nr)<-calcul shiftRight rule]

  data DBrule = DBR Char Rule Morphism [Int] deriving Show
  instance Eq DBrule where
   DBR c r m _ == DBR c' r' m' _ = c==c' && r==r' && m==m'

  frMorph (DBR _ r m _) = m
  toMorph (DBR _ (Ru _ antc _ (F [Tm m _]) _ _ _) _ _) = m

  dbIns rs   = [DBR c r m [nr r|DBR c r m _<-cl]|cl<-eqClass (==) cls, DBR c r m _<-take 1 cl]
   where cls = [ DBR 'I' r m [nr]
               | r@(Ru c antc pos (F [_]) expla sgn nr)<-concat [calcul shiftLeft r| r<-rs]
               , m<-mors antc
               ]
  dbDel rs   = [DBR c r m [nr r|DBR c r m _<-cl]|cl<-eqClass (==) cls, DBR c r m _<-take 1 cl]
   where cls = [ DBR 'D' r m [nr]
               | r@(Ru c (F [_]) pos cons expla sgn nr)<-concat [calcul shiftRight r| r<-rs]
               , m<-mors cons
               ]

-- Pre: s `elem` signatures antc                       &&
--      source r==source t && target s==target t

  dbNrs str nrs = ["    ",str," (by rule"++
                                  (if length nrs>1
                                   then "s "++commaEng "and" (map show nrs)
                                   else " "++show (head nrs))++")"]

  dbShow :: [DBrule] -> String
  dbShow dbrs
   = (if null dbs then "" else
      "ON insertEvent ("++s++","++t++") IN "++show m++showSign [source m,target m]++" DO<BR />\n"++
      htmlTable dbs []++"\n<P>\n")++
     (if null dbs' then "" else
      "ON deleteEvent ("++s++","++t++") IN "++show m++showSign [source m,target m]++" DO<BR />\n"++
      htmlTable dbs' [])
     where dbs   = [dbShw d| d@(DBR 'I' _ _ _)<-dbrs, not (null (dbShw d))]
           dbs'  = [dbShw d| d@(DBR 'D' _ _ _)<-dbrs, not (null (dbShw d))]
           [s,t] = mkVar [] [source m,target m]
           (DBR _ _ m _):rest = dbrs

  dbShw (DBR 'I' r@(Ru _ antc _ (F [Tm m@(Mph nmm pos atts sgn signr) _]) _ _ nr) m' nrs)
   = dbNrs ("INSERT ("++showADL expr'++") IN "++showADL m) nrs
     where [s,t] = mkVar [] [source m',target m']
           expr' = subst (m',F [Tm ins True]) antc
           ins   = Mph ("{("++s++","++t++")}") pos atts sgn signr
  dbShw (DBR 'I' r@(Ru _ antc _ (F [Tf m@(Mph nmm pos atts sgn signr) _]) _ _ nr) m' nrs)
   = dbNrs ("INSERT ("++showADL expr'++") IN "++showADL m) nrs
     where [s,t] = mkVar [] [source m',target m']
           expr' = subst (m',F [Tm ins True]) antc
           ins   = Mph ("{("++s++","++t++")}") pos atts sgn signr
  dbShw (DBR 'I' r@(Ru _ antc _ (F [Tm (Id _ _ _) _]) _ _ _) _ _) = []
  dbShw (DBR 'D' r@(Ru _ (F [Tm (Id _ _ _) _]) _ cons _ _ _) _ _) = []
  dbShw (DBR 'D' r@(Ru _ (F [Tm m@(Mph nmm pos atts sgn signr) _]) _ cons _ _ nr) m' nrs)
   = dbNrs ("DELETE ("++showADL expr'++") IN "++showADL m) nrs
     where [s,t] = mkVar [] [source m',target m']
           expr' = subst (m',F [Tm del True]) cons
           del   = Mph ("{("++s++","++t++")}") pos atts sgn signr
  dbShw (DBR 'D' r@(Ru _ (F [Tf m@(Mph nmm pos atts sgn signr) _]) _ cons _ _ nr) m' nrs)
   = dbNrs ("DELETE ("++showADL expr'++") IN "++showADL m) nrs
     where [s,t] = mkVar [] [source m',target m']
           expr' = subst (m',F [Tm del True]) cons
           del   = flp (Mph ("{("++t++","++s++")}") pos atts sgn signr)
  dbShw r = error ("Fatal: non-exhaustive pattern in dbShw("++show r++")")

  calcul :: (Rule->[Rule]) -> Rule -> [Rule]
  calcul shiftTerms (Ru 'E' antc pos cons expla sgn nr)
   = (concat.map shiftTerms)
     ([ Ru 'I' antc pos (shrink f) expla sgn nr
      | Fu [fu]<-[normFu cons], Fi fis<-[fu], f<-fis]++
      [ Ru 'I' cons pos (shrink f) expla sgn nr
      | Fu [fu]<-[normFu antc], Fi fis<-[fu], f<-fis])
  calcul shiftTerms (Ru 'I' antc pos cons expla sgn nr)
   = (concat.map shiftTerms)
     [Ru 'I' antc pos (shrink f) expla sgn nr| Fu [fu]<-[normFu cons], Fi fis<-[fu], f<-fis]
  calcul shiftTerms x = [x]

--      con 'A' (Ru c antc pos (F [t]) expla sgn nr) = t
--      con 'C' (Ru c (F [t]) pos cons expla sgn nr) = t
--      con _ r = error ("Fatal: con applied to "++showADL r++"\nSpecifically: "++showHS r)
--      uni _ [r] = r
--      uni 'A' rs  = Ru c (normFu (Fu [f| Ru c antc pos cons expla sgn nr<-rs
--                                       , Fu fs<-[normFu antc], f<-fs])) pos cons expla sgn nr
--       where Ru c antc pos cons expla sgn nr = head rs
--      uni 'C' rs  = Ru c (normFu (Fu [f| Ru c antc pos cons expla sgn nr<-rs
--                                       , Fu fs<-[normFu antc], f<-fs])) pos cons expla sgn nr
--       where Ru c antc pos cons expla sgn nr = head rs

-- For testing purposes

--  splitStr f (x:xs) | f x  = (x:yes, no)
--                    | True = (yes, x:no)
--                    where (yes,no) = splitStr f xs
--  splitStr f [] = ([],[])
--  main
--   = do { a <- getArgs
--        ; let (switches,args) = splitStr ((=="-").take 1) a
--        ; putStr ("Arguments: "++chain ", " args++"\nSwitches: "++chain ", " switches)
--        ; if length args==0 then putStr ("Please provide a filename (.adl) and a context name") else
--     do { let fn = args!!0; contextname = args!!1
--              (fnPrefix,fnSuffix) = break ('.' ==) fn
--              fnFull = if null fnSuffix then (fn ++ ".adl") else fn
--        ; inp<-readFile fnFull
--        ; putStr ("\n"++fnFull++" is read.")
--        ; slRes <- parseIO pArchitecture (scan keywordstxt keywordsops specialchars opchars fnFull initPos inp)
--        ; putStr ("\n"++fnFull++" has been parsed.")
--        ; let (contexts,errs) = sem_Architecture slRes
--        ; (putStr . ('\n':) . chain "\n\n" . map shR . rules. head) contexts
--        }}
--  fakeLub a b = a
--  shR r = showADL r++"\n  "++chain "\n  " (map showADL (insertRules [r]++deleteRules [r])) -- ++"\n  "++showHS r++"\n  "++showHS (normR r)
