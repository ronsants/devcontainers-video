  module Restrictions (assemble, Restriction(Forall, Exists, Implies, Equiv, Conj, Disj, Not, Rel, Funs)) where
  import Char
  import Auxiliaries
--  import UU_Scanner
  import Classification
  import Typology
  import CC_aux
  import Html

Het opmaken van regels in predikaatlogica

  data Restriction
   = Forall [(String,Concept)] Restriction   |
     Exists [(String,Concept)] Restriction   |
     Implies Restriction Restriction         |
     Equiv Restriction Restriction           |
     Conj [Restriction]                      |
     Disj [Restriction]                      |
     Not Restriction                         |
     Rel [Expression] Expression [Expression] String String    |
     Funs String [Morphism]                         deriving Show

  assembleFIns :: (Morphism,String,String) -> [String] -> Expression -> (String->String->Restriction,[String])
  assembleFIns (m,s,t) exclVars (F ts)
   = if null ics then ((\s->(\t->head (frels s t))), exclVars) else
     ((\s->(\t->Exists (zip ivs ics) (Conj (frels s t)))), ivs++exclVars)
     where -- To do: prove that frels s t can never be empty (which is highly likely) and remove the check.
      res       = pars3 (exclVars++ivs) (split ts)
      frels s t = if null rvws then error ("Fatal: frels "++show s++" "++show t++" is empty.") else rvws
                  where rvws = [r v w|((r,exV,t',s'),v,w)<-zip3 res ([s]++ivs) (ivs++[t]) ]
      ics       = [if v `order` w then v `lub` w else error("Fatal: assembleFIns")
                  |(v,w)<-zip [s|(r,exV,s,t)<-tail res] [t|(r,exV,s,t)<-init res]]
      ivs       = mkVar exclVars ics
  assembleFIns (m,s,t) exclVars (Fu fs)
   = ((\s->(\t->Disj [fst (assembleFIns (m,s,t) exclVars f) s t|f<-fs])), exclVars)
  assembleFIns (m,s,t) exclVars (Fi fs)
   = ((\s->(\t->Conj [fst (assembleFIns (m,s,t) exclVars f) s t|f<-fs])), exclVars)

  instance Show Restriction where
    showsPrec p r = showString (charshow r)

  charshow (Forall vars restr)
   = charVars "For all" vars ++ charshow restr
  charshow (Exists vars restr)
   = charVars "Exists" vars  ++ charshow restr
  charshow (Implies antc conseq)
   = charshow antc++" ==> "  ++ charshow conseq
  charshow (Equiv lhs rhs)
   = charshow lhs++" <=> "   ++ charshow rhs
  charshow (Conj rs)
   = if null rs then "" else
     chain " & " (map charshow rs)
  charshow (Disj rs)
   = if null rs then "" else
     chain " || " (map charshow rs)
  charshow (Not rs) = "not "++charshow rs
  charshow (Rel lhs (Tm m compl) rhs d c)
   = (if compl then "" else "not(")++
     charshow (Funs d left)++" "++name m++" "++charshow (Funs c right)++
     (if compl then "" else ")")
     where left = [m| t<-lhs, m<-mors t]; right = [m| t<-reverse rhs, m<-mors t]
  charshow (Rel lhs (Tf m compl) rhs d c)
   = (if compl then "" else "not(")++
     charshow (Funs c left)++" "++name m++" "++charshow (Funs d right)++
     (if compl then "" else ")")
     where right = [m| t<-lhs, m<-mors t]; left = [m| t<-reverse rhs, m<-mors t]
  charshow (Rel lhs (Tc f compl) rhs d c)
   = (if compl then "" else "not(")++
     charshow (Funs c left)++" temporary Quirk: ("++show f++") "++charshow (Funs d right)++
     (if compl then "" else ")")
     where right = [m| t<-lhs, m<-mors t]; left = [m| t<-reverse rhs, m<-mors t]
  charshow (Funs x []) = x
  charshow (Funs x (Id pos atts c:ms)) = charshow (Funs x ms)
  charshow (Funs x (m:ms))             = charshow (Funs (name m++"("++x++")") ms)

  charVars q vs
   = if null vs then "" else
     q++" "++chain "; " [chain ", " vs++"::"++dType | (vs,dType)<-vss]++": "
     where
      vss = [(map fst vs,show(snd (head vs))) |vs<-eqCl snd vs]

Testing a little bit

  did        = Sgn "did"        "Actor"  "Event"    [Sur, Inj] "Object " " has caused event " "" [] posNone
  mdid       = Mph "did" posNone [] (C "Actor",C "Event") did
  creation   = Sgn "creation"  "Object"  "Time"     [Uni, Tot] "Object " " was created on time " "" [] posNone
  mcreation  = Mph "creation" posNone [] (C "Object",C "Time") creation
  elimination= Sgn "elimination" "Object" "Time"    [Uni]      "Object " " was eliminated on time " "" [] posNone
  now        = Sgn "now"       "Object"  "Time"     [Uni, Tot] "The clock of (object) " " shows time " "" [] posNone
  location   = Sgn "location"  "Object"  "Location" [Uni, Tot] "Object " " is located at " "" [] posNone
  occured    = Sgn "occured"   "Event"   "Time"     [Uni, Tot] "Event " " occurred on time " "" [] posNone
  moccured   = Mph "occured" posNone [] (C "Event",C "Time") occured
  ltEq       = Sgn "ltEq"       "Time"    "Time"    []         "Moment " " occurred before moment " "" [] posNone
  mltEq      = Mph "ltEq" posNone [] (C "Time",C "Time") ltEq

  r = Ru c (F [Tm mdid True])
         posNone 
         (F [ Tm mcreation True   -- (C "Object",C "Time")
            , Tm mltEq True       -- (C "Time",C "Time")
            , Tf moccured True    -- (C "Time",C "Event")
            ]                     -- (C "Actor",C "Event"))
         "" (C "Actor",C "Event") 0
  s = Ru c (F [Tf bevat1 True     -- (C "Handeling",C "Processtap")
              ])
         posNone
         (F [ Tm repr1 True       -- (C "Handeling",C "Usecase")
            , Tf bevat2 True      -- (C "Usecase",C "Business_Usecase")
            , Tf repr2 True       -- (C "Business_Usecase",C "Processtap")
            ])
         "" (C "Handeling",C "Processtap") 0

  bevat1 = Mph "bevat" posNone [] (C "Processtap",C "Handeling") mbevat1
  mbevat1 = Sgn "bevat" "Processtap" "Handeling" [Sur,Tot] "Processtap " " bevat handeling " "" [] posNone
  bevat2 = Mph "bevat" posNone [] (C "Business_Usecase",C "Usecase") mbevat2
  mbevat2 = Sgn "bevat" "Business_Usecase" "Usecase" [Sur,Tot] "Business_Usecase " " bevat usecase " "" [] posNone
  repr1 = Mph "repr" posNone [] (C "Handeling",C "Usecase") mrepr1
  mrepr1 = Sgn "repr" "Handeling" "Usecase" [Uni,Tot] "Handeling " " wordt ondersteund door UC " "" [] posNone
  repr2 = Mph "repr" posNone [] (C "Processtap",C "Business_Usecase") mrepr2
  mrepr2 = Sgn "repr" "Processtap" "Business_Usecase" [Uni,Sur,Tot] "Processtap " " wordt ondersteund door BUC " "" [] posNone

  inBD  = Mph "in" posNone [] (C "Bericht",C "Dossier") mor_inBerichtDossier
  test1 = Ru 'E' inBD posNone
             (Fi [F [Tc (F [ Tm (Mph "rel" posNone [] (C "Bericht",C "Relatie") mor_relBerichtRelatie) True
                           , Tf (Mph "klantdossier" posNone [] (C "Dossier",C "Relatie") mor_klantdossierDossierRelatie) True
                           ]) True
                    ]
                 ,F [Tc (F [ Tm (Mph "contractnr" posNone [] (C "Bericht",C "Polis") mor_contractnrBerichtPolis) True
                           , Tm (Mph "verzekeringnemer" posNone [] (C "Polis",C "Relatie") mor_verzekeringnemerPolisRelatie) True
                           , Tf (Mph "klantdossier" posNone [] (C "Dossier",C "Relatie") mor_klantdossierDossierRelatie) True
                           ]) True
                    ]
                 ])
             "Berichten worden opgeslagen in de klantdossiers van verzekeringnemer(s) en de relatie zelf."
             (C "Bericht",C "Dossier") 29

*Restrictions assemble (==) test1
Forall [("b",Bericht),("d",Dossier)]
       (Equiv (Rel [in] =[Dossier] [] "b" "d")
              (Conj [Exists [] (Conj [Exists [] (Conj [Rel [rel] =[Relatie] [FLP klantdossier] "Bericht" "Dossier"
                                                      ]
                                                 )
                                     ]
                               )
              ,Exists [] (Conj [Exists *** Exception: <<loop>

  mor_inBerichtDossier             = Sgn "in" "Bericht" "Dossier" [Uni,Tot] "Bericht " " is toegekend aan dossier " "" [] posNone
  mor_relBerichtRelatie            = Sgn "rel" "Bericht" "Relatie" [Uni,Tot] "Bericht " "is een communicatie met Relatie " "" [] posNone
  mor_klantdossierDossierRelatie   = Sgn "klantdossier" "Dossier" "Relatie" [Uni,Tot,Inj] "Dossier " " hoort bij " "" [] posNone
  mor_contractnrBerichtPolis       = Sgn "contractnr" "Bericht" "Polis" [] "Bericht " " gaat over contract " "" [] posNone
  mor_verzekeringnemerPolisRelatie = Sgn "verzekeringnemer" "Polis" "Relatie" [Uni,Inj] "Contract " " is van Relatie " "" [] posNone

  test2 = Ru 'E' inBD posNone 
             (F [Tc (Fi [F [Tm (Mph "rel" posNone [] (C "Bericht",C "Relatie") mor_relBerichtRelatie) True
                           ]
                        ,F [Tm (Mph "contractnr" posNone [] (C "Bericht",C "Polis") mor_contractnrBerichtPolis) True
                           ,Tm (Mph "verzekeringnemer" posNone [] (C "Polis",C "Relatie") mor_verzekeringnemerPolisRelatie) True
                           ]
                        ]
                     ) True
                ,Tf (Mph "klantdossier" posNone [] (C "Dossier",C "Relatie") mor_klantdossierDossierRelatie) True
                ]
             )
             "Berichten worden opgeslagen in de klantdossiers van verzekeringnemer(s) en de relatie zelf."
             (C "Bericht",C "Dossier") 29

  auth =    Mph "auth" posNone [] (C "Rol",C "Taak") mor_authRolTaak
  mor_authRolTaak          = Sgn "auth"        "Rol"  "Taak"   []        "Rol " " is geautoriseerd om taak " " uit te voeren" [] posNone
  mor_responsibleRolProces = Sgn "responsible" "Rol"  "Proces" []        "Rol " " is volgens de R van RACI in proces " " verantwoordelijk voor de uitvoering" [] posNone
  mor_inTaakProces         = Sgn "in"          "Taak" "Proces" [Uni,Tot] "Het procesmodel beschrijft taak " " in proces " "" [] posNone
  mor_consultingRolProces  = Sgn "consulting"  "Rol"  "Proces" []        "Volgens de RACI matrix heeft een " " in proces " " een adviserende rol" [] posNone

  tryRule = Ru 'E' auth posNone
            (Fi [F [Tm (Mph "responsible" posNone [] (C "Rol",C "Proces") mor_responsibleRolProces) True
                   ,Tm (Mph "in" posNone [] (C "Taak",C "Proces") mor_inTaakProces) True
                   ]
                ,F [Tm (Mph "consulting" posNone [] (C "Rol",C "Proces") mor_consultingRolProces) True
                   ,Tf (Mph "in" posNone [] (C "Taak",C "Proces") mor_inTaakProces) True
                   ]
                ])
            "Een medewerker is geautoriseerd op basis van zijn rol als er een R of C in de RACI matrix staat"
            (C "Rol",C "Taak") 4

Main assemble (==) r
For all a::Actor; e::Event: a did e == creation(a) ltEq occured(e)

Main rule ([ex did] ==> [ex creation, ex ltEq, inv occured])
E [did] : Actor<--Event = E [.creation,ltEq,.occured] : (Object,Event)   

Main assemble (rule ([ex did] ==> [ex creation, ex ltEq, inv occured]))
For all o::Object; e::Event: o did e == .creation(o) ltEq .occured(e)

Main rule ([inv creation, ex now] ==> [ex ltEq])
E [.creation,.now] = E [ltEq] : (Time,Time) 

Main assemble (rule ([inv creation, ex now] ==> [ex ltEq]))
For all t, t'::Time; o::Object: t = .creation(o) & .now(o) = t' == t ltEq t'

  main= do {putStr (show p)}

  mor_m = Sgn "m" "A" "A" [] "" "" "" [] posNone
  mor_n = Sgn "n" "A" "A" [] "" "" "" [] posNone
  mor_o = Sgn "o" "A" "A" [] "" "" "" [] posNone
  p = Ru 'E'
        (Mph "m" posNone [] (C "A",C "A") mor_m)
        posNone expr "" (C "A",C "A") 0

  expr = F tss
  tss  =   [ Tm
               (Mph "m" posNone [] (C "A",C "A") mor_m) True
           , Tc expr2 True
           ]

  expr1 =       F [ Tm
                      (Mph "n" posNone [] (C "A",C "A") mor_n) True
                  , Tm
                      (Mph "o" posNone [] (C "A",C "A") mor_o) True
                  ]
  expr2 =       F [ Tm
                      (Mph "n" posNone [] (C "A",C "A") mor_n) True
                  ]
                 
        
  (me,exV) = pars0 (==) ["a","b"] [Tm (Mph "m" posNone [] (C "A",C "A") mor_m) True]

  assemble (==) p
=      {    assembleF (==) ["a","a'"] expr1
         =
            (Exists [("a''",A)] (Conj [Rel [] n [] "a" "a''",Rel [] o [] "a''" "a'"]),["a''","a","a'"])
   
       ;  me "a" "a'" = Rel [] m [] "a" "a'"
       
  Forall [("a",A),("a'",A)] (Equiv (Rel [] m [] "a" "a'") (Exists [("a''",A)] (Conj [Rel [] m [] "a" "a''",Exists *** Exception: <<loop>

  assemble :: Rule -> Restriction
  assemble r@(Ru 'E' defd pos expr expla (sourc,targ) nr) 
   = Forall [(s,sourc),(t,targ)]
      (Equiv (ra s t) (rc s t))
     where
      (ra,avars) = assembleF [s,t] defd
      (rc,cvars) = assembleF avars expr
      [s,t] = mkVar [] [sourc, targ]
  assemble r@(Ru 'I' antc pos cons expla (sourc,targ) nr)
   | isIdent antc = Forall [(s,sourc)] (rb s s)
   | otherwise   = transform (Forall [(s,sourc), (t,targ)] (Implies (ra s t) (rc s t)))
   where
      (ra,avars) = assembleF [s,t] antc
      (rb,bvars) = assembleF [s] cons
      (rc,cvars) = assembleF avars cons
      [s,t] = mkVar [] [sourc, targ]
      transform (Forall vs (Implies (Exists es antc) cons)) = Forall (vs++es) (Implies antc cons)
      transform expr = expr

  assembleF :: [String] -> Expression -> (String->String->Restriction,[String])
  assembleF exclVars (F ts)
   = if null ics then ((\s->(\t->head (frels s t))), exclVars) else
     ((\s->(\t->Exists (zip ivs ics) (Conj (frels s t)))), ivs++exclVars)
     where
      res       = pars3 (exclVars++ivs) (split ts)
      frels s t = [r v w|((r,exV,t',s'),v,w)<-zip3 res ([s]++ivs) (ivs++[t]) ]
      ics       = [if v `order` w then v `lub` w else error("Fatal: assembleF")
                  |(v,w)<-zip [s|(r,exV,s,t)<-tail res] [t|(r,exV,s,t)<-init res]]
      ivs       = mkVar exclVars ics
  assembleF exclVars (Fu fs)
   = ((\s->(\t->Disj [fst (assembleF exclVars f) s t|f<-fs])), exclVars)
  assembleF exclVars (Fi fs)
   = ((\s->(\t->Conj [fst (assembleF exclVars f) s t|f<-fs])), exclVars)

  data Notation = Flr | Frl | Rn | Wrap deriving Eq   -- yields notations y=r(x)  |  x=r(y)  |  x r y  | exists ... respectively.

  pars3 exV (r: s: t: ts)
   | denotes r==Flr && denotes s==Rn && denotes t==Frl
      = (Rel r (head s) t, exV, source (head r), target (last t)): pars3 exV ts
   | otherwise = pars2 exV (r:s:t:ts)
  pars3 exV ts = pars2 exV ts -- for lists shorter than 3
  pars2 exV (r: s: ts)
   | denotes r==Flr && denotes s==Frl
               = ( Rel r (Tm (Id posNone [] (source (head s))) True) s
                 , exV, source (head r), target (last s)): pars3 exV ts
   | denotes r==Flr && denotes s==Rn
               = (Rel r (head s) [], exV, source (head r), target (last s)): pars3 exV ts
   | denotes s==Frl && denotes r==Rn
               = (Rel [] (head r) s, exV, source (head r), target (last s)): pars3 exV ts
   | otherwise = pars1 exV (r:s:ts)
  pars2 exV ts = pars1 exV ts -- for lists shorter than 2
  pars1 exV (r: ts)
   = (restr, exV', source (head r), target (last r)): pars3 exV ts
     where (restr,exV') = pars0 exV r
  pars1 exV [] = []
  pars0 exV r
   | denotes r==Flr = (Rel r (Tm (Id posNone [] (target (last r))) True) [], exV)
   | denotes r==Frl = (Rel [] (Tm (Id posNone [] (target (head r))) True) r, exV)
   | denotes r==Rn  = (Rel [] (head r) [], exV)
   | otherwise      = assembleF exV f
                      where [Tc f c] = r

  denote :: Expression -> Notation
  denote (Tm (Mph _ _ _ _ s) _)
   | null([Uni,Inj,Tot,Sur] >- ps)  = Rn
   | Uni `elem` ps && Tot `elem` ps = Flr
   | Inj `elem` ps && Sur `elem` ps = Frl
   | otherwise                      = Rn
   where ps = multiplicities s
  denote (Tm (Id _ _ _) _) = Rn
  denote (Tf (Mph _ _ _ _ s) _)
   | null([Uni,Inj,Tot,Sur] >- ps)  = Rn
   | Inj `elem` ps && Sur `elem` ps = Flr
   | Uni `elem` ps && Tot `elem` ps = Frl
   | otherwise                      = Rn
   where ps = multiplicities s
  denote (Tf (Id _ _ _) _) = Rn
  denote (Tc _ _)                   = Wrap
  denotes = denote . head

Property: each element of (split ts) is a nonempty list. (Proof by induction over the recursion of split)

  split []  = []
  split [t] = [[t]]
  split (t:t':ts)
   = if denote t `eq` Wrap      then (t:spl):spls else
     if denote t `eq` denote t' then (t:spl):spls else
                                     [t]:spl:spls
     where 
       spl:spls = split (t':ts)
       Flr `eq` Flr = True
       Frl `eq` Frl = True
       x `eq` y     = False

Html stuff

  instance HTML Restriction where
    hshow (Forall vars restr)
     = charVars "&forall;" {- "<IMG SRC=\"treemenutils/forall.gif\" ALT=\"For all \">" -} vars ++ hshow restr
    hshow (Exists vars restr)
     = charVars "&exist;" {- "<IMG SRC=\"treemenutils/exists.gif\" ALT=\"Exists \">" -} vars  ++ hshow restr
    hshow (Implies antc cons)
     = "<BR />\n"++hshow antc++" <IMG SRC=\"treemenutils/arright.gif\" ALT=\"implies \"> "  ++ hshow cons
    hshow (Equiv lhs rhs)
     = "<BR />\n"++hshow lhs++" <IMG SRC=\"treemenutils/arboth.gif\" ALT=\"Equivalent \"> "   ++ hshow rhs
    hshow (Conj [r]) = hshow r
    hshow (Conj rs)
     = if null rs then "" else
       chain " &and; " {- " <IMG SRC=\"treemenutils/and.gif\" ALT=\"and \"> " -} (map hsh rs)
    hshow (Disj [r]) = hshow r
    hshow (Disj rs)
     = if null rs then "" else
       chain " &or; " {- " <IMG SRC=\"treemenutils/or.gif\" ALT=\"or \"> " -} (map hsh rs)
    hshow (Not rs) = " &not; " {- "<IMG SRC=\"treemenutils/not.gif\" ALT=\"not \"> " -} ++hshow rs
    hshow (Rel lhs (Tm m compl) rhs d c)
     = "<A TITLE=\""++helptext d left m c right++"\">"++
       hshow (Funs d left)++" "++hshow m++" "++hshow (Funs c right)++"</A>"
       where left = [m| t<-lhs, m<-mors t]; right = [m| t<-reverse rhs, m<-mors t]
    hshow (Rel lhs (Tf m compl) rhs d c)
     = "<A TITLE=\""++helptext c left m d right++"\">"++
       hshow (Funs c left)++" "++hshow m++" "++hshow (Funs d right)++"</A>"
       where right = [m| t<-lhs, m<-mors t]; left = [m| t<-reverse rhs, m<-mors t]
    hshow (Rel lhs (Tc f compl) rhs d c)
     = error "Rel lhs (Tc f compl) rhs"
    hshow (Funs x [])         = x
    hshow (Funs x (Id _ _ _:ms)) = hshow (Funs x ms)
    hshow (Funs x (m:ms))        = hshow (Funs (hshow m++"("++x++")") ms)

  helptext :: String -> [Morphism] -> Morphism -> String -> [Morphism] -> String
  helptext a ams (Id pos atts nm)  b bms = funnotate a ams ++ " = " ++ funnotate b bms
  helptext a ams (Mph _ _ _ _ s) b bms = applyM s (funnotate a ams) (funnotate b bms)++shCard s
   where
     shCard s
      | null (multiplicities s) = ""
      | otherwise        = "\n(Properties: "++(chain ", " . map show ) (multiplicities s)++")"

  funnotate x      []       = x
  funnotate x (Id _ _ _:ms) = funnotate x ms
  funnotate x (m:ms)        = funnotate (hshow m++"("++x++")") ms

  hsh r@(Exists vars restr)
   = "("++hshow r++")"
  hsh r@(Forall vars restr)
   = "("++hshow r++")"
  hsh r@(Conj [r']) = hshow r'
  hsh r@(Conj restr)
   = "("++hshow r++")"
  hsh r@(Disj [r']) = hshow r'
  hsh r@(Disj restr)
   = "("++hshow r++")"
  hsh r = hshow r
