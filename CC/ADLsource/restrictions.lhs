>  module Restrictions (assemble, Restriction(Forall, Exists, Implies, Equiv, Conj, Disj, Not, Rel, Funs)) where
>  import Char
>  import Auxiliaries
>--  import UU_Scanner
>  import Classification
>  import Typology
>  import CC_aux

Het opmaken van regels in predikaatlogica

>  data Restriction
>   = Forall [(String,Concept)] Restriction   |
>     Exists [(String,Concept)] Restriction   |
>     Implies Restriction Restriction         |
>     Equiv Restriction Restriction           |
>     Conj [Restriction]                      |
>     Disj [Restriction]                      |
>     Not Restriction                         |
>     Rel [Term] Term [Term] String String    |
>     Funs String [Morph]                         deriving Show

  instance Show Restriction where
    showsPrec p r = showString (charshow r)

>  charshow (Forall vars restr)
>   = charVars "For all" vars ++ charshow restr
>  charshow (Exists vars restr)
>   = charVars "Exists" vars  ++ charshow restr
>  charshow (Implies antc conseq)
>   = charshow antc++" ==> "  ++ charshow conseq
>  charshow (Equiv lhs rhs)
>   = charshow lhs++" <=> "   ++ charshow rhs
>  charshow (Conj rs)
>   = if null rs then "" else
>     chain " & " (map charshow rs)
>  charshow (Disj rs)
>   = if null rs then "" else
>     chain " || " (map charshow rs)
>  charshow (Not rs) = "not "++charshow rs
>  charshow (Rel lhs (Tm m (cType,dType)) rhs d c)
>   = charshow (Funs d left)++" "++name m++" "++charshow (Funs c right)
>     where left = [m| t<-lhs, m<-mors t]; right = [m| t<-reverse rhs, m<-mors t]
>  charshow (Rel lhs (Tf m (cType,dType)) rhs d c)
>   = charshow (Funs c left)++" "++name m++" "++charshow (Funs d right)
>     where right = [m| t<-lhs, m<-mors t]; left = [m| t<-reverse rhs, m<-mors t]
>  charshow (Rel lhs (Tc f (cType,dType)) rhs d c)
>   = charshow (Funs c left)++" temporary Quirk: ("++show f++") "++charshow (Funs d right)
>     where right = [m| t<-lhs, m<-mors t]; left = [m| t<-reverse rhs, m<-mors t]
>  charshow (Funs x []) = x
>  charshow (Funs x (Id pos atts c:ms)) = charshow (Funs x ms)
>  charshow (Funs x (m:ms))             = charshow (Funs (name m++"("++x++")") ms)

>  charVars q vs
>   = if null vs then "" else
>     q++" "++chain "; " [chain ", " vs++"::"++dType | (vs,dType)<-vss]++": "
>     where
>      vss = [(map fst vs,show(snd (head vs))) |vs<-eqCl snd vs]

Testing a little bit

  did        = Mor "did"        "Actor"  "Event"    [Sur, Inj] "Object " " has caused event " "" [] posNone
  mdid       = Mph "did" posNone [] (C "Actor",C "Event") did
  creation   = Mor "creation"  "Object"  "Time"     [Uni, Tot] "Object " " was created on time " "" [] posNone
  mcreation  = Mph "creation" posNone [] (C "Object",C "Time") creation
  elimination= Mor "elimination" "Object" "Time"    [Uni]      "Object " " was eliminated on time " "" [] posNone
  now        = Mor "now"       "Object"  "Time"     [Uni, Tot] "The clock of (object) " " shows time " "" [] posNone
  location   = Mor "location"  "Object"  "Location" [Uni, Tot] "Object " " is located at " "" [] posNone
  occured    = Mor "occured"   "Event"   "Time"     [Uni, Tot] "Event " " occurred on time " "" [] posNone
  moccured   = Mph "occured" posNone [] (C "Event",C "Time") occured
  ltEq       = Mor "ltEq"       "Time"    "Time"    []         "Moment " " occurred before moment " "" [] posNone
  mltEq      = Mph "ltEq" posNone [] (C "Time",C "Time") ltEq

  fakeGE x y = x==y
  r = Hc (F [Tm mdid (C "Actor",C "Event")] (C "Actor",C "Event"))
         posNone 
         (F [Tm mcreation (C "Object",C "Time"), Tm mltEq (C "Time",C "Time"), Tf moccured (C "Time",C "Event")] (C "Actor",C "Event"))
         "" (C "Actor",C "Event") 0
  s = Hc (F [Tf bevat1 (C "Handeling",C "Processtap")] (C "Handeling",C "Processtap"))
         posNone
         (F [Tm repr1 (C "Handeling",C "Usecase")
            ,Tf bevat2 (C "Usecase",C "Business_Usecase")
            ,Tf repr2 (C "Business_Usecase",C "Processtap")] (C "Handeling",C "Processtap"))
         "" (C "Handeling",C "Processtap") 0

  bevat1 = Mph "bevat" posNone [] (C "Processtap",C "Handeling") mbevat1
  mbevat1 = Mor "bevat" "Processtap" "Handeling" [Sur,Tot] "Processtap " " bevat handeling " "" [] posNone
  bevat2 = Mph "bevat" posNone [] (C "Business_Usecase",C "Usecase") mbevat2
  mbevat2 = Mor "bevat" "Business_Usecase" "Usecase" [Sur,Tot] "Business_Usecase " " bevat usecase " "" [] posNone
  repr1 = Mph "repr" posNone [] (C "Handeling",C "Usecase") mrepr1
  mrepr1 = Mor "repr" "Handeling" "Usecase" [Uni,Tot] "Handeling " " wordt ondersteund door UC " "" [] posNone
  repr2 = Mph "repr" posNone [] (C "Processtap",C "Business_Usecase") mrepr2
  mrepr2 = Mor "repr" "Processtap" "Business_Usecase" [Uni,Sur,Tot] "Processtap " " wordt ondersteund door BUC " "" [] posNone

  inBD  = Mph "in" posNone [] (C "Bericht",C "Dossier") mor_inBerichtDossier
  test1 = Dc inBD posNone
             (Fi [F [Tc (F [ Tm (Mph "rel" posNone [] (C "Bericht",C "Relatie") mor_relBerichtRelatie) (C "Bericht",C "Relatie")
                           , Tf (Mph "klantdossier" posNone [] (C "Dossier",C "Relatie") mor_klantdossierDossierRelatie) (C "Relatie",C "Dossier")
                           ] (C "Bericht",C "Dossier")) (C "Bericht",C "Dossier")
                    ] (C "Bericht",C "Dossier")
                 ,F [Tc (F [ Tm (Mph "contractnr" posNone [] (C "Bericht",C "Polis") mor_contractnrBerichtPolis) (C "Bericht",C "Polis")
                           , Tm (Mph "verzekeringnemer" posNone [] (C "Polis",C "Relatie") mor_verzekeringnemerPolisRelatie) (C "Polis",C "Relatie")
                           , Tf (Mph "klantdossier" posNone [] (C "Dossier",C "Relatie") mor_klantdossierDossierRelatie) (C "Relatie",C "Dossier")
                           ] (C "Bericht",C "Dossier")) (C "Bericht",C "Dossier")
                    ] (C "Bericht",C "Dossier")
                 ] (C "Bericht",C "Dossier"))
             "Berichten worden opgeslagen in de klantdossiers van verzekeringnemer(s) en de relatie zelf."
             (C "Bericht",C "Dossier") 29

*Restrictions> assemble fakeGE test1
Forall [("b",Bericht),("d",Dossier)]
       (Equiv (Rel [in] =[Dossier] [] "b" "d")
              (Conj [Exists [] (Conj [Exists [] (Conj [Rel [rel] =[Relatie] [FLP klantdossier] "Bericht" "Dossier"
                                                      ]
                                                 )
                                     ]
                               )
              ,Exists [] (Conj [Exists *** Exception: <<loop>>

  mor_inBerichtDossier = Mor "in" "Bericht" "Dossier" [Uni,Tot] "Bericht " " is toegekend aan dossier " "" [] posNone
  mor_relBerichtRelatie = Mor "rel" "Bericht" "Relatie" [Uni,Tot] "Bericht " "is een communicatie met Relatie " "" [] posNone
  mor_klantdossierDossierRelatie = Mor "klantdossier" "Dossier" "Relatie" [Uni,Tot,Inj] "Dossier " " hoort bij " "" [] posNone
  mor_contractnrBerichtPolis = Mor "contractnr" "Bericht" "Polis" [] "Bericht " " gaat over contract " "" [] posNone
  mor_verzekeringnemerPolisRelatie = Mor "verzekeringnemer" "Polis" "Relatie" [Uni,Inj] "Contract " " is van Relatie " "" [] posNone

  test2 = Dc inBD posNone 
             (F [Tc (Fi [F [Tm (Mph "rel" posNone [] (C "Bericht",C "Relatie") mor_relBerichtRelatie) (C "Bericht",C "Relatie")
                           ] (C "Bericht",C "Relatie")
                        ,F [Tm (Mph "contractnr" posNone [] (C "Bericht",C "Polis") mor_contractnrBerichtPolis) (C "Bericht",C "Polis")
                           ,Tm (Mph "verzekeringnemer" posNone [] (C "Polis",C "Relatie") mor_verzekeringnemerPolisRelatie) (C "Polis",C "Relatie")
                           ] (C "Bericht",C "Relatie")
                        ] (C "Bericht",C "Relatie")
                     ) (C "Bericht",C "Relatie")
                ,Tf (Mph "klantdossier" posNone [] (C "Dossier",C "Relatie") mor_klantdossierDossierRelatie) (C "Relatie",C "Dossier")
                ] (C "Bericht",C "Dossier")
             )
             "Berichten worden opgeslagen in de klantdossiers van verzekeringnemer(s) en de relatie zelf."
             (C "Bericht",C "Dossier") 29

  auth =    Mph "auth" posNone [] (C "Rol",C "Taak") mor_authRolTaak
  mor_authRolTaak          = Mor "auth"        "Rol"  "Taak"   []        "Rol " " is geautoriseerd om taak " " uit te voeren" [] posNone
  mor_responsibleRolProces = Mor "responsible" "Rol"  "Proces" []        "Rol " " is volgens de R van RACI in proces " " verantwoordelijk voor de uitvoering" [] posNone
  mor_inTaakProces         = Mor "in"          "Taak" "Proces" [Uni,Tot] "Het procesmodel beschrijft taak " " in proces " "" [] posNone
  mor_consultingRolProces  = Mor "consulting"  "Rol"  "Proces" []        "Volgens de RACI matrix heeft een " " in proces " " een adviserende rol" [] posNone

  tryRule = Dc auth posNone
            (Fi [F [Tm (Mph "responsible" posNone [] (C "Rol",C "Proces") mor_responsibleRolProces) (C "Rol",C "Proces")
                   ,Tm (Mph "in" posNone [] (C "Taak",C "Proces") mor_inTaakProces) (C "Taak",C "Proces")
                   ] (C "Rol",C "Taak")
                ,F [Tm (Mph "consulting" posNone [] (C "Rol",C "Proces") mor_consultingRolProces) (C "Rol",C "Proces")
                   ,Tf (Mph "in" posNone [] (C "Taak",C "Proces") mor_inTaakProces) (C "Proces",C "Taak")
                   ] (C "Rol",C "Taak")
                ] (C "Rol",C "Taak"))
            "Een medewerker is geautoriseerd op basis van zijn rol als er een R of C in de RACI matrix staat"
            (C "Rol",C "Taak") 4

Main> assemble fakeGE r
For all a::Actor; e::Event: a did e ==> creation(a) ltEq occured(e)

Main> rule gE ([ex did] ==> [ex creation, ex ltEq, inv occured])
E [did] : Actor<--Event => E [.creation,ltEq,.occured] : (Object,Event)   

Main> assemble gE (rule gE ([ex did] ==> [ex creation, ex ltEq, inv occured]))
For all o::Object; e::Event: o did e ==> .creation(o) ltEq .occured(e)

Main> rule gE ([inv creation, ex now] ==> [ex ltEq])
E [.creation,.now] => E [ltEq] : (Time,Time) 

Main> assemble gE (rule gE ([inv creation, ex now] ==> [ex ltEq]))
For all t, t'::Time; o::Object: t = .creation(o) & .now(o) = t' ==> t ltEq t'

  main= do {putStr (show p)}

  mor_m = Mor "m" "A" "A" [] "" "" "" [] posNone
  mor_n = Mor "n" "A" "A" [] "" "" "" [] posNone
  mor_o = Mor "o" "A" "A" [] "" "" "" [] posNone
  p = Dc
        (Mph "m" posNone [] (C "A",C "A") mor_m)
        posNone expr "" (C "A",C "A") 0

  expr = F tss (C "A",C "A")
  tss  =   [ Tm
               (Mph "m" posNone [] (C "A",C "A") mor_m) (C "A",C "A")
           , Tc expr2 (C "A",C "A")
           ]

  expr1 =       F [ Tm
                      (Mph "n" posNone [] (C "A",C "A") mor_n) (C "A",C "A")
                  , Tm
                      (Mph "o" posNone [] (C "A",C "A") mor_o) (C "A",C "A")
                  ] (C "A",C "A")
  expr2 =       F [ Tm
                      (Mph "n" posNone [] (C "A",C "A") mor_n) (C "A",C "A")
                  ] (C "A",C "A")
                 
        
  (me,exV) = pars0 fakeGE ["a","b"] [Tm (Mph "m" posNone [] (C "A",C "A") mor_m) (C "A",C "A")]

  assemble fakeGE p
=      {    assembleF fakeGE ["a","a'"] expr1
         =
            (Exists [("a''",A)] (Conj [Rel [] n [] "a" "a''",Rel [] o [] "a''" "a'"]),["a''","a","a'"])
   
       ;  me "a" "a'" = Rel [] m [] "a" "a'"
       
  Forall [("a",A),("a'",A)] (Equiv (Rel [] m [] "a" "a'") (Exists [("a''",A)] (Conj [Rel [] m [] "a" "a''",Exists *** Exception: <<loop>>

>  assemble :: GenR -> Rule -> Restriction
>  assemble gE r@(Dc m pos expr expla (sourc,targ) nr) 
>   = Forall [(s,sourc),(t,targ)]
>      (Equiv (me s t) (rc s t))
>     where
>      (rc,cvars) = assembleF gE [s,t] expr
>      [s,t] = mkVar [] [sourc, targ]
>      (me,exV) = pars0 gE [s,t] [Tm m (source m, target m)]
>  assemble gE r@(Hc antc pos cons expla (sourc,targ) nr)
>   | isIdent antc = Forall [(s,sourc)] (rb s s)
>   | otherwise   = transform (Forall [(s,sourc), (t,targ)] (Implies (ra s t) (rc s t)))
>   where
>      (ra,avars) = assembleF gE [s,t] antc
>      (rb,bvars) = assembleF gE [s] cons
>      (rc,cvars) = assembleF gE avars cons
>      [s,t] = mkVar [] [sourc, targ]
>      transform (Forall vs (Implies (Exists es antc) cons)) = Forall (vs++es) (Implies antc cons)
>      transform expr = expr

>  assembleF :: GenR -> [String] -> Factor -> (String->String->Restriction,[String])
>  assembleF gE exclVars (F ts (sourc, targ))
>   = if null ics then ((\s->(\t->head (frels s t))), exclVars) else
>     ((\s->(\t->Exists (zip ivs ics) (Conj (frels s t)))), ivs++exclVars)
>     where
>      res       = pars3 gE (exclVars++ivs) (split ts)
>      frels s t = [r v w|((r,exV,t',s'),v,w)<-zip3 res ([s]++ivs) (ivs++[t]) ]
>      ics       = [lub gE v w
>                  |(v,w)<-zip [s|(r,exV,s,t)<-tail res] [t|(r,exV,s,t)<-init res]]
>      ivs       = mkVar exclVars ics
>  assembleF gE exclVars (Fu fs (sourc, targ))
>   = ((\s->(\t->Disj [fst (assembleF gE exclVars f) s t|f<-fs])), exclVars)
>  assembleF gE exclVars (Fi fs (sourc, targ))
>   = ((\s->(\t->Conj [fst (assembleF gE exclVars f) s t|f<-fs])), exclVars)

>  mkVar ex cs = mknew ex [[(toLower.head.name) c]|c<-cs]
>   where
>    mknew ex [] = []
>    mknew ex (x:xs) | x `elem` ex = mknew ex ((x++"'"):xs)
>                    | otherwise = x: mknew (ex++[x]) xs

>  data Notation = Flr | Frl | Rn | Wrap deriving Eq   -- yields notations y=r(x)  |  x=r(y)  |  x r y  | exists ... respectively.

>  pars3 gE exV (r: s: t: ts)
>   | denotes r==Flr && denotes s==Rn && denotes t==Frl
>      = (Rel r (head s) t, exV, source (head r), target (last t)): pars3 gE exV ts
>   | otherwise = pars2 gE exV (r:s:t:ts)
>  pars3 gE exV ts = pars2 gE exV ts -- for lists shorter than 3
>  pars2 gE exV (r: s: ts)
>   | denotes r==Flr && denotes s==Frl
>               = ( Rel r (Tm (Id posNone [] (source (head s))) (source (head s),source (head s))) s
>                 , exV, source (head r), target (last s)): pars3 gE exV ts
>   | denotes r==Flr && denotes s==Rn
>               = (Rel r (head s) [], exV, source (head r), target (last s)): pars3 gE exV ts
>   | denotes s==Frl && denotes r==Rn
>               = (Rel [] (head r) s, exV, source (head r), target (last s)): pars3 gE exV ts
>   | otherwise = pars1 gE exV (r:s:ts)
>  pars2 gE exV ts = pars1 gE exV ts -- for lists shorter than 2
>  pars1 gE exV (r: ts)
>   = (restr, exV', source (head r), target (last r)): pars3 gE exV ts
>     where (restr,exV') = pars0 gE exV r
>  pars1 gE exV [] = []
>  pars0 gE exV r
>   | denotes r==Flr = (Rel r (Tm (Id posNone [] (target (last r))) (source (last r),target (last r))) [], exV)
>   | denotes r==Frl = (Rel [] (Tm (Id posNone [] (target (head r))) (source (head r),target (head r))) r, exV)
>   | denotes r==Rn  = (Rel [] (head r) [], exV)
>   | otherwise      = assembleF gE exV f
>                      where [Tc f (sF,tF)] = r

>  denote :: Term -> Notation
>  denote (Tm (Mph _ _ _ _ (Mor _ _ _ ps _ _ _ _ _)) _)
>   | null([Uni,Inj,Tot,Sur] >- ps)  = Rn
>   | Uni `elem` ps && Tot `elem` ps = Flr
>   | Inj `elem` ps && Sur `elem` ps = Frl
>   | otherwise                      = Rn
>  denote (Tm (Id _ _ _) _) = Rn
>  denote (Tf (Mph _ _ _ _ (Mor _ _ _ ps _ _ _ _ _)) _)
>   | null([Uni,Inj,Tot,Sur] >- ps)  = Rn
>   | Inj `elem` ps && Sur `elem` ps = Flr
>   | Uni `elem` ps && Tot `elem` ps = Frl
>   | otherwise                      = Rn
>  denote (Tf (Id _ _ _) _) = Rn
>  denote (Tc _ _)                   = Wrap
>  denotes = denote . head

Property: each element of (split ts) is a nonempty list. (Proof by induction over the recursion of split)

>  split []  = []
>  split [t] = [[t]]
>  split (t:t':ts)
>   = if denote t `eq` Wrap      then (t:spl):spls else
>     if denote t `eq` denote t' then (t:spl):spls else
>                                         [t]:spl:spls
>     where 
>       spl:spls = split (t':ts)
>       Flr `eq` Flr = True
>       Frl `eq` Frl = True
>       x `eq` y     = False
