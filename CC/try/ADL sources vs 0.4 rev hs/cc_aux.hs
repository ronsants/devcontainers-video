 module CC_aux where
  import Char
  import UU_BinaryTrees
  import UU_Scanner
  import UU_Parsing
  import Auxiliaries
  import Classification
  import Typology

  data Architecture = Arch Contexts
  data Concept      = C String GenR [String] | Anything GenR
  type Concepts  = [Concept]
  instance Eq Concept where
   C a _ _ == C b _ _ = a==b
   _ == _ = True

  class ABoolAlg a where
   glb,lub :: a -> a -> a
   gEq, order :: a -> a -> Bool
   glb a b | b `gEq` a = b
           | a `gEq` b = a
           | otherwise  = error "glb undefined"
   lub a b | a `gEq` b = b
           | b `gEq` a = a
           | otherwise = error "lub undefined"
   order a b | a `gEq` b = True
             | b `gEq` a = True
             | otherwise = False

  gEtable cs
   = chain "\n" ([f l " "     ++"  "++chain " " (map (f 6.name) cs)]++
                 [f l (name j)++" |"++chain "|"[f 6 (show (i `gEq` j))|i<-cs]| j<-cs])
     where l = maximum (map (length.name) cs); f n str = take n (str++forever ' ')
           forever c = c:forever c

  gEtabG gEq cs
   = chain "\n" ([f l " "     ++"  "++chain " " (map (f 6.name) cs)]++
                 [f l (name j)++" |"++chain "|"[f 6 (show (i `gEq` j))|i<-cs]| j<-cs])
     where l = maximum (map (length.name) cs); f n str = take n (str++forever ' ')
           forever c = c:forever c

  type GenR = Concept->Concept->Bool

  instance ABoolAlg Concept where
   gEq (Anything _) b = True
   gEq a (Anything _) = False
   gEq a b = genE a a b
   glb a b | b `gEq` a = b
           | a `gEq` b = a
           | otherwise = error ("Fatal: (C) glb undefined: a="++show a++", b="++show b)
   lub a b | a `gEq` b = b
           | b `gEq` a = a
           | otherwise = error ("Fatal: (C) lub undefined: a="++show a++", b="++show b)

  instance (Show a,Show b,ABoolAlg a,ABoolAlg b) => ABoolAlg (a,b) where
   (a,a') `gEq` (b,b') = a `gEq` b && a' `gEq` b'
   glb a b | b `gEq` a = b
           | a `gEq` b = a
           | otherwise = error ("Fatal: glb undefined: a="++show a++", b="++show b)
   lub a b | a `gEq` b = b
           | b `gEq` a = a
           | otherwise = error ("Fatal: lub undefined: a="++show a++", b="++show b)

  instance ABoolAlg Signature where
   a `gEq` b = source a `gEq` source b && target a `gEq` target b

  instance ABoolAlg Morphism where
   a `gEq` b = source a `gEq` source b && target a `gEq` target b

  instance ABoolAlg Expression where
   a `gEq` b = source a `gEq` source b && target a `gEq` target b

  data ConceptDef= Cd FilePos String String String
  instance Eq ConceptDef where
   Cd _ n _ _ == Cd _ m _ _ = n==m
  instance Identified ConceptDef where
   name (Cd _ n _ _) = n
  type ConceptDefs = [ConceptDef]
  data Context   = Ctx String [String] (Inheritance Concept) [Classification Context] Patterns Signatures [ConceptDef]
--  instance Eq Context where
--   Ctx nm _ _ _ _ _ _ == Ctx nm' _ _ _ _ _ _ = nm == nm'
  type Contexts  = [Context]
  type Link      = [String]
  src, trg      :: Link -> String
  src            = head
  trg            = last
  type Links     = [Link]
  data Gen       = G Concept Concept                                     deriving Eq
  type Gens      = [Gen]
  data Morphism  = Mph String FilePos [Concept] (Concept,Concept) Signature
                 | Id FilePos [Concept] Concept -- this alternative is used only in restrictions.
  data Signature = Sgn String Concept Concept [Prop] String String String [Link] FilePos
                 | Idn Concept FilePos
  dom, cod :: Signature -> [String]
  dom s = rd [head l| l<-contents s]
  cod s = rd [last l| l<-contents s]
  type Signatures = [Signature]
  type Patterns  = [Pattern]
  data Prop      = Uni | Inj | Sur | Tot | Sym | Asy | Trn | Rfx         deriving Eq
  pMeaning Uni   = "univalent"
  pMeaning Inj   = "injective"
  pMeaning Sur   = "surjective"
  pMeaning Tot   = "total"
  pMeaning Sym   = "symmetric"
  pMeaning Asy   = "antisymmetric"
  pMeaning Trn   = "transitive"
  pMeaning Rfx   = "reflexive"
  isFunction m   = null ([Uni,Tot]>-multiplicities m)

  data Rule      = Ru Char Expression FilePos Expression String (Concept,Concept) Int
                 | Gc FilePos Morphism Expression (Concept,Concept) Int   deriving Show
  instance Eq Rule where
   Ru c antc _ cons _ sgn _ == Ru c' antc' _ cons' _ sgn' _
    = c==c' && sgn==sgn' && antc==antc' && cons==cons'
   Gc _ m expr sgn _ == Gc _ m' expr' sgn' _
    = sgn==sgn' && m==m' && expr==expr'
   _ == _ = False
  type Rules     = [Rule]
  data Pattern   = Pat String Rules Gens Signatures ConceptDefs    -- deriving Show
  data Expression  = Tf Morphism Bool
                   | Tm Morphism Bool
                   | Tc Expression Bool
                   | F Expressions
                   | Fi Expressions
                   | Fu Expressions deriving Eq

  instance Eq Expression where
   Tm m c == Tm m' c' = m==m' && c==c'
   Tf m c == Tf m' c' = m==m' && c==c'
   Tc e c == Tc e' c' = e==e' && c==c'

  type Expressions = [Expression]

  class Numbered a where
   nr :: a->Int
   pos :: a->FilePos
   nr x = nr (CC_aux.pos x)

  instance Numbered FilePos where
   nr (FilePos (fn,Pos l c,sym)) = l
   pos p = p

  instance Numbered Rule where
   pos (Ru c antc p cons expla sgn nr) = p
   pos (Gc p m expr sgn n)             = p
   nr (Ru c antc p cons expla sgn n)   = n
   nr (Gc p m expr sgn n)              = n

  instance Numbered Morphism where
   pos (Mph nm p atts sgn s) = p
   pos (Id p atts c)         = p

  instance Numbered Expression where
   pos (Tm m _) = CC_aux.pos m
   pos (Tf m _) = CC_aux.pos m
   pos (Tc f _) = CC_aux.pos f
   pos (F ts)   = if not (null ts) then CC_aux.pos (head ts) else error "!!Software error 813. Please submit a complete bug report to your dealer"
   pos (Fu fs)  = if not (null fs) then CC_aux.pos (head fs) else error "!!Software error 814. Please submit a complete bug report to your dealer"
   pos (Fi fs)  = if not (null fs) then CC_aux.pos (head fs) else error "!!Software error 815. Please submit a complete bug report to your dealer"

  class Explained a where
   explain :: a -> String

  instance Explained Rule where
   explain (Ru c antc pos cons expla sgn nr) = expla
   explain (Gc p m expr sgn n)             = ""

  class Conceptual a where
   conts      :: a -> [String]                   -- the set of all objects in a concept

  instance Conceptual a => Conceptual [a] where
   conts                                         = rd . concat . map conts

  instance Conceptual a => Conceptual (Classification a) where
   conts                                         = rd . concat . map conts . preCl

  instance Conceptual Concept where
   conts (C _ _ os)   = os
   conts (Anything _) = error("Fatal: Anything is Everything...")

  class Morphical a where
   concs        :: a -> [Concept]                  -- the set of all concepts used in data structure a
   conceptdefs  :: a -> [ConceptDef]               -- the set of all concept definitions in the data structure
   conceptdefs x = []
   mors         :: a -> [Morphism]                 -- the set of all morphisms used within data structure a
   signatures   :: a -> [Signature]
   signatures x  = rd [m|Mph _ _ _ _ m<-mors x]
   genE         :: a -> GenR
   genE x        = if null cx then (==) else head cx where cx = [gE|C c gE cs<-concs x]

  instance Morphical a => Morphical [a] where
   concs                                         = rd . concat . map concs
   conceptdefs                                   = rd . concat . map conceptdefs
   mors                                          = rd . concat . map mors
   signatures                                    = rd . concat . map signatures

  instance Morphical Concept where
   concs       c                                 = [c]
   conceptdefs c                                 = []
   mors        c                                 = [Id posNone [] c]
   signatures  c                                 = [Idn c posNone]
   genE        (C c gE cs)                       = gE
   genE        (Anything gE)                     = gE

  instance Morphical a => Morphical (Classification a) where
   concs                                         = rd . concat . map concs . preCl
   conceptdefs                                   = rd . concat . map conceptdefs . preCl
   mors                                          = rd . concat . map mors . preCl
   signatures                                    = rd . concat . map signatures . preCl

  instance Morphical Signature where
   concs (Sgn _ a b _ _ _ _ _ _)                 = rd [a,b]
   concs (Idn a _)                               = [a]
   mors m                                        = []
   genE (Sgn nm a b props prL prM prR cs pos)    = genE a
   genE (Idn a _)                                = genE a

  instance Morphical Morphism where
   concs (Mph nm pos atts (a,b) m)               = rd [a,b]
   concs (Id pos atts c)                         = [c]
   mors m@(Mph _ _ _ _ _)                        = [m]
   mors m@(Id _ _ _)                             = []
   genE (Mph nm pos atts (a,b) m)                = genE m
   genE (Id pos atts c)                          = genE c

  instance Morphical Gen where
   concs (G g s)                                 = rd [g,s]
   mors m                                        = []
   genE (G g s)                                  = genE s

  instance Morphical Expression where
   concs (Tm m _)                                = rd (concs m)
   concs (Tf m _)                                = rd (concs m)
   concs (Tc f _)                                = rd (concs f)
   concs (F ts)                                  = rd (concs ts)
   concs (Fu fs)                                 = rd (concs fs)
   concs (Fi fs)                                 = rd (concs fs)

   mors (Tm m _)                                 = mors m
   mors (Tf m _)                                 = mors m
   mors (Tc f _)                                 = mors f
   mors (F ts)                                   = mors ts
   mors (Fu fs)                                  = mors fs
   mors (Fi fs)                                  = mors fs

   genE (F ts)                                   = genE ts
   genE (Fu fs)                                  = genE fs
   genE (Fi fs)                                  = genE fs
   genE (Tm m _)                                 = genE m
   genE (Tf m _)                                 = genE m
   genE (Tc f _)                                 = genE f

  instance Morphical Rule where
   concs (Ru c antc pos cons expla (a,b) nr)     = rd (concs antc++concs cons)
   concs (Gc pos m expr (a,b) nr)                = rd (concs m++concs expr)
   mors (Ru c antc pos cons expla sgn nr)        = rd (mors antc++mors cons)
   mors (Gc pos m expr sgn nr)                   = rd (mors m++mors expr)
   genE (Ru c antc _ cons _ _ _)                 = genE [antc,cons]
   genE (Gc pos m expr sgn n)                    = genE m

  instance Morphical Pattern where
   concs (Pat nm rs gen pms cs)                  = rd (concs rs++concs gen++concs pms)
   conceptdefs (Pat nm rs gen pms cs)            = cs
   mors (Pat nm rs gen pms cs)                   = mors rs
   signatures (Pat nm rs parChds pms cs)         = pms
   genE  (Pat nm rs parChds pms cs)              = genE (signatures rs++pms)

  instance Morphical Context where
   concs       (Ctx nm on isa world dc ms cs) = rd (concs ms++concs dc)
   conceptdefs (Ctx nm on isa world dc ms cs) = cs
   mors        (Ctx nm on isa world dc ms cs) = mors dc
   signatures  (Ctx nm on isa world dc ms cs) = rd (signatures dc++ms)

  instance Show Prop where
   showsPrec p Uni = showString "UNI"
   showsPrec p Inj = showString "INJ"
   showsPrec p Sur = showString "SUR"
   showsPrec p Tot = showString "TOT"
   showsPrec p Sym = showString "SYM"
   showsPrec p Asy = showString "ASY"
   showsPrec p Trn = showString "TRN"
   showsPrec p Rfx = showString "RFX"

{-  instance Show Context where
   showsPrec p (Ctx nm on isa world dc ms cs)
    = showString ("CONTEXT "++nm++
                  (if on==[] then "" else " EXTENDS "++chain ", " on)++"\n"++
                  chain "\n\n" (map show dc)++"\n"++
                  chain "\n" (map show ms)++"\nENDCONTEXT" ) -}

The function showHS prints structures as haskell source, which is intended for testing.

  class ShowHS a where
   showHS, showADL :: a -> String

  instance ShowHS Prop where
   showHS Uni  = "Uni"
   showHS Inj  = "Inj"
   showHS Sur  = "Sur"
   showHS Tot  = "Tot"
   showHS Sym  = "Sym"
   showHS Asy  = "Asy"
   showHS Trn  = "Trn"
   showHS Rfx  = "Rfx"
   showADL Uni = "UNI"
   showADL Inj = "INJ"
   showADL Sur = "SUR"
   showADL Tot = "TOT"
   showADL Sym = "SYM"
   showADL Asy = "ASY"
   showADL Trn = "TRN"
   showADL Rfx = "RFX"

  instance ShowHS a => ShowHS [a] where
   showHS  = chain "\n".map showHS
   showADL = chain "\n".map showADL

  instance ShowHS Context where
   showHS (Ctx nm on isa world dc ms cs)
    = nlHs++"ctx_"++nm++"\n>   = Ctx "++show nm++" "++show on++" isa (genEq (typology isa)) []"++
      ind++showL ["pat_"++name p|p<-dc]++
      ind++showL ["mor_"++name m++name(source m)++name(target m)|m<-ms]++
      init nlHs'++"where"++nlHs'++
      "isa = "++showHS isa++
      concat [nlHs'++showHS m|m<-ms]++"\n"++
      showHS dc++
      concat ["\n\nSignatures from "++name pat++"\n"++concat[nlHs'++showHS m|m<-signatures pat]|pat<-dc]
      where nlHs = "\n>  "; ind = nlHs++"       "; nlHs' = nlHs++"    "
   showADL (Ctx nm on isa world dc ms cs)
    = "CONTEXT\n" ++
      chain "\n\n" (map showADL dc) ++
      "\nENDCONTEXT"

  instance ShowHS Pattern where
   showHS (Pat nm rs gen pms cs)
    = nlHs++"pat_"++nm++nlHs'++"= Pat "++show nm++
      (if null rs then " []" else ind++"[ "++chain (ind++", ") [showHS r| r<-rs]++ind++"]")++
      (if null gen   then " []" else ind++"[ "++chain (ind++", ") [showHS g| g<-gen] ++ind++"]")++
      (if null gen   then " []" else ind++"[ "++chain (ind++", ") ["mor_"++name m++name(source m)++name(target m)| m<-pms] ++ind++"]")++
      init nlHs'
      where nlHs = "\n>      "; ind = nlHs++"       "; nlHs' = nlHs++"    "
   showADL (Pat nm rs gen pms cs)
    = "PATTERN\n" ++
      chain "\n" (map showADL (rd(signatures rs++pms))) ++
      chain "\n\n" (map showADL rs) ++
      "\nENDPATTERN"

  instance ShowHS Rule where
   showHS r@(Ru c antc p cons expla sgn nr)
    = chain " " ["Ru",[c],"("++showHS antc++")","posNone","("++showHS cons++")",show(explain r),showSgn sgn,show nr]
   showHS (Gc p m expr sgn nr)
    = chain " " ["Gc","posNone","("++showHS m++")","("++showHS expr++")",showSgn sgn,show nr]
   showADL r@(Ru 'I' antc p cons expla sgn nr)
    = showADL antc++" -: "++showADL cons
   showADL r@(Ru 'E' defd p expr expla sgn nr)
    = showADL defd++" = "++showADL expr
   showADL (Gc p m expr sgn nr)
    = "GLUE "++showADL m++" = "++showADL expr

  instance ShowHS Expression where
   showHS  (Tm m c) = "Tm ("++showHS m++") "++show c
   showHS  (Tf f c) = "Tf ("++showHS f++") "++show c
   showHS  (Tc f c) = "Tc ("++showHS f++") "++show c
   showHS (F ts)    = chain " " ["F",showL (map showHS ts)]
   showHS (Fu fs)   = chain " " ["Fu",showL (map showHS fs)]
   showHS (Fi fs)   = chain " " ["Fi",showL (map showHS fs)]

   showADL (Tm m c) = showADL m++['-'|not c]
   showADL (Tf f c) = showADL f++"~"++['-'|not c]
   showADL (Tc f c) = "("++showADL f++")"++['-'|not c]
   showADL (F ts)   = chain ";" (map showADL ts)
   showADL (Fu fs)  = chain "\\/" (map showADL fs)
   showADL (Fi fs)  = chain "/\\" (map showADL fs)

  showS m = name m++"["++show (source m)++","++show (target m)++"]"
  showSgn (a,b) = "("++showHS a++","++showHS b++")"
  showSign cs = "["++chain "*" (map name cs)++"]"
  showL xs = "["++chain "," xs++"]"

  instance ShowHS a => ShowHS (Inheritance a) where
   showHS (Isa ts cs) = "Isa "++showL ["("++showHS g++","++showHS s++")"|(g,s)<-ts] ++" "++ showL (map showHS cs)
   showADL (Isa ts cs) = ""

  instance ShowHS Concept where
   showHS (Anything _) = "Anything"
   showHS  c = "C "++show (name c)++" "++show (conts c)
   showADL c = show (name c)

  instance ShowHS Signature where
   showHS (Sgn nm a b props prL prM prR cs pos)
    = chain " " ["mor_"++nm++name a++name b,"= Sgn",show nm,"("++showHS a++")","("++showHS b++")",showL(map showHS props),show prL,show prM,show prR,"[]",show pos]
   showHS (Idn a pos)
    = ""
   showADL (Sgn nm a b props prL prM prR cs _)
    = nm++" :: "++name a++" * "++name b++(if null props then "" else showL(map showADL props))++
      (if null(prL++prM++prR) then "" else " PRAGMA "++chain " " (map show [prL,prM,prR]))
      ++"."
   showADL _
    = ""

  instance ShowHS Morphism where
   showHS (Mph nm pos atts sgn@(a,b) m)
    = chain " " ["Mph",show nm,(\(FilePos (_,Pos l c,_))->show (l,c)) pos,showL(map showHS atts),showSgn sgn,"mor_"++nm++name a++name b]
   showHS (Id pos atts c)
    = chain " " ["Id",(\(FilePos (_,Pos l c,_))->show (l,c)) pos,showL(map showHS atts),name c]
   showADL (Mph nm pos atts sgn@(a,b) m)
    = nm++if null atts then "" else showSign atts
   showADL (Id pos atts c)
    = "I"++if null atts then "" else showSign atts

  instance ShowHS Gen where
   showHS (G g s)  = "G ("++show s++") ("++show g++")"
   showADL (G g s) = "GEN "++showADL s++" ISA "++show g

  instance Show Pattern where
   showsPrec p (Pat nm rs gen pms cs)
    = showString ("PATTERN "++nm++"\n"++
                  pr gen++['\n'| not (null gen)]++
                  pr pms++['\n'| not (null pms)]++
                  pr rs++"\nENDPATTERN" )
      where pr xs = chain "\n" [" "++show x| x<-xs]

  instance Show Rule where
   showsPrec p r@(Ru c antc _ cons _ _ _)
    = showString (show antc++(if c=='I' then " -: " else " = ")++show cons++"\n  EXPLANATION "++explain r++"\n("++show (pos r)++")")
   showsPrec p r@(Gc _ m expr _ _)
    = showString ("GLUE "++show m++" = "++show expr++"\n("++show (pos r)++")")

This show is used in error messages. It should therefore not display the factor's type

  instance Show Expression where
   showsPrec p (Tm m c) = showString (show m++['-'|not c])
   showsPrec p (Tf f c) = showString (show f++"~"++['-'|not c])
   showsPrec p (Tc f c) = showString ("("++show f++")"++['-'|not c])
   showsPrec p (F ts)   = showString (chain ";" (map show ts))
   showsPrec p (Fu fs)  = showString ("("++chain "\\/" (map show fs)++")")
   showsPrec p (Fi fs)  = showString ("("++chain "/\\" (map show fs)++")")

The following shSigns is intendes for error messages

  shSigns [(a,b)] = "["++show a++"*"++show b++"]"
  shSigns ss = commaEng "or" ["["++show a++"*"++show b++"]"|(a,b)<-ss]
   
  instance Show Concept where
   showsPrec p c = showString (name c)

This show is used in error messages. It should therefore not display the morphism's type

  instance Show Morphism where
   showsPrec p (Mph nm pos  []  sgn m) = showString (nm  {- ++"("++show a++"*"++show b++")" where (a,b)=sgn -} )
   showsPrec p (Mph nm pos atts sgn m) = showString (nm  {- ++"["++chain "*" (map name (rd atts))++"]" -} )
   showsPrec p (Id pos atts c)         = showString ("I" ++ if null atts then {- ++"["++name c++"]" -} "" else show atts)

  instance Show Signature where
   showsPrec p (Sgn nm a b props prL prM prR cs _)
    = showString (chain " " [nm,"::",name a,"*",name b,show props,"PRAGMA",show prL,show prM,show prR])
   showsPrec p (Idn a pos)
    = showString ""

This show is used in error messages. It should therefore not display the term's type

  instance Show Gen where
   showsPrec p (G g s) = showString ("GEN "++show s++" ISA "++show g)

  fEmpty (F [])  = True
  fEmpty (Fu []) = True
  fEmpty (Fi []) = True
  fEmpty     _   = False

  instance Eq Signature where
--   I nm == I nm' = True
   m == m' = mor2name m==mor2name m' && source m==source m' && target m==target m'
  instance Eq Morphism where
   m == m' = mor2name m==mor2name m' && source m==source m' && target m==target m'

A signature stands for a relation between two concepts. Mathematically, we
interpret a signature as a relation, i.e. a subset of the cartesian product of two sets.
m::signature  means interpret m `subsetEq` interpret (source m) x interpret (target m).
Every signature m has cardinalities, in which
  Uni:    forall x,y,y': x m y & x m y' = y==y'           (~m;m `subsetEq` I (target m))
  Tot:    forall x: exists y: x m y                        (I (source m) `subsetEq` m;~m)
  Inj:    forall x,x',y: x m y & x' m y = x==x'           (m;~m `subsetEq` I (source m))
  Sur:    forall y: exists x: x m y                        (I (target m) `subsetEq` ~m;m)
  Trn:    forall x,y,z: x m y & y m z = x m z             (m;m `subsetEq` m)
  Asy:    forall x,y: x m y & y m x = x==y                (m & ~m `subsetEq` I)
  Sym:    forall x,y: x m y <= y m x                      (m = ~m)
  Rfx:    forall x: x m x                                  (I `subsetEq` m)

  class Populated a where
   contents  :: a -> [Link]

  instance Populated Morphism where
   contents (Mph _ _ _ _ m)         = contents m
   contents (Id _ _ c)              = [[o,o] | o<-conts c]

  instance Populated Signature where
   contents (Sgn _ _ _ _ _ _ _ cs _) = cs
   contents (Idn _ _)                = []

  instance Populated Expression where
   contents (Tm m c)      = contents m
   contents (Tf m c)      = map reverse (contents m)
   contents (Tc f c)      = contents f
   contents f@(F ts)
    | and(map isIdent ts) = [[e,e]|e<-cs]
    | otherwise           = if null css then error ("Fatal: no terms in F "++showHS ts) else
                            foldr1 join css
                            where C _ _ cs = source f `lub` target f
                                  css = [contents t|t<-ts, not (isIdent t)]
   contents (Fu fs)       = if null fs then [] else (foldr1 uni .map contents) fs
   contents (Fi fs)       = if null fs then [] else (foldr1 isc .map contents) fs

  join::[Link]->[Link]->[Link]
  join a b = rd (merge ((sort' (last.head).eqCl last) a)
                       ((sort' (head.head).eqCl head) b))
             where merge (xs:xss) (ys:yss)
                    | last (head xs)<head (head ys) = merge xss (ys:yss)
                    | last (head xs)>head (head ys) = merge (xs:xss) yss
                    | otherwise = [[x,y]|[x,i]<-xs,[j,y]<-ys]++ merge xss yss
                   merge _ _ = []
  makeConceptSpace :: [Morphism] -> Concepts
  makeConceptSpace morphisms
   = [ C c gE (sord (concat (map snd raw)))
     | raw <- eqCl fst [(c,os)| m@(Mph nm pos atts (s,t) sgn@(Sgn _ s' t' _ _ _ _ ds _)) <- morphisms
                              , (c,os) <- [(s',rd (map src ds)),(t',rd (map trg ds))]
                              , if s' `gEq` s && t' `gEq` t then True else error("Fatal: err in makeConceptSpace with "++showHS m++"\nand sgn="++showHS sgn++"\n"++gEtable [s,s',t,t']) ]
     , C c gE os <- [fst (head raw)]
     ]

TODO: transform makeConceptSpace to makeConceptSpace :: [Signature] - Concepts

  
  class Pop a where
   populate   :: Concepts -> a -> a
   update     :: [Signature] -> a -> a
   specialize :: (Concept,Concept) -> a -> a
   update ss c = c

  instance Pop Concept where
   populate cs c = head ([c'|c'<-cs, c==c']++[c])
   specialize (a,b) c = if length (eqClass order [a,b,c])>1 then error ("Fatal: specialize 1 ("++show a++","++show b++") "++showHS c) else
                        a `lub` b `lub` c

  instance (Show a,Pop a) => Pop (a,a) where
   populate cs (s,t) = (populate cs s,populate cs t)

    specialize (a,b) (s,t) = if not (a `order` s && b `order` t) then error ("Fatal: specialize 2 ("++show a++","++show b++") ("++show s++","++show t++")") else
                             (a `lub` s, b `lub` t)

  instance Pop Gen where
   populate cs (G g s)  = G (populate cs g)  (populate cs s)
   update ss (G g s)    = G (update ss g)    (update ss s)
   specialize t (G g s) = G (specialize t g) (specialize t s)

  instance Pop Context where
   populate cs (Ctx nm on isa world dc ms cs')  = Ctx nm on isa world (map (populate cs) dc) (map (populate cs) ms) cs'
   update ss (Ctx nm on isa world dc ms cs')    = Ctx nm on isa world (map (update ss) dc) (map (update ss) ms) cs'
   specialize t (Ctx nm on isa world dc ms cs') = Ctx nm on isa world (map (specialize t) dc) (map (specialize t) ms) cs'

  instance Pop Pattern where
   populate cs (Pat nm rs gen pms cs')  = Pat nm (map (populate cs) rs) (map (populate cs) gen) (map (populate cs) pms) cs'
   update ss (Pat nm rs gen pms cs')    = Pat nm (map (update ss) rs) (map (update ss) gen) (map (update ss) pms) cs'
   specialize t (Pat nm rs gen pms cs') = Pat nm (map (specialize t) rs) (map (specialize t) gen) (map (specialize t) pms) cs'

  instance Pop Rule where
   populate cs (Ru c antc pos cons expla sgn nr)  = Ru c (populate cs antc) pos (populate cs cons) expla (populate cs sgn) nr
   populate cs (Gc pos m expr sgn nr)             = Gc pos (populate cs m) (populate cs expr) (populate cs sgn) nr
   update ss (Ru c antc pos cons expla sgn nr)    = Ru c (update ss antc) pos (update ss cons) expla (update ss sgn) nr
   update ss (Gc pos m expr sgn nr)               = Gc pos (update ss m) (update ss expr) (update ss sgn) nr
   specialize t (Ru c antc pos cons expla sgn nr) = Ru c (specialize t antc) pos (specialize t cons) expla (specialize t sgn) nr
   specialize t (Gc pos m expr sgn nr)            = Gc pos (specialize t m) (specialize t expr) (specialize t sgn) nr

  instance Pop Expression where
   populate cs (Tm m c)        = Tm (populate cs m) c
   populate cs (Tf m c)        = Tf (populate cs m) c
   populate cs (Tc f c)        = Tc (populate cs f) c
   populate cs (F ts)          = F  (map (populate cs) ts)
   populate cs (Fu fs)         = Fu (map (populate cs) fs)
   populate cs (Fi fs)         = Fi (map (populate cs) fs)

   update ss (Tm m c)          = Tm (update ss m) c
   update ss (Tf m c)          = Tf (update ss m) c
   update ss (Tc f c)          = Tc (update ss f) c
   update ss (F ts)            = F  (map (update ss) ts)
   update ss (Fu fs)           = Fu (map (update ss) fs)
   update ss (Fi fs)           = Fi (map (update ss) fs)

   specialize t (Tm m c)       = Tm (specialize t m) c
   specialize t@(a,b) (Tf m c) = Tf (specialize (b,a) m) c
   specialize t (Tc f c)       = Tc (specialize t f) c
   specialize t@(a,b) (F [t']) = F [specialize t t']
   specialize t@(a,b) (F ts)   = F ([specialize (a,target h) h]++init (tail ts)++[specialize (source l,b) l])
                                 where h=head ts; l=last ts
   specialize t@(a,b) (Fu fs)  = Fu (map (specialize t) fs) 
   specialize t@(a,b) (Fi fs)  = Fi (map (specialize t) fs) 

  instance Pop a = Pop [a] where
   populate cs xs  = map (populate cs) xs
   update ss xs    = map (update ss) xs
   specialize t xs = map (specialize t) xs

  instance Pop Morphism where
   populate cs (Mph nm p atts sgn s)  = Mph nm p (map (populate cs) atts) (populate cs sgn) (populate cs s)
   populate cs (Id p atts c)          = Id p (map (populate cs) atts) (populate cs c)
   update ss (Mph nm p atts sgn s)    = Mph nm p atts (update ss sgn) (update ss s) 
   update ss (Id pos atts c)          = Id pos (map (update ss) atts) (update ss c)
   specialize t@(a,b) (Mph nm p atts sgn s) = Mph nm p (if null atts then [] else [a,b]) t (specialize t s) 
   specialize t (Id pos atts c)       = Id pos (if null atts then [] else [spec]) spec
                                        where spec= specialize t c

  instance Pop Signature where
   populate cs (Sgn nm a b props prL prM prR cs' pos) = Sgn nm (populate cs a) (populate cs b) props prL prM prR cs' pos
   populate cs (Idn a pos)                            = Idn (populate cs a) pos
   update ss s@(Sgn nm a b props prL prM prR ss' pos) = head ([c|c<-ss, s==c]++[s])
   update ss s@(Idn a pos)                            = s
   specialize (x,y) (Sgn nm a b props prL prM prR ls pos) = Sgn nm x y props prL prM prR [[d,e]|[d,e]<-ls,d `elem` conts a, e `elem` conts b] pos
   specialize (x,y) s@(Idn a pos)                         = if x `order` y then Idn (x `lub` y) pos else error("Fatal: specialize 7 "++show (x,y)++showHS s)
  
  class Morphic a where
   source, target :: a -> Concept
   sign           :: a -> (Concept,Concept)
   sign x = (source x,target x)
   multiplicities :: a -> [Prop]
   multiplicities m = []
   flp            :: a -> a
   isIdent        :: a -> Bool
   isIdent m  = False
   equiv          :: a -> a -> Bool
   equiv m m' = source m==source m'&&target m==target m' || source m==target m'&&target m==source m'

  class Morphics a where
   anything       :: a -> Bool

  instance Morphics a => Morphics [a] where
   anything xs = and (map anything xs)
  instance Morphics Concept where
   anything (Anything _) = True
   anything _ = False
  instance (Morphics a,Morphics b) => Morphics (a,b) where
   anything (x,y) = anything x && anything y

  instance Morphic Morphism where
   source (Mph nm pos atts (a,b) m) = a
   source (Id pos atts c)           = c
   target (Mph nm pos atts (a,b) m) = b
   target (Id pos atts c)           = c
   sign   (Mph nm pos atts (a,b) m) = (a,b)
   sign   (Id pos atts c)           = (c,c)
   multiplicities (Mph nm pos atts sgn m)    = multiplicities m
   multiplicities (Id pos atts c)            = [Uni,Tot,Sur,Inj,Sym,Trn,Rfx]
   flp (Mph nm pos atts (a,b) m)    = Mph nm pos (reverse atts) (b,a) (flp m)
   isIdent (Mph _ _ _ _ _ )         = False
   isIdent (Id _ _ _ )              = True

  instance Morphic Signature where
   source (Sgn _ a b _ _ _ _ _ _)             = a
   source (Idn c _)                           = c
   target (Sgn _ a b _ _ _ _ _ _)             = b
   target (Idn c _)                           = c
   sign   (Sgn _ a b _ _ _ _ _ _)             = (a,b)
   sign   (Idn c _)                           = (c,c)
   multiplicities (Sgn _ _ _ props _ _ _ _ _) = props
   multiplicities (Idn _ _)                   = [Uni,Tot,Sur,Inj,Rfx,Trn,Sym]
   flp(Sgn nm a b props prL prM prR cs pos)   = Sgn nm b a (flipProps props) "" "" "" (map reverse cs) pos
   flp    i                                   = i
   isIdent (Idn _ _)                          = True
   isIdent _                                  = False

  instance Morphic Expression where
   source (Tm m _)         = source m
   source (Tf m _)         = target m
   source (Tc f _)         = source f
   source (F  ts)          = source (head ts)
   source (Fu fs)          = source (head fs)
   source (Fi fs)          = source (head fs)

   target (Tm m _)         = target m
   target (Tf m _)         = source m
   target (Tc f _)         = target f
   target (F ts)           = target (last ts)
   target (Fu fs)          = target (last fs)
   target (Fi fs)          = target (last fs)

   sign (Tm m _)           = sign m
   sign (Tf m _)           = (b,a) where (a,b) = sign m
   sign (Tc f _)           = sign f
   sign (F ts)             = if null ts then error ("Fatal: no terms in sign (F "++showHS ts++")") else
                             foldr1 jnSign (map sign ts)
                             where (s,t) `jnSign` (s',t') = (s,t')
   sign (Fu fs)            = if length (eqClass order (map sign fs))>1 then error ("Fatal: sign (Fu fs) not defined\nwith map sign fs="++show (map sign fs)) else
                             foldr1 lub (map sign fs)
   sign (Fi fs)            = if length (eqClass order (map sign fs))>1 then error ("Fatal: sign (Fi fs) not defined\nwith map sign fs="++show (map sign fs)) else
                             foldr1 lub (map sign fs)

   multiplicities (Tm m c) = [p|p<-multiplicities m, c]
   multiplicities (Tf m c) = [p|p<-flipProps (multiplicities m),c]
   multiplicities (Tc f c) = [p|p<-multiplicities f,c]
   multiplicities (F ts)   = foldr isc [Uni,Tot,Sur,Inj] (map multiplicities ts)
   multiplicities (Fu fs)  = []
   multiplicities (Fi fs)  = []

   flp (Tm m c)            = Tf m c
   flp (Tf m c)            = Tm m c
   flp (Tc f c)            = Tc (flp f) c
   flp (F ts)              = F (map flp (reverse ts))
   flp (Fu fs)             = Fu (map flp fs)
   flp (Fi fs)             = Fi (map flp fs)

   isIdent (Tm m _)        = isIdent m
   isIdent (Tf m _)        = isIdent m
   isIdent (Tc f _)        = isIdent f
   isIdent (F ts)          = and [isIdent t| t<-ts]
   isIdent (Fu fs)         = and [isIdent f| f<-fs]
   isIdent (Fi fs)         = and [isIdent f| f<-fs]

  instance Morphic a = Morphic [a] where
   source []   = Anything (==)
   source as   = source (head as)
   target []   = Anything (==)
   target as   = target (last as)
   flp         = map flp.reverse
   isIdent as  = and [isIdent m| m<-as]

  class Substitutive a where
-- Precondition: functional s
-- betekenis: Als (g,c) `elem` s dan wordt elk voorkomen van g in x vervangen door c in subsC s x
   subsC     :: [(Concept,Concept)] -> a -> a
   subsR     :: Rule -> a -> a  -- Note: the rule must be a Gc
   subsR r x = x
-- precondition: sign f `order` sign m
   subst :: (Morphism,Expression) -> a -> a
   subst (m,f) x = error "Unable to substitute"

  instance (Morphic a,Substitutive a) => Substitutive [a] where
   subsC c  xs = map (subsC c) xs
   subsR r  xs = map (subsR r) xs
   subst (m,f) xs = map (subst (m,f)) xs

  instance Substitutive Morphism where
   subsC s (Mph nm pos atts (a,b) m)
    = Mph nm pos atts (head ([d| (g,d)<-s, g==a]++[a]),head ([c| (g,c)<-s, g==b]++[b])) m
   subsC s (Id pos atts c)
    = Id pos atts (head ([d| (g,d)<-s, g==c]++[c]))

  instance Substitutive Expression where
   subsC s (Tm m c) = Tm m' c where m' = subsC s m
   subsC s (Tf m c) = Tf m' c where m' = subsC s m  -- CORRECT???
   subsC s (Tc f c) = Tc f' c where f' = subsC s f
   subsC s (F ts)   = F  (map (subsC s) ts)
   subsC s (Fu fs)  = Fu (map (subsC s) fs)
   subsC s (Fi fs)  = Fi (map (subsC s) fs)
   subsR (Gc pos m (F ts) sgn' nr) f@(F ts'')
    = head ([ F ts'
            | (ts',n)<-[ subs (Tm m True) ts ts''
                       , subs (Tf m True) (reverse (map flp ts)) ts'']
            , n>0]++[f])
     where
--      subs :: Eq a => a->[a]->[a]->([a],Int)
      subs m part whole = splits m [] [] part whole 0
       where
        splits m out state (i:ins) (t:ts) n
         | name i==name t && i `gEq` t  = splits m out state' ins ts n    -- might be t `gEq` i instead...???
         | otherwise  = splits m (out++[head state']) [] part (tail (state'++ts)) n
         where state' = state++[t]
        splits m out state [] ts n
         = splits m (out++[m]) [] state ts (n+1)
        splits m out state ins [] n = (out++state, n)
   subsR s (Fu fs) = Fu (map (subsR s) fs)
   subsR s (Fi fs) = Fi (map (subsR s) fs)
   subst (m,f) t@(Tm m' c) = if     m==m' then Tc f  c else
                             if flp m==m' then Tc f' c else t
                             where f'=flp f
   subst (m,f) t@(Tf m' c) = if     m==m' then Tc f' c else
                             if flp m==m' then Tc f  c else t
                             where f'=flp f
   subst (m,f) (Tc f' c)   = Tc (subst (m,f) f') c
   subst (m,f) (F ts)      = F  (subst (m,f) ts)
   subst (m,f) (Fu fs)     = Fu (subst (m,f) fs)
   subst (m,f) (Fi fs)     = Fi (subst (m,f) fs)

-- Identified Expression is necessary for subs only.
  instance Identified Expression where
   name (Tm m c) = name m
   name (Tf m c) = name m++"~"
   name (Tc f c) = ""       -- this is wrong! Todo: find out about Tc

  instance Substitutive Rule where
   subsC s r@(Ru c antc pos cons expla sgn nr)
    = if sign antc' `order` sign cons'
      then Ru c antc' pos cons' expla (sign antc' `lub` sign cons') nr
      else error("Fatal: cannot execute:   subsC s r\nwith s="++show s++"\nand  r="++showADL r++"\n"++showHS r++"\nbecause "++show (sign antc')++" `order` "++show (sign cons')++" is False.")
      where antc' = subsC s antc
            cons' = subsC s cons
   subsC s g@(Gc pos m expr sgn nr)
    = if sign m' `order` sign expr'
      then Gc pos m' expr' (sign m' `lub` sign expr') nr
      else error("Fatal: cannot execute:   subsC s g\nwith s="++show s++"\nand  g="++showADL g++"\n"++showHS g++"\nbecause "++show (sign m')++" `order` "++show (sign expr')++" is False.")
      where m'    = subsC s m
            expr' = subsC s expr
   subsR s r@(Ru c antc pos cons expla sgn nr)
    = if sign antc' `order` sign cons'
      then Ru 'I' (limit sgn' antc') pos (limit sgn' cons') expla sgn' nr
      else error("Fatal: cannot execute:   subsR s r\nwith s="++show s++"\nand  r="++showADL r++"\n"++showHS r++"\nbecause "++show (sign antc')++" `order` "++show (sign cons')++" is False.")
      where antc' = subsR s antc
            cons' = subsR s cons
            sgn'  = sign antc' `lub` sign cons'
   subsR s g@(Gc pos m expr sgn nr)
    = if sign m' `order` sign expr'
      then Gc pos m' expr' (sign m' `lub` sign expr') nr
      else error("Fatal: cannot execute:   subsR s g\nwith s="++show s++"\nand  g="++showADL g++"\n"++showHS g++"\nbecause "++show (sign m')++" `order` "++show (sign expr')++" is False.")
      where m'    = subsR s m
            expr' = subsR s expr
   subst (m,f) r@(Ru c antc pos cons expla sgn nr)
    = if sign antc' `order` sign cons'
      then Ru c antc' pos cons' expla (sign antc' `lub` sign cons') nr
      else error("Fatal: cannot execute:   subst (m,f) r\nwith m="++show m++"\n     f="++show f++"\nand  r="++showADL r++"\n"++showHS r++"\nbecause "++show (sign antc')++" `order` "++show (sign cons')++" is False.")
      where antc' = subst (m,f) antc
            cons' = subst (m,f) cons
   subst (m,f) (Gc pos m' expr sgn nr)
    = Gc pos m' expr' (sign expr') nr
      where expr' = subst (m,f) expr

  mkVar ex cs = mknew ex [[(toLower.head.(++"x").name) c]|c<-cs]
   where
    mknew ex [] = []
    mknew ex (x:xs) | x `elem` ex = mknew ex ((x++"'"):xs)
                    | otherwise = x: mknew (ex++[x]) xs

  class Calc a where
   limit     :: (Concept,Concept) -> a -> a
   calc      :: a -> [Signature] -> [Link]
   strands   :: (Concept,Concept) -> a -> [[String]]
   nostrds   :: (Concept,Concept) -> a -> [[String]]

todo: transform the following into  instance Collection Concept where
OBSOLETE:  (iscCpt is lub and uniCpt is glb)
  s `iscCpt` Anything _ = s
  Anything _ `iscCpt` t = t
  s `iscCpt` t | s `gEq` t   = C a' gE' (cs `isc` cs') 
               | t `gEq` s   = C a  gE  (cs `isc` cs') 
               | otherwise   = error ("unable to `isc` nonequivalent concepts: "++a++" and "++a'++".")
               where
                C a  gE  cs  = s
                C a' gE' cs' = t
  s `uniCpt` Anything gE = Anything gE
  Anything gE `uniCpt` t = Anything gE
  s `uniCpt` t | s `gEq` t   = C a' gE' (cs `uni` cs') 
               | t `gEq` s   = C a  gE  (cs `uni` cs') 
               | otherwise   = error ("unable to `uni` nonequivalent concepts: "++a++" and "++a'++".")
               where
                C a  gE  cs  = s
                C a' gE' cs' = t

  e `elemCpt` s  = e `elem` conts s

  emptyCpt     = C "" (==) [] -- used only in the parser, when the gE relation is not yet defined.

todo: transform the following into  instance Collection Signature where

  e `elemSgn` s  = e `elem` contents s


  s `jnSgn` t   | isIdent s = Sgn nm' (a `lub` a') b' props' prL' prM' prR' cs' pos'
                | isIdent t = Sgn nm  a' (b `lub` b') props  prL  prM  prR  cs  pos
                | source t `order` target s = Sgn (nm++";"++nm') a b' (h (multiplicities s) `isc` h (multiplicities t)) prL prM prR (cs `join` cs') posNone
                | otherwise = error ("unable to `;` (compose) nonequivalent relations "++show s++" and "++show t++".")
                where
                 h ps = ps>-[Sym,Asy,Trn,Rfx]
                 Sgn nm  a  b  props  prL  prM  prR  cs  pos  = s
                 Sgn nm' a' b' props' prL' prM' prR' cs' pos' = t
  s@(Sgn nm a b props prL prM prR cs pos) `uniSgn` s'@(Sgn nm' a' b' props' prL' prM' prR' cs' pos')
    = Sgn nm (if a `order` a' then a `lub` a' else
              error("Fatal : "++show a++" and "++show a'++" are not ordered in\n"++showHS s++" `uniSgn` "++showHS s'))
             (if b `order` b' then b `lub` b' else
              error("Fatal : "++show b++" and "++show b'++" are not ordered in\n"++showHS s++" `uniSgn` "++showHS s'))
             [p|p<-[Tot,Sur],p `elem` props,p `elem` props'] prL prM prR (cs `uni` cs') pos
  i@(Idn c@(C _ _ atoms) pos') `uniSgn` s@(Sgn nm a b props prL prM prR cs pos)
    = Sgn nm (if a `order` c then a `lub` c else
              error("Fatal : "++show a++" and "++show c++" are not ordered in\n"++showHS i++" `uniSgn` "++showHS s))
             (if b `order` c then b `lub` c else
              error("Fatal : "++show b++" and "++show c++" are not ordered in\n"++showHS i++" `uniSgn` "++showHS s))
             (props `uni` [Rfx]) prL prM prR (cs `uni` [[e,e]|e<-atoms]) pos
  s@(Sgn nm a b props prL prM prR cs pos) `uniSgn` i@(Idn c@(C _ _ atoms) pos')
    = Sgn nm (if a `order` c then a `lub` c else
              error("Fatal : "++show a++" and "++show c++" are not ordered in\n"++showHS i++" `uniSgn` "++showHS s))
             (if b `order` c then b `lub` c else
              error("Fatal : "++show b++" and "++show c++" are not ordered in\n"++showHS i++" `uniSgn` "++showHS s))
             (props `uni` [Rfx]) prL prM prR (cs `uni` [[e,e]|e<-atoms]) pos
  s@(Sgn nm a b props prL prM prR cs pos) `iscSgn` s'@(Sgn nm' a' b' props' prL' prM' prR' cs' pos')
    = Sgn nm (if a `order` a' then a `lub` a' else
              error("Fatal : "++show a++" and "++show a'++" are not ordered in\n"++showHS s++" `iscSgn` "++showHS s'))
             (if b `order` b' then b `lub` b' else
              error("Fatal : "++show b++" and "++show b'++" are not ordered in\n"++showHS s++" `iscSgn` "++showHS s'))
             [p|p<-[Uni,Inj],p `elem` props,p `elem` props'] prL prM prR (cs `isc` cs') pos
  s@(Sgn nm a b props prL prM prR cs pos) `difSgn` s'@(Sgn nm' a' b' props' prL' prM' prR' cs' pos')
    = Sgn nm (if a `order` a' then a `lub` a' else
              error("Fatal : "++show a++" and "++show a'++" are not ordered in\n"++showHS s++" `difSgn` "++showHS s'))
             (if b `order` b' then b `lub` b' else
              error("Fatal : "++show b++" and "++show b'++" are not ordered in\n"++showHS s++" `difSgn` "++showHS s'))
             [p|p<-[Uni,Inj],p `elem` props] prL prM prR [x|x<-cs, not (x `elem` cs')] pos
  emptySgn = Sgn "empty" (Anything (==)) (Anything (==)) [] "" "" "" [] posNone

  instance Calc Signature where
   limit (a,b) s@(Sgn nm a' b' props prL prM prR cs pos)
    | a `order` a' && b `order` b' = Sgn nm (a `lub` a') ( b `lub` b') props prL prM prR (strands (a,b) s) pos -- 'strands' is hier OK omdat die slechts op Sgn wordt aangeroepen.
    | otherwise = error ("Cannot limit "++show (a,b)++" with "++show (a',b'))
   limit (a,b) (Idn a' pos)
    = Idn a pos
   calc s@(Sgn _ _ _ _ _ _ _ _ _) ss = contents (head([x|x<-ss, x `gEq` s]++ error("Scope error1 :"++name s)))
   calc s@(Idn a pos)             ss = [[c,c]| c<-conts a]
   strands (a,b) (Sgn nm a' b' props prL prM prR cs pos)
    = [[x,y]| [x,y]<-cs, x `elem` conts a, y `elem` conts b]
   strands (a,b) (Idn (C a' _ cs) pos)
    = [[e,e]|e<-cs,e `elem` conts a && e `elem` conts b]
   nostrds (a,b) s
    = [[x,y]|[x,y]<-contents s, x `elem` conts a] -- ++[[x,y]|[x,y]<-contents s, y `elem` conts b]

  instance Calc Morphism where
   limit sgn (Mph nm pos atts (a,b) m) = Mph nm pos atts sgn (limit sgn m)
   limit (a,b) (Id pos atts c)         = if a==b then Id pos atts a
                                         else error ("!Err: equal "++show a++" and "++show b++" expected.")
   calc (Mph nm pos atts sgn s) ss  = if null signs then error("Scope error :"++showS s++" "++show (map showS ss)) else
                                      contents (foldr1 glb signs)
                                      where signs = [x|x<-ss, x `gEq` s]
   calc (Id pos atts c)         ss  = [[a,a]| a<-conts c]
   strands sgn (Mph nm pos atts (a,b) m) = strands sgn m
   strands (a,b) (Id pos atts (C a' _ cs))
    = [[e,e]|e<-cs,e `elem` conts a && e `elem` conts b]
   nostrds sgn (Mph nm pos atts (a,b) m) = nostrds sgn m
   nostrds (a,b) (Id pos atts (C a' _ cs)) = [[x,x]|x<-cs, x `elem` conts a, x `elem` conts b]

  instance Calc Expression where
   limit sgn'  (Tm m c) = Tm (limit sgn' m) c
   limit (a,b) (Tf m c) = Tf (limit (b,a) m) c
   limit sgn'  (Tc f c) = Tc (limit sgn' f) c 
   limit sgn (F ts)     = F (lim sgn ts)
    where lim sgn  [x] = [limit sgn x]
          lim sgn   [] = []
          lim (a,b) (x:xs) = [limit (a,c) x]++lim (c,b) xs
                             where c = if null xs then target x else target x `lub` source (head xs)
   limit sgn (Fu fs)    = Fu (map (limit sgn) fs)
   limit sgn (Fi fs)    = Fi (map (limit sgn) fs)

   calc (Tm m _) ss     = calc m ss
   calc (Tf f _) ss     = map reverse (calc f ss)
   calc (Tc f _) ss     = calc f ss
   calc (F  ts) ss      = if null ts then error ("Fatal: no terms in calc (F "++showHS ts++")") else
                          foldr1 join [calc t ss| t<-ts ]
   calc (Fu fs) ss      = foldr uni [] [calc f ss| f<-fs ]
   calc (Fi fs) ss      = if null fs then error ("Fatal: no factors in calc (Fi "++showHS fs++")") else
                          foldr1 isc  [calc f ss| f<-fs ]

   strands sgn (Tc f _) = strands sgn f
   strands sgn (F ts)   = str sgn ts
   strands sgn (Fu fs)  = concat (map (strands sgn) fs)
   strands sgn (Fi fs)  = [str| str<-concat (map (strands sgn) fs)
                              , head str `elem` conts (source (head fs))
                              , last str `elem` conts (target (last fs))]
   strands (a,b) t      = [[x,y]| [x,y]<-contents t, x `elem` conts a, y `elem` conts b]

   nostrds sgn (Tm m _) = nostrds sgn m
   nostrds sgn (Tf m _) = map reverse (nostrds sgn m)
   nostrds sgn (Tc f _) = nostrds sgn f
   nostrds sgn (F ts)   = [s++sp (length ts*2+1-length s)| s<-nostr  sgn ts] ++ 
                          [sp (length ts*2+1-length s)++s| s<-nostr' sgn ts]
                          where sp 0 = []; sp (n+1) = "": sp n
   nostrds sgn (Fi fs)  = concat (map (nostrds sgn) fs)
   nostrds sgn (Fu fs)  = concat (map (nostrds sgn) fs)

  str (a,b)   []    = [[show e]| e<-conts b]
  str (a,b) (Tc f _:ts)
   = [s++rest| s<-strands (a,c) f, head s `elem` conts a
             , rest<-str (c,b) ts, last s==head rest]
     where c = if null ts then target f else target f `lub` source (head ts)
  str (a,b) (t:ts)
   = [[show x,name t]++rest| [x,y]<-contents t, x `elem` conts a
                           , rest<-str (if null ts then target t else target t `lub` source (head ts),b) ts, show y==head rest]

  nostr (a,b) [t]
   = if null strt then [[show x]|x<-conts a] else
     [ [show x,name t,show y]
     | [x,y]<-strt
     ] where strt = [[x,y]| [x,y]<-contents t, x `elem` conts a]
  nostr (a,b) (t:ts)
   = if null strt then [[show x]|x<-conts a] else
     [ [show x,name t]++rest
     | [x,y]<-strt
     , rest<-nostr (tt,b) ts
     , show y==head rest
     ] where strt = [[x,y]| [x,y]<-contents t, x `elem` conts a]
             tt = C nm gE (rd [last l| l<-strt] `isc` os)
             C nm gE os = target t
  nostr (a,b) []  = [[show e]| e<-concs a]
  nostr' (a,b) [t]
   = if null strt then [[show x]|x<-conts b] else
     [ [show x,name t,show y]
     | [x,y]<-strt
     ] where strt = [[x,y]| [x,y]<-contents t, y `elem` conts b]
  nostr' (a,b) []  = [[show e]| e<-concs b]
  nostr' (a,b) tms
   = if null strt then [[show x]|x<-conts b] else
     [ rest++[name t,show y]
     | [x,y]<-strt
     , rest<-nostr' (a,tt) ts
     , show x==last rest
     ] where t=last tms; ts=init tms
             strt = [[x,y]| [x,y]<-contents t, y `elem` conts b]
             tt = C nm gE (rd [head l| l<-strt] `isc` os)
             C nm gE os = source t

  fun,tot,inj,sur :: [Prop]->Bool
  fun = elem Uni
  tot = elem Tot
  inj = elem Inj
  sur = elem Sur
  flipProps :: [Prop] -> [Prop]
  flipProps ps = [flipProp p| p<-ps]

  flipProp Uni = Inj
  flipProp Tot = Sur
  flipProp Sur = Tot
  flipProp Inj = Uni
  flipProp Trn = Trn
  flipProp Sym = Sym
  flipProp Asy = Asy
  flipProp Rfx = Rfx

  instance Typologic Concept
  instance Identified Concept where
   name (C nm _ _) = nm
   name (Anything _) = "Anything"
  instance Identified Morphism where
   name (Mph nm pos atts sgn m) = nm
   name (Id pos atts c) = "I"
  instance Identified Signature where
   name (Sgn nm _ _ _ _ _ _ _ _) = nm
   name (Idn _ _)                = "I"

  applyM (Sgn _ _ _ _ prL prM prR _ _) d c = prL++d++prM++c++prR++"."
  applyM (Idn _ _)                     d c = d++" = "++c

  normRule :: (Concept->Concept->Concept) -> Rule -> Rule
  normRule upb (Ru 'I' a@(F ants) pos c@(F cons) expla sgn nr)
   | and(map isIdent(mors ants)) = Ru 'I' (F [Tm (Id pos [] idA) True]) pos (F cons) expla (idC,idC) nr
   | otherwise                   = Ru 'I' (F as) pos (F cs) expla (sa,sc) nr
   where
    idC = source c `upb` target c `upb` idA
    idA = foldr upb (target (last ants)) (map source ants)
    (as,cs) = move ants cons
    (sa,sc) = (source (head as) `upb` source (head cs), target (last as) `upb` target (last cs))
    move [] cs = ([Tm (Id pos [] (source (head cs))) True],cs)
    move as cs
     | sur (multiplicities h) && inj (multiplicities h) = move (tail as) ([flp h]++cs)
     | fun (multiplicities l) && tot (multiplicities l) = move (init as) (cs++[flp l])
     | otherwise      = (as,cs)
     where h=head as; l=last as
  normRule upb r = r


irred removes redundancy from a list of signatures.
Intended for use in the type checker AGtry only.
Precondition: a `gEq` a' && b `gEq` b' || a' `gEq` a && b' `gEq` b

  irredT :: GenR -> [(Concept,Concept)] -> [(Concept,Concept)]
  irredT gE ms = map (foldr1 lub) (eqClass order ms)
                 where (a,a') `lub` (b,b')   = if a `gE` b && a' `gE` b' then (b,b') else (a,a')
                       (a,a') `order` (b,b') = (a `gE` b && a' `gE` b') || (b `gE` a && b' `gE` a')
  irredM :: GenR -> [Signature] -> [Signature]
  irredM gE ms = map (foldr1 lub) (eqClass order ms)
                 where m `lub` m'   = if a `gE` b && a' `gE` b' then m else m'
                                      where (a,a') = sign m; (b,b') = sign m'
                       m `order` m' = (a `gE` b && a' `gE` b') || (b `gE` a && b' `gE` a')
                                      where (a,a') = sign m; (b,b') = sign m'

  instance Identified Context where
   name (Ctx nm _ _ _ _ _ _) = nm

  instance Identified Pattern where
   name (Pat nm _ _ _ _) = nm

Interpretation of context as a language means to describe the classification tree,
the set of signatures and the rules that apply in that context. Inheritance of
properties is achieved as a result.


  union :: Pattern -> Pattern -> Pattern
  union (Pat nm rs parChds pms cs) (Pat nm' rs' parChds' pms' cs')
    = Pat nm' (rd(rs++rs')) (rd(parChds++parChds')) (rd(pms++pms')) (rd(cs++cs'))

  class Morphical a => Language a where
    rules     :: a -> [Rule]
    srules    :: a -> [Rule]
    srules    = rules.rd.signatures
    specs     :: a -> [Rule]
    grules    :: a -> [Rule]
    grules a  = rules a ++ [Ru 'E' (F [Tm m True]) pos expr "" sgn n | Gc pos m expr sgn n<-specs a]
    patterns  :: a -> [Pattern]
    isa       :: a -> Inheritance Concept

  instance Language a => Language [a] where
   rules xs = (concat. map rules) xs
   specs xs = (rd. concat. map specs) xs
   patterns = rd' name.concat.map patterns
   isa      = foldr uni empty.map isa

  instance Language a => Language (Classification a) where
   rules cl    = rules (preCl cl)
   specs cl    = specs (preCl cl)
   patterns cl = patterns (preCl cl)
   isa         = foldr uni empty.map isa.preCl

  instance Language Rule where
   rules   r@(Gc pos m expr sgn n) = [Ru 'E' (F [Tm m True]) pos expr (name m++" is implemented using "++enumerate (map name (mors expr))) sgn (nr r)]
   rules   r                       = [r]
   specs   r@(Gc _ _ _ _ _)        = [r]
   specs   r                       = []
   patterns r = [Pat "" [r] [] [] []]
   isa (Ru c antc _ cons _ _ _)
     = Isa tuples (rd(concs antc++concs cons)>-rd [e|(a,b)<-tuples,e<-[a,b]])
       where tuples = clear [(source antc,source cons),(target antc,target cons)]
   isa (Gc _ m expr _ _)
     = Isa tuples (concs expr>-rd [e|(a,b)<-tuples,e<-[a,b]])
       where tuples = clear [(source expr,source m),(target expr,target m)]

  clear abs = rd [(a,b)| (a,b)<-abs, a/=b]
  clearG abs = rd [G g s| G g s<-abs, g/=s]

  instance Language Pattern where
   rules (Pat nm rs parChds pms cs) = [r|r@(Ru c antc pos cons expla sgn nr)<-rs]
   specs (Pat nm rs parChds pms cs) = [r|r@(Gc pos m expr sgn nr)<-rs]
   patterns p                       = [p]
   isa   (Pat nm rs parChds pms cs) = Isa ts (singles>-rd [e| G g s<-parChds,e<-[g,s]])
                                      where Isa tuples singles = isa rs
                                            ts = clear (tuples++[(g,s)| G g s<-parChds])

  instance Language Context where
   rules    (Ctx nm on i world dc ms cs) = rules (foldr union (Pat "" [] [] [] []) dc)++rules world
   specs    (Ctx nm on i world dc ms cs) = specs (foldr union (Pat "" [] [] [] []) dc)++specs world
   patterns (Ctx nm on i world dc ms cs) = dc
   isa      (Ctx nm on i world dc ms cs) = i

  instance Language Signature where
   rules m@(Sgn nm a b props prL prM prR cs pos)
    = [h p| p<-props, p `elem` [Sym,Asy,Trn,Rfx]
          , if a==b then True else error ("!Err: Property "++show p++" requires equal source and target domains (you specified "++name a++" and "++name b++").") ]
     where h Sym = Ru 'E' (F [Tm r True]) pos (F [Tf r True]) (nm++"["++name a++"*"++name a++"] is symmetric.") sgn 0
           h Asy = Ru 'I' (Fi [F [Tm r True], F [Tf r True]]) pos id (nm++"["++name a++"*"++name a++"] is antisymmetric.") sgn 0
           h Trn = Ru 'I' (F [Tm r True, Tm r True]) pos (F [Tm r True]) (nm++"["++name a++"*"++name a++"] is transitive.") sgn 0
           h Rfx = Ru 'I' id pos (F [Tm r True]) (nm++"["++name a++"*"++name a++"] is reflexive.") sgn 0
           sgn   = (a,a)
           r     = Mph nm pos [a] sgn m
           id    = F [Tm (Id pos [] a) True]
   rules _    = []
   specs _    = []
   patterns _ = []
   isa _      = empty

  nogE a b = False

  wrld :: Context -> [Classification Context]
  wrld (Ctx nm on i world dc ms cs) = world

Graph layout (using GraphViz)

  class Graphic a where
   dotGraph :: String -> String -> [[String]] -> a -> String

  instance Graphic Pattern where
   dotGraph nm len rRelationList pat
    = "graph "++show [x|x<-nm,not(isSpace x)]++introG++chain " " [show (name c)|c<-concs pat]++"}\n   ; "++
                chain "\n   ; "
                 ([ "\""++mor2name m++name (source m)++name (target m)++
                    "\" [label=\""++mor2name m++['*'|mor2 m `elem` rRelationList]++"\"]"| m<-arcs]++
                  [ show (name (source m))++" -- \""++mor2name m++name (source m)++name (target m)++"\" -- "++show (name (target m))| m<-arcs]++
                  [ show (name c)++" -- "++show (name p)++" [color=red,style=dotted]"
                  | Isa pcs cs<-[isa pat], (p,c)<-pcs])++
                "\n   }"
      where
        arcs = rd (mors pat++mors(specs pat)++[Mph (name m) posNone [source m,target m] (source m,target m) m| m<-signatures pat])
        introG = "\n   { style=\"filled\"; color=\".60 .27 1.0\"\n   ; node [shape=plaintext,fontsize=12,style=bold]\n   ; edge [dir=none]\n   ; {node [shape=ellipse,style=filled,fontsize=9,font=helvetica]"

  instance Graphic Context where
   dotGraph nm len rRelationList (Ctx cnm on isa world dc ms cs)
    = dotGraph nm len rRelationList (foldr union (Pat "" [] [] [] []) dc)

  instance Graphic Morphism where
   dotGraph nm len rRelationList m@(Mph nm' _ atts sgn m')
    = dotGraph nm len rRelationList (Pat nm [Ru 'E' (F [Tm m True]) posNone (F []) "" sgn 0] [] [] [])
--   dotGraph nm len rRelationList m@(Id pos atts c)
--    = dotGraph nm len rRelationList (Pat nm [Ru 'E' (F [Tm m True]) posNone (F []) "" (c,c) 0] [] [] [])

  dotGraph' nm len ls
    = "graph "++show [x|x<-nm,not(isSpace x)]++introG++chain " " (rd [e| [r,d,c]<-sls, e<-[d,c]])++"}\n   ; "++
                chain "\n   ; "
                 ([ "\""++r++d++c++
                    "\" [label=\""++r++"\"]"| [r,d,c]<-sls]++
                  [ show d++" -- \""++r++d++c++"\" -- "++show c | [r,d,c]<-sls])++
                "\n   }"
      where
      -- verwijderd: <len="++show len++",> in de code <edge [len="++show len++",dir=none]>
        introG = "\n   { style=\"filled\"; color=\"black\"\n   ; node [shape=plaintext,fontsize=12,style=bold]\n   ; edge [dir=none]\n   ; {node [shape=ellipse,style=filled,fontsize=9,font=helvetica]"
        sls = map (map conceptForm) ls

Position calculation in the parser

  newtype FilePos = FilePos (String, Pos, String)                        deriving Eq
  posNone         = FilePos ("",noPos,"")

  instance Show Pos where
    show (Pos l c)
      = "line " ++ show l
        ++ ", column " ++ show c

  instance Show FilePos where
    show (FilePos (fn,Pos l c,sym))
      = "line " ++ show l
--        ++ ", column " ++ show c
        ++ ", file " ++ show fn

  get_tok_pos     (Tok _ _ s l f) = FilePos (f,l,s)
  get_tok_val_pos (Tok _ _ s l f) = (s,FilePos (f,l,s))

  gsym_pos :: IsParser p Token => TokenType -> String -> String -> p FilePos
  gsym_pos kind val val2 = get_tok_pos <$> pSym (Tok kind val val2 noPos "")

  gsym_val_pos :: IsParser p Token => TokenType -> String -> String -> p (String,FilePos)
  gsym_val_pos kind val val2 = get_tok_val_pos <$> pSym (Tok kind val val2 noPos "")

  pOperAny           ::  IsParser p Token => p String
  pOperAny           =   pOper    ""

  pOper_pos name     =   gsym_pos TkOp        name      name
  pKey_pos  keyword  =   gsym_pos TkKeyword   keyword   keyword
  pSpec_pos s        =   gsym_pos TkSymbol    [s]       [s]

  pOParen_pos, pString_pos, pChar_pos, pInteger8_pos, pInteger10_pos, pInteger16_pos,
     pVarid_pos, pConid_pos, pTextnm_pos, pTextln_pos, pInteger_pos
                     ::  IsParser p Token => p FilePos
  pOParen_pos        =   pSpec_pos '('

  pString_pos        =   gsym_pos TkString    ""        "?STR?"
  pChar_pos          =   gsym_pos TkChar      ""        "'chr'"
  pInteger8_pos      =   gsym_pos TkInteger8  ""        "1"
  pInteger10_pos     =   gsym_pos TkInteger10 ""        "1"
  pInteger16_pos     =   gsym_pos TkInteger16 ""        "1"
  pVarid_pos         =   gsym_pos TkVarid     ""        "?LC?"
  pConid_pos         =   gsym_pos TkConid     ""        "?UC?"
  pTextnm_pos        =   gsym_pos TkTextnm    ""        ""
  pTextln_pos        =   gsym_pos TkTextln    ""        ""
  pInteger_pos       =   pInteger10_pos

  pInteger10_val_pos, pString_val_pos, pChar_val_pos, pVarid_val_pos, pConid_val_pos,
     pInteger_val_pos
                     ::  IsParser p Token => p (String,FilePos)
  pInteger10_val_pos =   gsym_val_pos TkInteger10 ""        "1"
  pString_val_pos    =   gsym_val_pos TkString    ""        "?STR?"
  pChar_val_pos      =   gsym_val_pos TkChar      ""        "'chr'"
  pVarid_val_pos     =   gsym_val_pos TkVarid     ""        "?LC?"
  pConid_val_pos     =   gsym_val_pos TkConid     ""        "?UC?"
  pInteger_val_pos   =   pInteger10_val_pos

  pParens_pos        ::  IsParser p Token => p a -> p (FilePos,a)
  pParens_pos p      =   (,) <$> pOParen_pos <*> p <* pCParen
  
  mor2 :: Morphism -> [String]
  mor2 m = [mor2name m,name (source m),name (target m)]

  mor2filename m = "Atlas"++relName (mor2 m)++".html"
  relName [r,d,c] = strip(r++conceptForm d++conceptForm c)
