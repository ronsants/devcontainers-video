  import Char
  import UU_BinaryTrees
  import UU_Scanner
  import UU_Parsing
  import Auxiliaries
  import Classification
  import Typology

  data Architecture = Arch Contexts
  data Concept   = C String [String]
  type Concepts  = [Concept]
  instance Eq Concept where
   C a _ == C b _ = a==b

  instance ABoolAlg Concept
  data ConceptDef= Cd FilePos String String String
  instance Eq ConceptDef where
   Cd _ n _ _ == Cd _ m _ _ = n==m
  instance Identified ConceptDef where
   name (Cd _ n _ _) = n
  type ConceptDefs = [ConceptDef]
  data Context   = Ctx String [String] (Inheritance Concept) GenR [Classification Context] Patterns Morphisms [ConceptDef]
--  instance Eq Context where
--   Ctx nm _ _ _ _ _ _ _ == Ctx nm' _ _ _ _ _ _ _ = nm == nm'
  type Contexts  = [Context]
  type Drecord   = [String]
  src, trg      :: Drecord -> String
  src            = head
  trg            = last
  type Drecords  = [Drecord]
  data Factor    = F Terms (Concept,Concept)
                 | Fi Factors (Concept,Concept)
                 | Fu Factors (Concept,Concept)                          deriving Eq
  type Factors   = [Factor]
  data Gen       = G Concept Concept                                     deriving Eq
  type Gens      = [Gen]
  data Morph     = Mph String FilePos [Concept] (Concept,Concept) Morphism
                 | Id FilePos [Concept] Concept -- this alternative is used only in restrictions.
  data Morphism  = Mor String Concept Concept [Prop] String String String [Drecord] FilePos
                 | Idn Concept FilePos
  type Morphisms = [Morphism]
  type Patterns  = [Spec]
  data Prop      = Uni | Inj | Sur | Tot | Sym | Asy | Trn | Rfx         deriving Eq
  pMeaning Uni   = "univalent"
  pMeaning Inj   = "injective"
  pMeaning Sur   = "surjective"
  pMeaning Tot   = "total"
  pMeaning Sym   = "symmetric"
  pMeaning Asy   = "antisymmetric"
  pMeaning Trn   = "transitive"
  pMeaning Rfx   = "reflexive"

  data Rule      = Dc Morph FilePos Factor String (Concept,Concept) Int
                 | Hc Factor FilePos Factor String (Concept,Concept) Int
                 | Gc FilePos Morph Factor (Concept,Concept) Int          deriving (Eq,Show)
  type Rules     = [Rule]
  data Spec      = Pat String Rules Gens Morphisms ConceptDefs    -- deriving Show
  data Term      = Tf Morph (Concept,Concept)
                 | Tm Morph (Concept,Concept)
                 | Tc Factor (Concept,Concept)                           deriving Eq
  type Terms     = [Term]

  class Numbered a where
   nr :: a->Int
   pos :: a->FilePos
   nr x = nr (CC_aux.pos x)

  instance Numbered FilePos where
   nr (FilePos (fn,Pos l c,sym)) = l
   pos p = p

  instance Numbered Rule where
   pos (Hc antc p cons expla sgn nr) = p
   pos (Dc defd p expr expla sgn nr) = p
   pos (Gc p m expr sgn n)           = p

  instance Numbered Morph where
   pos (Mph nm p atts sgn m) = p
   pos (Id p atts c)         = p

  instance Numbered Factor where
   pos (F ts _)  = if not (null ts) then CC_aux.pos (head ts) else error "!!Software error 813. Please submit a complete bug report to your dealer"
   pos (Fu fs _) = if not (null fs) then CC_aux.pos (head fs) else error "!!Software error 814. Please submit a complete bug report to your dealer"
   pos (Fi fs _) = if not (null fs) then CC_aux.pos (head fs) else error "!!Software error 815. Please submit a complete bug report to your dealer"

  instance Numbered Term where
   pos (Tm m _)  = CC_aux.pos m
   pos (Tf m _)  = CC_aux.pos m
   pos (Tc f _)  = CC_aux.pos f

  class Explained a where
   explain :: a -> String

  instance Explained Rule where
   explain (Hc antc pos cons expla sgn nr) = expla
   explain (Dc defd pos expr expla sgn nr) = expla
   explain (Gc p m expr sgn n)             = ""

  class Conceptual a where
   conts      :: a -> [String]                   -- the set of all objects in a concept

  instance Conceptual a => Conceptual [a] where
   conts                                         = rd . concat . map conts

  instance Conceptual a => Conceptual (Classification a) where
   conts                                         = rd . concat . map conts . preCl

  instance Conceptual Concept where
   conts (C nm os) = os

  class Morphical a where
   concs      :: a -> [Concept]                  -- the set of all concepts used in data structure a
   conceptdefs:: a -> [ConceptDef]               -- the set of all concept definitions in the data structure
   conceptdefs x = []
   mors       :: a -> [Morph]                    -- the set of all morphs used within data structure a
   morphisms  :: a -> [Morphism]
   morphisms x = [m|Mph _ _ _ _ m<-mors x]

  instance Morphical a => Morphical [a] where
   concs                                         = rd . concat . map concs
   conceptdefs                                   = rd . concat . map conceptdefs
   mors                                          = rd . concat . map mors
   morphisms xs                                  = concat (map morphisms xs)

  instance Morphical a => Morphical (Classification a) where
   concs                                         = rd . concat . map concs . preCl
   conceptdefs                                   = rd . concat . map conceptdefs . preCl
   mors                                          = rd . concat . map mors . preCl
   morphisms cl                                  = morphisms (preCl cl)

  instance Morphical Morphism where
   concs (Mor _ a b _ _ _ _ _ _)                 = rd [a,b]
   concs (Idn a _)                               = [a]
   mors m                                        = []

  instance Morphical Morph where
   concs (Mph nm pos atts (a,b) m)               = rd ([a,b]++concs m)
   concs (Id pos atts c)                         = [c]
   mors m@(Mph _ _ _ _ _)                        = [m]
   mors m@(Id _ _ _)                             = []

  instance Morphical Gen where
   concs (G g s)                                 = rd [g,s]
   mors m                                        = []

  instance Morphical Term where
   concs (Tm m (a,b))                            = rd ([a,b]++concs m)
   concs (Tf m (a,b))                            = rd ([a,b]++concs m)
   concs (Tc f (a,b))                            = rd ([a,b]++concs f)
   mors (Tm m sgn)                               = mors m
   mors (Tf m sgn)                               = mors m
   mors (Tc f sgn)                               = mors f

  instance Morphical Factor where
   concs (F ts (a,b))                        = rd ([a,b]++concs ts)
   concs (Fu fs (a,b))                       = rd ([a,b]++concs fs)
   concs (Fi fs (a,b))                       = rd ([a,b]++concs fs)
   mors (F ts sgn)                           = mors ts
   mors (Fu fs sgn)                          = mors fs
   mors (Fi fs sgn)                          = mors fs

  instance Morphical Rule where
   concs (Hc antc pos cons expla (a,b) nr)       = rd ([a,b]++concs antc++concs cons)
   concs (Dc defd pos expr expla (a,b) nr)       = rd ([a,b]++concs defd++concs expr)
   concs (Gc pos m expr (a,b) nr)                = rd ([a,b]++concs m++concs expr)
   mors (Hc antc pos cons expla sgn nr)          = rd (mors antc++mors cons)
   mors (Dc defd pos expr expla sgn nr)          = rd (mors defd++mors expr)
   mors (Gc pos m expr sgn nr)                   = rd (mors m++mors expr)

  instance Morphical Spec where
   concs (Pat nm rs gen pms cs)            = rd (concs rs++concs gen++concs pms)
   conceptdefs (Pat nm rs gen pms cs)      = cs
   mors (Pat nm rs gen pms cs)             = mors rs
   morphisms (Pat nm rs parChds pms cs)    = pms

  instance Morphical Context where
   concs (Ctx nm on isa gE world dc ms cs)       = rd (concs ms++concs dc)
   conceptdefs (Ctx nm on isa gE world dc ms cs) = cs
   mors (Ctx nm on isa gE world dc ms cs)        = mors dc
   morphisms (Ctx nm on isa gE world dc ms cs)   = morphisms dc++ms

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
   showsPrec p (Ctx nm on isa gE world dc ms cs)
    = showString ("CONTEXT "++nm++
                  (if on==[] then "" else " EXTENDS "++chain ", " on)++"\n"++
                  chain "\n\n" (map show dc)++"\n"++
                  chain "\n" (map show ms)++"\nENDCONTEXT" ) -}

-- The function showHS prints structures as haskell source, which is intended for testing.

  class ShowHS a where
   showHS :: a -> String

  instance ShowHS Prop where
   showHS Uni = "Uni"
   showHS Inj = "Inj"
   showHS Sur = "Sur"
   showHS Tot = "Tot"
   showHS Sym = "Sym"
   showHS Asy = "Asy"
   showHS Trn = "Trn"
   showHS Rfx = "Rfx"

  instance ShowHS a => ShowHS [a] where
   showHS = chain "\n".map showHS

  instance ShowHS Context where
   showHS (Ctx nm on isa gE world dc ms cs)
    = nlHs++"ctx_"++nm++"\n>   = Ctx "++show nm++" "++show on++" isa (genEq (typology isa)) []"++
      ind++showL ["pat_"++name p|p<-dc]++
      ind++showL ["mor_"++name m++name(source m)++name(target m)|m<-ms]++
      init nlHs'++"where"++nlHs'++
      "isa = "++showHS isa++
      concat [nlHs'++showHS m|m<-ms]++"\n"++
      showHS dc++
      concat ["\n\nMorphisms from "++name pat++"\n"++concat[nlHs'++showHS m|m<-morphisms pat]|pat<-dc]
      where nlHs = "\n>  "; ind = nlHs++"       "; nlHs' = nlHs++"    "

  instance ShowHS Spec where
   showHS (Pat nm rules gen pms cs)
    = nlHs++"pat_"++nm++nlHs'++"= Pat "++show nm++
      (if null rules then " []" else ind++"[ "++chain (ind++", ") [showHS r| r<-rules]++ind++"]")++
      (if null gen   then " []" else ind++"[ "++chain (ind++", ") [showHS g| g<-gen] ++ind++"]")++
      (if null gen   then " []" else ind++"[ "++chain (ind++", ") ["mor_"++name m++name(source m)++name(target m)| m<-pms] ++ind++"]")++
      init nlHs'
      where nlHs = "\n>      "; ind = nlHs++"       "; nlHs' = nlHs++"    "

  instance ShowHS Rule where
   showHS r@(Hc antc p cons expla sgn nr)
    = chain " " ["Hc","("++showHS antc++")","posNone","("++showHS cons++")",show(explain r),showSgn sgn,show nr]
   showHS r@(Dc defd p expr expla sgn nr)
    = chain " " ["Dc","("++show defd++")","posNone","("++showHS expr++")",show(explain r),showSgn sgn,show nr]
   showHS (Gc p m expr sgn nr)
    = chain " " ["Gc","posNone","("++showHS m++")","("++showHS expr++")",showSgn sgn,show nr]

  instance ShowHS Factor where
   showHS (F ts sgn)
    = chain " " ["F",showL (map showHS ts),showSgn sgn]
   showHS (Fu fs sgn)
    = chain " " ["Fi",showL (map showHS fs),showSgn sgn]
   showHS (Fi fs sgn)
    = chain " " ["Fi",showL (map showHS fs),showSgn sgn]

  showSgn (a,b) = "("++showHS a++","++showHS b++")"
  showSign (C a _,C b _) = "["++a++"*"++b++"]"
  showL xs = "["++chain "," xs++"]"

  instance ShowHS a => ShowHS (Inheritance a) where
   showHS (Isa ts cs) = "Isa "++showL ["("++showHS g++","++showHS s++")"|(g,s)<-ts] ++" "++ showL (map showHS cs)

  instance ShowHS Concept where
   showHS (C nm _) = "C "++show nm

  instance ShowHS Morphism where
   showHS (Mor nm a b props prL prM prR cs pos)
    = chain " " ["mor_"++nm++name a++name b,"= Mor",show nm,"("++showHS a++")","("++showHS b++")",showL(map showHS props),show prL,show prM,show prR,"[]",show pos]
   showHS (Idn a pos)
    = ""

  instance ShowHS Morph where
   showHS (Mph nm pos atts sgn@(a,b) m)
    = chain " " ["Mph",show nm,"posNone",showL(map showHS atts),showSgn sgn,"mor_"++nm++name a++name b]
   showHS (Id pos atts c)
    = chain " " ["Id","posNone",showL(map showHS atts),name c]

  instance ShowHS Term where
   showHS (Tm m sgn) = "Tm ("++showHS m++") "++showSgn sgn
   showHS (Tf f sgn) = "Tf ("++showHS f++") "++showSgn sgn
   showHS (Tc f sgn) = "Tc ("++showHS f++") "++showSgn sgn

  instance ShowHS Gen where
   showHS (G g s) = "G ("++show s++") ("++show g++")"

--   instance Show Spec where
--    showsPrec p (Pat nm rules gen pms cs)
--     = showString ("PATTERN "++nm++"\n"++
--                   pr gen++['\n'| not (null gen)]++
--                   pr pms++['\n'| not (null pms)]++
--                   pr rules++"\nENDPATTERN" )
--       where pr xs = chain "\n" [" "++show x| x<-xs]

--   instance Show Rule where
--    showsPrec p r@(Hc antc _ cons _ _ _)
--     = showString (show antc++" -: "++show cons++"\n  EXPLANATION "++explain r++"\n("++show (pos r)++")")
--    showsPrec p r@(Dc defd _ expr _ _ _)
--     = showString (show defd++" = "++show expr++"\n  EXPLANATION "++explain r++"\n("++show (pos r)++")")
--    showsPrec p r@(Gc _ m expr _ _)
--     = showString ("GLUE "++show m++" = "++show expr++"\n("++show (pos r)++")")

-- This show is used in error messages. It should therefore not display the factor's type

  instance Show Factor where
   showsPrec p (F ts sgn)
    = showString (chain ";" (map show ts))
   showsPrec p (Fu fs sgn)
    = showString ("("++chain "\\/" (map show fs)++")")
   showsPrec p (Fi fs sgn)
    = showString ("("++chain "/\\" (map show fs)++")")

  instance Show Concept where
   showsPrec p (C nm _) = showString (nm)

-- This show is used in error messages. It should therefore not display the morph's type

  instance Show Morph where
   showsPrec p (Mph nm pos  []  sgn m) = showString (nm  {- ++"("++show a++"*"++show b++")" where (a,b)=sgn -} )
   showsPrec p (Mph nm pos atts sgn m) = showString (nm  {- ++"["++chain "*" (map name atts)++"]" -} )
   showsPrec p (Id pos atts c)         = showString ("I" {- ++ "["++name c++"]" -} )

  instance Show Morphism where
   showsPrec p (Mor nm a b props prL prM prR cs pos)
    = showString (chain " " [nm,"::",name a,"*",name b,show props,"PRAGMA",show prL,show prM,show prR])
   showsPrec p (Idn a pos)
    = showString ""

-- This show is used in error messages. It should therefore not display the term's type

  instance Show Term where
   showsPrec p (Tm m sgn) = showString (show m)
   showsPrec p (Tf f sgn) = showString (show f++"~")
   showsPrec p (Tc f sgn) = showString ("("++show f++")")

  instance Show Gen where
   showsPrec p (G g s) = showString ("GEN "++show s++" ISA "++show g)

  fEmpty (F  [] sgn) = True
  fEmpty (Fu [] sgn) = True
  fEmpty (Fi [] sgn) = True
  fEmpty     _       = False

  instance Eq Morphism where
--   I nm == I nm' = True
   m == m' = mor2name m==mor2name m' && source m==source m' && target m==target m'
  instance Eq Morph where
   m == m' = mor2name m==mor2name m' && source m==source m' && target m==target m'

-- A morphism stands for a relation between two concepts. Mathematically, we
-- interpret a morphism as a relation, i.e. a subset of the cartesian product of two sets.
-- m::morphism  means interpret m `subsetEq` interpret (source m) x interpret (target m).
-- Every morphism m has cardinalities, in which
--   Uni:    forall x,y,y': x m y & x m y' => y==y'           (~m;m `subsetEq` I (target m))
--   Tot:    forall x: exists y: x m y                        (I (source m) `subsetEq` m;~m)
--   Inj:    forall x,x',y: x m y & x' m y => x==x'           (m;~m `subsetEq` I (source m))
--   Sur:    forall y: exists x: x m y                        (I (target m) `subsetEq` ~m;m)
--   Trn:    forall x,y,z: x m y & y m z => x m z             (m;m `subsetEq` m)
--   Asy:    forall x,y: x m y & y m x => x==y                (m & ~m `subsetEq` I)
--   Sym:    forall x,y: x m y <=> y m x                      (m = ~m)
--   Rfx:    forall x: x m x                                  (I `subsetEq` m)

  class Morphic a where
   source, target  :: a -> Concept
   cards     :: a -> [Prop]
   cards m    = []
   flp       :: a -> a
   isIdent   :: a -> Bool
   isIdent m  = False
   equiv     :: a -> a -> Bool
   equiv m m' = source m==source m'&&target m==target m' || source m==target m'&&target m==source m'
   contents  :: a -> [Drecord]

  instance Morphic a => Morphic [a] where
   source []      = C "Anything" []
   source as      = source (head as)
   target []      = C "Anything" []
   target as      = target (last as)
   cards []    = []
   cards as    = (foldr1 isc . map cards) as
   flp         = map flp.reverse
   isIdent as  = and [isIdent m| m<-as]
   contents as = foldr join [] (map contents as)
                 where join::[Drecord]->[Drecord]->[Drecord]
                       join a b = merge ((sort' (last.head).eqCl last) a)
                                        ((sort' (head.head).eqCl head) b)
                       merge (xs:xss) (ys:yss)
                        | last (head xs)<head (head ys) = merge xss (ys:yss)
                        | last (head xs)>head (head ys) = merge (xs:xss) yss
                        | otherwise = [[x,y]|[x,i]<-xs,[j,y]<-ys]++ merge xss yss
                       merge _ _ = []

  instance Morphic Morph where
   source (Mph nm pos atts (a,b) m) = a
   source (Id pos atts c)           = c
   target (Mph nm pos atts (a,b) m) = b
   target (Id pos atts c)           = c
   cards (Mph nm pos atts sgn m)    = cards m
   cards (Id pos atts c)            = [Uni,Tot,Sur,Inj,Sym,Trn,Rfx]
   flp (Mph nm pos atts (a,b) m)    = Mph nm pos (reverse atts) (b,a) (flp m)
   isIdent (Mph _ _ _ _ _ )         = False
   isIdent (Id _ _ _ )              = True
   contents (Mph _ _ _ _ m)         = contents m
   contents (Id _ _ c)              = [[o,o] | o<-conts c]

  instance Morphic Morphism where
   source   (Mor nm a b props prL prM prR cs pos) = a
   source   (Idn a pos)                           = a
   target   (Mor nm a b props prL prM prR cs pos) = b
   target   (Idn a pos)                           = a
   cards (Mor nm a b props prL prM prR cs pos)    = props
   cards (Idn _ _)                                = [Uni,Tot,Sur,Inj,Rfx,Trn,Sym]
   flp   (Mor nm a b props prL prM prR cs pos)    = Mor nm b a (flipProps props) "" "" "" (map reverse cs) pos
   flp    i                                       = i
   isIdent _                                      = False
   contents (Mor _ _ _ _ _ _ _ cs _)              = cs
   contents (Idn _ _)                             = []

  instance Morphic Term where
   source (Tm mm (a,b))   = a
   source (Tf mm (a,b))   = a
   source (Tc f (a,b))    = a
   target (Tm mm (a,b))   = b
   target (Tf mm (a,b))   = b
   target (Tc f (a,b))    = b
   cards (Tm mm sgn)   = cards mm
   cards (Tf mm sgn)   = flipProps (cards mm)
   cards (Tc f sgn)    = cards f
   flp (Tm mm (a,b))   = Tf mm (b,a)
   flp (Tf mm (a,b))   = Tm mm (b,a)
   isIdent (Tm mm sgn) = isIdent mm
   isIdent (Tf mm sgn) = isIdent mm
   contents (Tm mm _)  = contents mm
   contents (Tf mm _)  = map reverse (contents mm)

  instance Morphic Factor where
   source (F ts (a,b))    = a
   source (Fu fs (a,b))   = a
   source (Fi fs (a,b))   = a
   target (F ts (a,b))    = b
   target (Fu fs (a,b))   = b
   target (Fi fs (a,b))   = b
   cards (F ts sgn)    = foldr isc [Uni,Tot,Sur,Inj] (map cards ts)
   cards (Fu fs sgn)   = []
   cards (Fi fs sgn)   = []
   flp (F ts (a,b))    = F (flp ts) (b,a)
   flp (Fu fs (a,b))   = Fu (map flp fs) (b,a)
   flp (Fi fs (a,b))   = Fi (map flp fs) (b,a)
   isIdent (F ts sgn)  = and [isIdent t| t<-ts]
   isIdent (Fu fs sgn) = and [isIdent f| f<-fs]
   isIdent (Fi fs sgn) = and [isIdent f| f<-fs]
   contents (F ts _)   = contents ts
   contents (Fu fs _)  = (foldr1 uni.map contents) fs
   contents (Fi fs _)  = (foldr1 isc.map contents) fs

  class Substitutive a where
-- Precondition: functional s
-- betekenis: Als (g,c) `elem` s dan wordt elk voorkomen van g in x vervangen door c in subsC s x
   subsC     :: GenR -> [(Concept,Concept)] -> a -> a
   subsR     :: GenR -> Rule -> a -> a  -- Note: the rule must be a Gc
   subsR gE r x = x
   limit     :: (Concept,Concept) -> a -> a

  instance (Morphic a,Substitutive a) => Substitutive [a] where
   subsC gE c xs = map (subsC gE c) xs
   subsR gE r xs = map (subsR gE r) xs
   limit sgn        [x]   = [limit sgn x]
   limit sgn@(a,b) (x:xs) = limit (a,target x) x:limit (source xs,b) xs
   limit sgn         []   = []

  instance Substitutive Morph where
   subsC gE s (Mph nm pos atts (a,b) m)
    = Mph nm pos atts (head ([d| (g,d)<-s, g==a]++[a]),head ([c| (g,c)<-s, g==b]++[b])) m
   subsC gE s (Id pos atts c)
    = Id pos atts (head ([d| (g,d)<-s, g==c]++[c]))
   limit sgn (Mph nm pos atts (a,b) m) = Mph nm pos atts sgn m
   limit (a,b) (Id pos atts c) = if a==b then Id pos atts a
                                 else error ("!Err: equal "++show a++" and "++show b++" expected.")

  instance Substitutive Term where
   subsC gE s (Tm m sgn) = Tm m' (source m',target m') where m' = subsC gE s m
   subsC gE s (Tf m sgn) = Tf m' (source m',target m') where m' = subsC gE s m
   subsC gE s (Tc f sgn) = Tc f' (source f',target f') where f' = subsC gE s f
   limit sgn' (Tm m sgn) = Tm (limit sgn' m) sgn'
   limit sgn' (Tf f sgn) = Tf (limit sgn' f) sgn'
   limit sgn' (Tc f sgn) = Tc (limit sgn' f) sgn'


  instance Substitutive Factor where
   subsC gE s (F ts sgn)  = subs (map (subsC gE s) ts)
    where subs ts = F ts (source (head ts),target (last ts))
   subsC gE s (Fu fs sgn) = subs (map (subsC gE s) fs)
    where subs fs = Fu fs (foldr1 (lub gE) [source f|f<-fs],foldr1 (lub gE) [target f|f<-fs])
   subsC gE s (Fi fs sgn) = subs (map (subsC gE s) fs)
    where subs fs = Fi fs (foldr1 (lub gE) [source f|f<-fs],foldr1 (lub gE) [target f|f<-fs])
   subsR gE (Gc pos m (F ts sgn) sgn' nr) f@(F ts'' sgn'')
    = head ([ F ts' (source (head ts'), target (last ts'))
            | (ts',n)<-[subs (Tm m (source m,target m)) ts ts'', subs (Tf m (target m,source m)) (flp ts) ts''],n>0]++[f])
     where
--      subs :: Eq a => a->[a]->[a]->([a],Int)
      subs m part whole = splits m [] [] part whole 0
       where
        splits m out state (i:ins) (t:ts) n
         | i `rgE` t  = splits m out state' ins ts n
         | otherwise  = splits m (out++[head state']) [] part (tail (state'++ts)) n
         where state' = state++[t]
        splits m out state [] ts n
         = splits m (out++[m]) [] state ts (n+1)
        splits m out state ins [] n = (out++state, n)
        i `rgE` t = if name i==name t then source t `gE` source i && target t `gE` target i else False
   subsR gE s (Fu fs sgn'') = subs (map (subsR gE s) fs)
    where subs fs = Fu fs (foldr1 (lub gE) [source f|f<-fs],foldr1 (lub gE) [target f|f<-fs])
   subsR gE s (Fi fs sgn'') = subs (map (subsR gE s) fs)
    where subs fs = Fi fs (foldr1 (lub gE) [source f|f<-fs],foldr1 (lub gE) [target f|f<-fs])
   limit sgn' (F ts sgn)  = F  (limit sgn' ts) sgn'
   limit sgn' (Fi fs sgn) = Fi (map (limit sgn') fs) sgn'
   limit sgn' (Fu fs sgn) = Fi (map (limit sgn') fs) sgn'


-- Identified Term is necessary for subs only.
  instance Identified Term where
   name (Tm m sgn) = name m
   name (Tf m sgn) = name m++"~"
   name (Tc f sgn) = ""       -- this is wrong! Todo: find out about Tc

  instance Substitutive Rule where
   subsC gE s (Hc antc pos cons expla sgn nr)
    = Hc antc' pos cons' expla (lub gE (source antc') (source cons'),lub gE (target antc') (target cons')) nr
      where antc' = subsC gE s antc
            cons' = subsC gE s cons
   subsC gE s (Dc defd pos expr expla sgn nr)
    = Dc defd' pos expr' expla (lub gE (source defd') (source expr'),lub gE (target defd') (target expr')) nr
      where defd' = subsC gE s defd
            expr' = subsC gE s expr
   subsC gE s (Gc pos m expr sgn nr)
    = Gc pos m' expr' (lub gE (source m') (source expr'),lub gE (target m') (target expr')) nr
      where m'    = subsC gE s m
            expr' = subsC gE s expr
   subsR gE s r@(Hc antc pos cons expla sgn nr)
    = Hc (limit sgn' antc') pos (limit sgn' cons') expla sgn' nr
      where antc' = subsR gE s antc
            cons' = subsR gE s cons
            sgn'  = (lub gE (source antc') (source cons'),lub gE (target antc') (target cons'))
   subsR gE s r@(Dc defd pos expr expla sgn nr)
    = Dc defd' pos expr' expla (lub gE (source defd') (source expr'),lub gE (target defd') (target expr')) nr
      where defd' = subsR gE s defd
            expr' = subsR gE s expr
   subsR gE s r@(Gc pos m expr sgn nr)
    = Gc pos m' expr' (lub gE (source m') (source expr'),lub gE (target m') (target expr')) nr
      where m'    = subsR gE s m
            expr' = subsR gE s expr
   limit sgn r = error "Should not call `limit' on rules"

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
   name (C nm _) = nm
  instance Identified Morph where
   name (Mph nm pos atts sgn m) = nm
   name (Id pos atts c) = "I"
  instance Identified Morphism where
   name (Mor nm _ _ _ _ _ _ _ _) = nm
   name (Idn _ _)                = "I"

  applyM (Mor _ _ _ _ prL prM prR _ _) d c = prL++d++prM++c++prR++"."
  applyM (Idn _ _)                     d c = d++" = "++c

  normRule :: (Concept->Concept->Concept) -> Rule -> Rule
  normRule upb (Hc (F ants sgna) pos (F cons sgnb) expla sgn nr)
   | and(map isIdent(mors ants)) = Hc (F [Tm (Id pos [] idA) (idA,idA)] (idA,idA)) pos (F cons sgnb) expla (idC,idC) nr
   | otherwise                   = Hc (F as (source as,target as)) pos (F cs (source cs,target cs)) expla 
                                      (source as `upb` source cs, target as `upb` target cs) nr
   where
    idC = source cons `upb` target cons `upb` idA
    idA = foldr upb (target (last ants)) (map source ants)
    (as,cs)  = move ants cons
    move :: [Term] -> [Term] -> ([Term],[Term])
    move [] cs = ([Tm (Id pos [] (source cs)) (source cs,source cs)],cs)
    move as cs
     | sur (cards h) && inj (cards h) = move (tail as) ([flp h]++cs)
     | fun (cards l) && tot (cards l) = move (init as) (cs++[flp l])
     | otherwise      = (as,cs)
     where h=head as; l=last as
  normRule upb r = r

  class Show a => ABoolAlg a where
   glb :: (a -> a -> Bool) -> a -> a -> a
   glb gE a b | b `gE` a  = b
              | a `gE` b  = a
              | otherwise = error ("glb undefined: a="++show a++", b="++show b)
   lub :: (a -> a -> Bool) -> a -> a -> a
   lub gE a b | a `gE` b  = b
              | b `gE` a  = a
              | otherwise = error ("lub undefined: a="++show a++", b="++show b)
   order :: (a -> a -> Bool) -> a -> a -> Bool
   order gE a b | a `gE` b  = True
                | b `gE` a  = True
                | otherwise = False

  type GenR      = Concept->Concept->Bool

  makeConceptSpace :: Morphisms -> Concepts
  makeConceptSpace ms
   = [ C (name (fst (head raw))) (sord (concat (map snd raw)))
     | raw <- eqCl fst [(c,os)| Mor _ s t _ _ _ _ ds _ <- ms
                              , (c,os) <- [(s,rd (map src ds)),(t,rd (map trg ds))]]
     ]

--  raw                           :: [[(Concept,[String])]]
--  map (map snd) raw             :: [[[String]]]
--  map (concat.map snd) raw      :: [[String]]
--  map (sord.concat.map snd) raw :: [[String]]
--  map (map fst) raw             :: [[Concept]]
--  map (head.map fst) raw        :: [Concept]
 
--    restrct gE [] ts    = ts
--    restrct gE [a,b] ts = take 1 [(a,b)| (d,c)<-ts, d `gE` a, c `gE` b]   -- take 1 is more efficient than rd
--    restrct gE atts ts  = restrct gE [head atts,last atts] ts

-- irred removes redundancy from a list of signatures.
-- Precondition: a `gE` a' && b `gE` b' || a' `gE` a && b' `gE` b

  irred :: GenR -> [(Concept,Concept)] -> [(Concept,Concept)]
  irred gE signs = map (foldr1 bound) (eqClass (ordr gE) signs)
   where ordr gE (a,b) (a',b') = order gE a a' && order gE b b'
         (a,b) `bound` (a',b') = (lub gE a a', lub gE b b')

  irredM :: GenR -> [Morphism] -> [Morphism]
  irredM gE ms = map (foldr1 bound) (eqClass (ordr gE) ms)
   where ordr gE m m' = order gE (source m) (source m') && order gE (target m) (target m')
         m `bound` m' = if source m `gE` source m' && target m `gE` target m' then m' else m

  instance Identified Context where
   name (Ctx nm _ _ _ _ _ _ _) = nm

  instance Identified Spec where
   name (Pat nm _ _ _ _) = nm

-- Interpretation of context as a language means to describe the classification tree,
-- the set of morphisms and the rules that apply in that context. Inheritance of
-- properties is achieved as a result.


  union :: Spec -> Spec -> Spec
  union (Pat nm rs parChds pms cs) (Pat nm' rs' parChds' pms' cs')
    = Pat nm' (rd(rs++rs')) (rd(parChds++parChds')) (rd(pms++pms')) (rd(cs++cs'))

  class Language a where
    rules     :: a -> [Rule]
    specs     :: a -> [Rule]
    grules    :: a -> [Rule]
    grules a  = rules a ++ [Dc m pos expr "" sgn n | Gc pos m expr sgn n<-specs a]
    patterns  :: a -> [Spec]
    isa       :: a -> Inheritance Concept

  instance Language a => Language [a] where
   rules xs = (concat. map rules) xs
   specs xs = (concat. map specs) xs
   patterns = rd' name.concat.map patterns
   isa      = foldr uni empty.map isa

  instance Language a => Language (Classification a) where
   rules cl    = rules (preCl cl)
   specs cl    = specs (preCl cl)
   patterns cl = patterns (preCl cl)
   isa         = foldr uni empty.map isa.preCl

  instance Language Rule where
   rules   r@(Gc pos m expr sgn n) = [Dc m pos expr (name m++" is implemented using "++enumerate (map name (mors expr))) sgn (nr r)]
   rules   r                       = [r]
   specs   r@(Gc _ _ _ _ _)        = [r]
   specs   r                       = []
   patterns r = [Pat "" [r] [] [] []]
   isa (Hc antc _ cons _ _ _)
     = Isa tuples (rd(concs antc++concs cons)>-rd [e|(a,b)<-tuples,e<-[a,b]])
       where tuples = clear [(source antc,source cons),(target antc,target cons)]
   isa (Dc defd _ expr _ _ _)
     = Isa tuples (concs expr>-rd [e|(a,b)<-tuples,e<-[a,b]])
       where tuples = clear [(source expr,source defd),(target expr,target defd)]
   isa (Gc _ m expr _ _)
     = Isa tuples (concs expr>-rd [e|(a,b)<-tuples,e<-[a,b]])
       where tuples = clear [(source expr,source m),(target expr,target m)]

  clear abs = rd [(a,b)| (a,b)<-abs, a/=b]
  clearG abs = rd [G g s| G g s<-abs, g/=s]

  instance Language Spec where
   rules (Pat nm rs parChds pms cs) = [r|r@(Hc antc pos cons expla sgn nr)<-rs] ++
                                      [r|r@(Dc defd pos expr expla sgn nr)<-rs] ++
                                      (rules.rd.morphisms) rs
   specs (Pat nm rs parChds pms cs) = [r|r@(Gc pos m expr sgn nr)<-rs]
   patterns p                       = [p]
   isa   (Pat nm rs parChds pms cs) = Isa ts (singles>-rd [e| G g s<-parChds,e<-[g,s]])
                                      where Isa tuples singles = isa rs
                                            ts = clear (tuples++[(g,s)| G g s<-parChds])

  instance Language Context where
   rules    (Ctx nm on i gE world dc ms cs) = rules (foldr union (Pat "" [] [] [] []) dc)
   specs    (Ctx nm on i gE world dc ms cs) = specs (foldr union (Pat "" [] [] [] []) dc)
   patterns (Ctx nm on i gE world dc ms cs) = dc
   isa      (Ctx nm on i gE world dc ms cs) = i

  instance Language Morphism where
   rules m@(Mor nm a b props prL prM prR cs pos)
    = [h p| p<-props, p `elem` [Sym,Asy,Trn,Rfx]
          , if a==b then True else error ("!Err: Property "++show p++" requires equal source and target domains (you specified "++name a++" and "++name b++").") ]
     where h Sym = Dc r pos (F [Tf r sgn] sgn) (nm++"["++name a++"*"++name a++"] is symmetric.") sgn 0
           h Asy = Hc (Fi [F [Tm r sgn] sgn, F [Tf r sgn] sgn] sgn) pos id (nm++"["++name a++"*"++name a++"] is antisymmetric.") sgn 0
           h Trn = Hc (F [Tm r sgn, Tm r sgn] sgn) pos (F [Tm r sgn] sgn) (nm++"["++name a++"*"++name a++"] is transitive.") sgn 0
           h Rfx = Hc id pos (F [Tm r sgn] sgn) (nm++"["++name a++"*"++name a++"] is reflexive.") sgn 0
           sgn   = (a,a)
           r     = Mph nm pos [a] sgn m
           id    = F [Tm (Id pos [] a) sgn] sgn
   rules _    = []
   specs _    = []
   patterns _ = []
   isa _      = empty

-- (SJ: I have no idea why the number 0 sits at the end of Hc.)

  genE :: Context -> GenR
  genE (Ctx nm on i gE world dc ms cs) = gE
  nogE a b = False

  wrld :: Context -> [Classification Context]
  wrld (Ctx nm on i gE world dc ms cs) = world

-- Graph layout (using GraphViz)

  class Graphic a where
   dotGraph :: String -> String -> [[String]] -> a -> String

  instance Graphic Spec where
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
        arcs = rd (mors pat++mors(specs pat)++[Mph (name m) posNone [source m,target m] (source m,target m) m| m<-morphisms pat])
        introG = "\n   { style=\"filled\"; color=\".60 .27 1.0\"\n   ; node [shape=plaintext,fontsize=12,style=bold]\n   ; edge [dir=none]\n   ; {node [shape=ellipse,style=filled,fontsize=9,font=helvetica]"

  instance Graphic Context where
   dotGraph nm len rRelationList (Ctx cnm on isa gE world dc ms cs)
    = dotGraph nm len rRelationList (foldr union (Pat "" [] [] [] []) dc)

  instance Graphic Morph where
   dotGraph nm len rRelationList m@(Mph nm' _ atts (a,b) m')
    = dotGraph nm len rRelationList (Pat nm [Dc m posNone (F [] (a,b)) "" (a,b) 0] [] [] [])
--   dotGraph nm len rRelationList m@(Id pos atts c)
--    = dotGraph nm len rRelationList (Pat nm [Dc m posNone (F [] (c,c)) "" (c,c) 0] [] [] [])

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

-- Position calculation in the parser

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
  
  mor2 :: Morph -> [String]
  mor2 m = [mor2name m,name (source m),name (target m)]

  mor2filename m = "Atlas"++relName (mor2 m)++".html"
  relName [r,d,c] = strip(r++conceptForm d++conceptForm c)
