 module CC where
  import UU_Scanner
  import UU_Parsing
  import Auxiliaries
  import Classification
  import Typology
  import CC_aux
  import AGtry

  keywordstxt      = [ "CONTEXT", "ENDCONTEXT", "EXTENDS"
                     , "PATTERN", "ENDPATTERN"
                     , "POPULATION", "ENDPOPULATION"
                     , "UNI", "INJ", "SUR", "TOT", "SYM", "ASY", "TRN", "RFX"
                     , "RELATION", "CONCEPT"
                     , "IMPORT", "GLUE", "GEN", "ISA", "I"
                     , "PRAGMA", "EXPLANATION"
                     ]
  keywordsops      = [ "-:", "=", "~", "-", ";", "*", "::", "\\/", "/\\" ]
  specialchars     = "()[].,"
  opchars          = "-~:=;*\\/"

  data ContextElement = CPat Pattern
                      | CMor Signature
                      | CCon ConceptDef
  data PatElem        = Pr Rule
                      | Pg Gen
                      | Pm Signature
                      | Pc ConceptDef


-- Top level expression, with all expression related parsers (implicitly)
-- parameterized with operator expression constructor

  pArchitecture    :: Parser Token Architecture
  pArchitecture     = Arch <$> pList1 pContext

  pContext         :: Parser Token Context
  pContext          = rebuild <$ pKey "CONTEXT" <*> pConid <*>
                                 ((pKey "EXTENDS" *> pList1Sep (pSpec ',') pConid) `opt` []) <*>
                                 pList pContextElement <* pKey "ENDCONTEXT"
                      where rebuild nm on ces
                             = Ctx nm on empty [] [p| CPat p<-ces] [m| CMor m<-ces] [c| CCon c<-ces]

  pContextElement  :: Parser Token ContextElement
  pContextElement   = CPat <$> pPattern    <|>
                      CMor <$> pSignature   <|>
                      CCon <$> pConceptDef

  pPattern         :: Parser Token Pattern
  pPattern          = rebuild <$ pKey "PATTERN" <*> (pConid <|> pString)
                              <*> pList pPatElem
                              <* pKey "ENDPATTERN"
                      where rebuild nm pes = Pat nm [r|Pr r<-pes] [gen |Pg gen<-pes] [m| Pm m<-pes] [c| Pc c<-pes]

  pPatElem         :: Parser Token PatElem
  pPatElem          = Pr <$> pRule       <|>
                      Pg <$> pGen        <|>
                      Pm <$> pSignature  <|>
                      Pc <$> pConceptDef

  pRule            :: Parser Token Rule
  pRule             = hc <$> pExpr <*> pKey_pos "-:" <*> pExpr <*> (pKey "EXPLANATION" *> pString `opt` "") <|>
                      dc <$> pExpr <*> pKey_pos "="  <*> pExpr <*> (pKey "EXPLANATION" *> pString `opt` "") <|>
                      gc <$> pKey_pos "GLUE" <*> pMorphism <* pKey "=" <*> pExpr
                      where
                       hc antc pos cons expl = Ru 'I' antc pos cons expl (Anything (==),Anything (==)) 0
                       dc defd pos expr expl = Ru 'E' defd pos expr expl (Anything (==),Anything (==)) 0
                       gc pos pm pf = Gc pos pm pf (Anything (==),Anything (==)) 0

  pGen             :: Parser Token Gen
  pGen              = rebuild <$ pKey "GEN" <*> (pConid <|> pString) <* pKey "ISA" <*> (pConid <|> pString)
                      where rebuild spec genus = G (C genus (==) []) (C spec (==) [])

  postStr          :: Parser Token String
  postStr           = f <$> pList1 (pKey "~" <|> pKey "-")
                      where
                       f xs = g ['~'|'~'<-concat xs] ++ g ['-'|'-'<-concat xs]
                       g xs = if odd (length xs) then take 1 xs else [] --if null xs then "" else error (show xs) -- 

-- There are always two or more factors in a factorU.

  pExpr            :: Parser Token Expression
  pExpr             = f <$> pList1Sep (pKey "\\/") pFactorI
                      where f [x] = x
                            f  xs = Fu xs

-- There are always two or more factors in a factorU.

  pFactorI         :: Parser Token Expression
  pFactorI          = f <$> pList1Sep (pKey "/\\") pFactor
                      where f [x] = x
                            f  xs = Fi xs

-- There are always one or more terms in a factor. F [] cannot occur

  pFactor          :: Parser Token Expression
  pFactor           = f <$> pList1Sep (pKey ";") pTerm
                      where f [Tc f' True] = f'
                            f ts           = F ts

  pTerm            :: Parser Token Expression
  pTerm             = tm <$> pMorphism <*> (pKey "~" `opt` "")                               <|>
                      tc <$> (pSpec '(' *> pExpr <* pSpec ')') <*> (pKey "~" `opt` "")
                      where
                       tm pm ""   = Tm pm       True
                       tm pf "~"  = Tf pf       True
                       tm pf "-"  = Tf pf       False
                       tm pf "~-" = Tf pf       False
                       tc pc ""   = Tc pc       True
                       tc pc "~"  = Tc (flp pc) True
                       tc pc "-"  = Tc pc       False
                       tc pc "~-" = Tc (flp pc) False
                       flp (F  ts)  = F  (reverse (map f1p ts))
                       flp (Fi fs)  = Fi (map flp fs)
                       flp (Fu fs)  = Fu (map flp fs)
                       f1p (Tm m c) = Tf m       c
                       f1p (Tf m c) = Tm m       c
                       f1p (Tc f c) = Tc (flp f) c

  pMorphism        :: Parser Token Morphism
  pMorphism         = rebuild <$> pVarid_val_pos <*> ((pSpec '[' *> pListSep (pKey "*") pConcept <* pSpec ']') `opt` []) <|>
                      iden <$> pKey_pos "I" <*> ((pSpec '[' *> pConcept <* pSpec ']') `opt` Anything (==))
                      where rebuild (nm,pos) atts = Mph nm pos atts (Anything (==),Anything (==))
                                                     (Sgn nm (Anything (==)) (Anything (==)) [] "" "" "" [] posNone)
                            iden pos (C "" _ _)   = Id pos []  (Anything (==))
                            iden pos (Anything _) = Id pos []  (Anything (==))
                            iden pos c            = Id pos [c] c

  pConcept         :: Parser Token Concept
  pConcept          = c <$> (pConid <|> pString)
                      where c str = C str (==) []

  pConceptDef      :: Parser Token ConceptDef
  pConceptDef       = Cd <$> pKey_pos "CONCEPT" <*> (pConid <|> pString) <*> pString <*> pString

--   pSignature        :: Parser Token Signature
--   pSignature         = rebuild <$ pVarid <*> pKey_pos "::" <*> pList1Sep (pKey "*") (pConid <|> pString) 
--                                <* (pProps `opt` []) <*> (pPragma `opt` [])
--                                <* (pContent `opt` []) <* pSpec '.'
--                        where rebuild nm pos atts props pragma content
--                               = Sgn nm (C (ats!!0) (==) []) (C (ats!!1) (==) []) props (pr!!0) (pr!!1) (pr!!2) content pos
--                                 where pr = pragma++["","",""]; ats=atts++["",""]

  pSignature        :: Parser Token Signature
  pSignature         = rebuild <$> pVarid <*> pKey_pos "::" <*> pConcept <* pKey "*" <*> pConcept
                               <*> (pProps `opt` []) <*> (pPragma `opt` [])
                               <*> (pContent `opt` []) <* pSpec '.'
                       where rebuild nm pos s t props pragma content
                              = Sgn nm s t props (pr!!0) (pr!!1) (pr!!2) content pos
                                where pr = pragma++["","",""]

  pContent         :: Parser Token Links
  pContent          = pKey "=" *> pSpec '[' *> pListSep (pKey ";") pRecord <* pSpec ']'
                      

  pProps           :: Parser Token [Prop]
  pProps            = pSpec '['  *> pListSep (pSpec ',') pProp <* pSpec ']'

  pProp            :: Parser Token Prop
  pProp             = k Uni "UNI" <|> k Inj "INJ" <|> k Sur "SUR" <|> k Tot "TOT"
                      <|> k Sym "SYM" <|> k Asy "ASY" <|> k Trn "TRN" <|> k Rfx "RFX"
                      where k obj str = f <$> pKey str where f _ = obj

  pPragma          :: Parser Token [String]
  pPragma           = pKey "PRAGMA" *> pList1 pString

  pRecord          :: Parser Token [String]
  pRecord           = pSpec '(' *> pListSep (pSpec ',') pString <* pSpec ')'
  
