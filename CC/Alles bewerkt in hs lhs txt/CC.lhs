> module CC where
>  import UU_Scanner
>  import UU_Parsing
>  import Auxiliaries
>  import Classification
>  import Typology
>  import CC_aux
>  import AGtry

>  keywordstxt      = [ "CONTEXT", "ENDCONTEXT", "EXTENDS"
>                     , "PATTERN", "ENDPATTERN"
>                     , "POPULATION", "ENDPOPULATION"
>                     , "UNI", "INJ", "SUR", "TOT", "SYM", "ASY", "TRN", "RFX"
>                     , "RELATION", "CONCEPT"
>                     , "IMPORT", "GLUE", "GEN", "ISA", "I"
>                     , "PRAGMA", "EXPLANATION"
>                     ]
>  keywordsops      = [ "-:", "=", "~", "-", ";", "*", "::", "\\/", "/\\" ]
>  specialchars     = "()[].,"
>  opchars          = "-~:=;*\\/"

>  data ContextElement = CPat Spec
>                      | CMor Morphism
>                      | CCon ConceptDef
>  data PatElem        = Pr Rule
>                      | Pg Gen
>                      | Pm Morphism
>                      | Pc ConceptDef

>  anything = C "" []

>-- Top level expression, with all expression related parsers (implicitly)
>-- parameterized with operator expression constructor

>  pArchitecture    :: Parser Token Architecture
>  pArchitecture     = Arch <$> pList1 pContext

>  pContext         :: Parser Token Context
>  pContext          = rebuild <$ pKey "CONTEXT" <*> pConid <*>
>                                 ((pKey "EXTENDS" *> pList1Sep (pSpec ',') pConid) `opt` []) <*>
>                                 pList pContextElement <* pKey "ENDCONTEXT"
>                      where rebuild nm on ces
>                             = Ctx nm on empty nogE [] [p| CPat p<-ces] [m| CMor m<-ces] [c| CCon c<-ces]

>  pContextElement  :: Parser Token ContextElement
>  pContextElement   = CPat <$> pPattern    <|>
>                      CMor <$> pMorphism   <|>
>                      CCon <$> pConceptDef

>  pPattern         :: Parser Token Spec
>  pPattern          = rebuild <$ pKey "PATTERN" <*> (pConid <|> pString)
>                              <*> pList pPatElem
>                              <* pKey "ENDPATTERN"
>                      where rebuild nm pes = Pat nm [r|Pr r<-pes] [gen |Pg gen<-pes] [m| Pm m<-pes] [c| Pc c<-pes]

>  pPatElem         :: Parser Token PatElem
>  pPatElem          = Pr <$> pRule       <|>
>                      Pg <$> pGen        <|>
>                      Pm <$> pMorphism   <|>
>                      Pc <$> pConceptDef

>  pRule            :: Parser Token Rule
>  pRule             = hc <$> pExpr   <*> pKey_pos "-:" <*> pExpr <*> (pKey "EXPLANATION" *> pString `opt` "") <|>
>                      dc <$> pMorph  <*> pKey_pos "="  <*> pExpr <*> (pKey "EXPLANATION" *> pString `opt` "") <|>
>                      gc <$> pKey_pos "GLUE" <*> pMorph <* pKey "=" <*> pExpr
>                      where
>                       hc antc pos cons expl = Hc antc pos cons expl (anything,anything) 0
>                       dc defd pos expr expl = Dc defd pos expr expl (anything,anything) 0
>                       gc pos pm pf = Gc pos pm pf (anything,anything) 0

>  pGen             :: Parser Token Gen
>  pGen              = rebuild <$ pKey "GEN" <*> (pConid <|> pString) <* pKey "ISA" <*> (pConid <|> pString)
>                      where rebuild spec genus = G (C genus []) (C spec [])

>  pExpression      :: Parser Token [Term]
>  pExpression       = pList1Sep (pKey ";") pTerm

>  pTerm            :: Parser Token Term
>  pTerm             = tm <$> pMorph <*> (pKey "~" `opt` "")                                 <|>
>                      tc <$> (pSpec '(' *> pExpr <* pSpec ')') <*> (pKey "~" `opt` "")
>                      where
>                       tm pm ""  = Tm pm (anything,anything)
>                       tm pf "~" = Tf pf (anything,anything)
>                       tc pc ""  = Tc pc (anything,anything)
>                       tc pc "~" = Tc (flp pc) (anything,anything)
>                                   where flp (F  ts sgn) = F  (reverse (map f1p ts)) sgn
>                                         flp (Fi fs sgn) = Fi (map flp fs)           sgn
>                                         flp (Fu fs sgn) = Fu (map flp fs)           sgn
>                                         f1p (Tm m sgn)  = Tf m                      sgn
>                                         f1p (Tf m sgn)  = Tm m                      sgn
>                                         f1p (Tc f sgn)  = Tc (flp f)                sgn

>-- There are always two or more factors in a factorU.

>  pExpr            :: Parser Token Factor
>  pExpr             = f <$> pList1Sep (pKey "\\/") pFactorI
>                      where f [x] = x
>                            f  xs = Fu xs (anything,anything)

>-- There are always two or more factors in a factorU.

>  pFactorI         :: Parser Token Factor
>  pFactorI          = f <$> pList1Sep (pKey "/\\") pFactor
>                      where f [x] = x
>                            f  xs = Fi xs (anything,anything)

>-- There are always one or more terms in a factor. F [] _ cannot occur

>  pFactor          :: Parser Token Factor
>  pFactor           = f <$> pList1Sep (pKey ";") pTerm
>                      where f [Tc f' _] = f'
>                            f ts        = F ts (anything,anything)

>  pMorph           :: Parser Token Morph
>  pMorph            = rebuild <$> pVarid_val_pos <*> ((pSpec '[' *> pListSep (pKey "*") pConcept <* pSpec ']') `opt` []) <|>
>                      iden <$> pKey_pos "I" <*> ((pSpec '[' *> pConcept <* pSpec ']') `opt` anything)
>                      where rebuild (nm,pos) atts = Mph nm pos atts (anything,anything)
>                                                     (Mor nm anything anything [] "" "" "" [] posNone)
>                            iden pos (C "" _)  = Id pos []         anything
>                            iden pos (C str _) = Id pos [C str []] anything

>  pConcept         :: Parser Token Concept
>  pConcept          = c <$> (pConid <|> pString)
>                      where c str = C str []

>  pConceptDef      :: Parser Token ConceptDef
>  pConceptDef       = Cd <$> pKey_pos "CONCEPT" <*> (pConid <|> pString) <*> pString <*> pString

>--  pMorphism        :: Parser Token Morphism
>--  pMorphism         = rebuild <$> pVarid <*> pKey_pos "::" <*> pList1Sep (pKey "*") (pConid <|> pString) 
>--                              <*> (pProps `opt` []) <*> (pPragma `opt` [])
>--                              <*> (pContent `opt` []) <* pSpec '.'
>--                      where rebuild nm pos atts props pragma content
>--                             = Mor nm (C (ats!!0) []) (C (ats!!1) []) props (pr!!0) (pr!!1) (pr!!2) content pos
>--                               where pr = pragma++["","",""]; ats=atts++["",""]

>  pMorphism        :: Parser Token Morphism
>  pMorphism         = rebuild <$> pVarid <*> pKey_pos "::" <*> pConcept <* pKey "*" <*> pConcept
>                              <*> (pProps `opt` []) <*> (pPragma `opt` [])
>                              <*> (pContent `opt` []) <* pSpec '.'
>                      where rebuild nm pos s t props pragma content
>                             = Mor nm s t props (pr!!0) (pr!!1) (pr!!2) content pos
>                               where pr = pragma++["","",""]

>  pContent         :: Parser Token Drecords
>  pContent          = pKey "=" *> pSpec '[' *> pListSep (pKey ";") pRecord <* pSpec ']'
>                      

>  pProps           :: Parser Token [Prop]
>  pProps            = pSpec '['  *> pListSep (pSpec ',') pProp <* pSpec ']'

>  pProp            :: Parser Token Prop
>  pProp             = k Uni "UNI" <|> k Inj "INJ" <|> k Sur "SUR" <|> k Tot "TOT"
>                      <|> k Sym "SYM" <|> k Asy "ASY" <|> k Trn "TRN" <|> k Rfx "RFX"
>                      where k obj str = f <$> pKey str where f _ = obj

>  pPragma          :: Parser Token [String]
>  pPragma           = pKey "PRAGMA" *> pList1 pString

>  pRecord          :: Parser Token [String]
>  pRecord           = pSpec '(' *> pListSep (pSpec ',') pString <* pSpec ')'