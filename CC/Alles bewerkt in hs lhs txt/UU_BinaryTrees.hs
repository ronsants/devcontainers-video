Fast, Error Correcting Parser Combinators; Version 2.0.1, April 22, 2001
 - Copyright:  S. Doaitse Swierstra
               Department of Computer Science
               Utrecht University
               P.O. Box 80.089
               3508 TB UTRECHT  
               the Netherlands
               swierstra@cs.uu.nl

-- ===== BINARY SEARCH TREES =============================================================
-- =======================================================================================
  ( BinSearchTree(..)
  , tab2tree
  , btFind
  , btLocateIn
  )
  where
   = Node (BinSearchTree av) av (BinSearchTree av)
   | Nil

  tab2tree tab = tree
   where
    (tree,[]) = sl2bst (length tab) (tab)
    sl2bst 0 list     = (Nil   , list)
    sl2bst n list     
     = let 
        ll = (n - 1) `div` 2 ; rl = n - 1 - ll
        (lt,a:list1) = sl2bst ll list 
        (rt,  list2) = sl2bst rl list1
       in (Node lt a rt, list2)
  btLocateIn :: (a -> b -> Ordering) -> BinSearchTree a      -> b -> Maybe a
  btFind     = btLookup fst snd
  btLocateIn = btLookup id id
  btLookup  key val cmp (Node Nil  kv Nil)
    =  let comp = cmp (key kv)
           r    = val kv
       in \i -> case comp i of
                LT -> Nothing    
                EQ -> Just r
                GT -> Nothing 
    =  let comp = cmp (key kv)
           findleft = btLookup key val cmp left
           r    = val kv
       in \i -> case comp i of
                LT -> Nothing    
                EQ -> Just r
                GT -> findleft i 
    =  let comp      = cmp (key kv)
           findright = btLookup key val cmp right
           r         = val kv
           in \i -> case comp i of
                    LT -> findright i   
                    EQ -> Just r
                    GT -> Nothing 
    =  let comp = cmp (key kv)
           findleft  = btLookup key val cmp left
           findright = btLookup key val cmp right
           r    = val kv
       in \i -> case comp i of
                LT -> findright i    
                EQ -> Just r
                GT -> findleft i
