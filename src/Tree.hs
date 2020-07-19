module Tree where

import Data.Maybe
import Data.List
import Term
import Exception

data Tree = Node Term Tree -- a memoised node
          | Or [([(String,Term)],Tree)] -- a disjunction of alternatives, with the original term at the root and the different possible substitutions along the edges
          | And [Tree] -- a conjunction
          | Gen [(String,Term)] Tree -- a most specific generalisation giving the generalisation substitution
          | BackEdge Term -- an instance of a previous conjunction
          | Leaf Bool -- failure or success
   deriving (Show)

-- construct a program from a tree

residualise b = residualise' b [] []

residualise'(Leaf b) m d = (Truth b,d)
residualise' (Node t b) m d = let a = renameVar (map atomName (fst(unzip (d++m)))) "p"
                                  h = Atom a (map Var (vars t))
                                  d' = residualiseClause h b ((h,t):m) d
                              in  (h,d')
residualise' (And bs) m d = let (d',ts) = mapAccumL (\d b -> let (t,d') = residualise' b m d in  (d',t)) d bs
                            in  (makeConjunction ts,d')
residualise' (Gen s b) m d = let (t,d') = residualise' b m d
                             in  (instantiate s t,d')
residualise' (BackEdge t) m d = case find (\(h,t') -> isInst t' t) m of
                                   Just (h,t') -> let s = fromJust (inst t' t [])
                                                  in  (instantiate s h,d)
                                   Nothing -> (t,d)

residualiseClause h (Node t b) m d = case find (isInst t) (backedges b) of
                                        Nothing -> residualiseClause h b m d
                                        Just t' -> let a = renameVar (map atomName (fst(unzip (d++m)))) "p"
                                                       h' = Atom a (map Var (vars t))
                                                       d' = residualiseClause h' b ((h',t):m) d
                                                   in  (h,h'):d'
residualiseClause h (Or ebs) m d = foldl (\d (e,b) -> residualiseClause (walk e h) b m d) d ebs
residualiseClause h b m d = let (t,d') = residualise' b m d
                            in  (h,t):d'

-- backedges in a tree

backedges (Leaf _) = []
backedges (Node t b) = backedges b
backedges (Or ebs) = concat [backedges b | (e,b) <- ebs]
backedges (Gen _ b) = backedges b
backedges (BackEdge t) = [t]
backedges (And bs) = concat [backedges b | b <- bs]

-- the conjunctive partial deduction transformation

trans (t,d) = let t' = returnval (trans' t [] d [] (vars t))
              in  residualise t'

trans' (Truth b) m d e xs = return (Leaf b)
trans' t m d e xs = let t' = walk e t
                    in  case find (`isInst` t') m of
                           Just _ -> return (BackEdge t')
                           Nothing -> case find (`couple` t') m of
                                         Just t'' -> throw (t'',t')
                                         Nothing ->  let handler (t,t'') = if   t==t'
                                                                           then let (u,s1,s2) = generalise t' t''
                                                                                    ts = split u s1
                                                                                in  do
                                                                                    bs <- mapM (\t -> trans' t m d e (xs++fst(unzip s1))) ts
                                                                                    return (Gen s1 (And bs))
                                                                           else throw (t,t'')
                                            in  handle (fold t' m d (unfold t d e xs)) handler

fold t m d texs = do
                  bs <- mapM (\(t',e,xs) -> trans' t' (t:m) d e xs) texs
                  return (Node t (Or [(e,b) | ((t,e,xs),b) <- zip texs bs]))

