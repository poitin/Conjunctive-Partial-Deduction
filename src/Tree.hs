module Tree where

import Data.Maybe
import Data.List
import Term
import Debug.Trace

data Tree = Node Term [([(String,Term)],Tree)] -- a disjunction of alternatives, with the original term at the root and the different possible substitutions along the edges
          | Split Tree Tree -- a conjunction resulting form a generaliation by splitting
          | Gen [(String,Term)] Tree -- a most specific generalisation giving the generalisation substitution
          | BackEdge Term -- a renaming of a previous conjunction
          | Leaf Bool -- failure or success
   deriving (Show)

-- construct a program from a tree

residualise = residualise' [] []
residualise' r cs (Leaf True) = ([],cs)
residualise' r cs (Leaf False) = ([Truth False],cs)
residualise' r cs (Node (Atom a ts) ebs) = let a' = renameVar (snd (unzip r) ++ map atomName (fst(unzip cs))) a
                                               cs' = foldl (\cs (e,b) -> let (ts',cs') = residualise' ((a,a'):r) cs b
                                                                         in  (instantiateTerm e (Atom a' ts),ts'):cs') cs ebs
                                           in  ([Atom a' ts],cs')
residualise' r cs (Split b1 b2) = let (ts1,cs1) = residualise' r cs b1
                                      (ts2,cs2) = residualise' r cs1 b2
                                  in  (ts1++ts2,cs2)
residualise' r cs (Gen e b) = let (ts,cs') = residualise' r cs b
                              in  (map (instantiateTerm e) ts,cs')
residualise' r cs (BackEdge t) = ([renameAtom r t],cs)

-- the conjunctive partial deduction transformation

trans ts cs = trans' ts cs []
trans' [] cs m = Leaf True
trans' ts cs m = case find (\(t,ts') -> isJust (renamingTerm (Conjunction ts') (Conjunction ts) [])) m of
                    Just (t,ts') -> BackEdge (renameTerm (fromJust (renamingTerm (Conjunction ts') (Conjunction ts) [])) t)
                    Nothing -> case find (\(t,ts') -> isJust (coupleTerm (Conjunction ts') (Conjunction ts) [])) m of
                                  Just (t,ts') -> if   length ts == length ts'
                                                  then let (Conjunction ts'',s1,s2) = generaliseTerm (Conjunction ts) (Conjunction ts')
                                                       in  Gen s1 (trans' ts'' cs m)
                                                  else let r = fromJust (coupleTerm (Conjunction ts') (Conjunction ts) [])
                                                           (ts1,ts2) = split ts ts' r
                                                       in  Split (trans' ts1 cs m) (trans' ts2 cs m)
                                  Nothing ->  case unfold ts cs of
                                                    [] -> Leaf True
                                                    [([Truth b],env)] -> Leaf b
                                                    [(ts',env)] -> trans' (map (instantiateTerm env) ts') cs m
                                                    tss -> let xs = foldr varsTerm' [] ts
                                                               x = renameVar (map atomName (fst(unzip m))) "p"
                                                               t = Atom x (map Var xs)
                                                           in  Node t [(env,trans' (map (instantiateTerm env) ts') cs ((t,ts):m)) | (ts',env) <- tss]


