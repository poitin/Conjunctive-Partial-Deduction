module Term where

import Prelude hiding ((<>))
import Exception
import Data.Char
import Data.Maybe
import Data.List
import Data.Foldable
import Control.Monad
import Text.PrettyPrint
import Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.ParserCombinators.Parsec.Language

-- programs consist of a goal conjunction and a set of clauses

type Prog = (Term,[(Term,Term)])

showprog = render.prettyProg

-- terms are logical variables, atoms, conjunctions or truth values

data Term = Var String
          | Atom String [Term]
          | Conjunction [Term]
          deriving Eq

instance Show Term where
   show = render.prettyTerm

-- standard unification which includes occurs check

unify t t' e = unify' (walk e t) (walk e t') e

unify' (Var x) t e = if x `elem` vars t then Nothing else Just ((x,t):e)
unify' t (Var x) e = if x `elem` vars t then Nothing else Just ((x,t):e)
unify' (Atom a ts) (Atom a' ts') e | a==a' = foldlM (\e (t,t') -> unify t t' e) e (zip ts ts')
unify' (Conjunction ts) (Conjunction ts') e | length ts == length ts' = foldlM (\e (t,t') -> unify t t' e) e (zip ts ts')
unify' t t' e = Nothing

walk e (Var x) = case lookup x e of
                    Nothing -> Var x
                    Just t -> walk e t
walk e (Atom a ts) = Atom a (map (walk e) ts)
walk e (Conjunction ts) = Conjunction (map (walk e) ts)

-- checking for instance for folding

isInst t t' = isJust (inst t t' [])

inst (Var x) t s = if   x `elem` fst (unzip s)
                   then if (x,t) `elem` s then Just s else Nothing
                   else Just ((x,t):s)
inst (Atom a ts) (Atom a' ts') s | a==a' && length ts==length ts' = foldrM (\(t,t') s -> inst t t' s) s (zip ts ts')
inst (Conjunction ts) (Conjunction ts') s | length ts==length ts' = foldrM (\(t,t') s -> inst t t' s) s (zip ts ts')
inst t t' s = Nothing

-- chacking for embedding for generalisation

embedding (t,u) = couple t u || dive t u

couple (Var x) (Var x') = True
couple (Atom a ts) (Atom a' ts') | a==a' && length ts==length ts' = all embedding (zip ts ts')
couple (Conjunction ts) (Conjunction ts') | length ts==length ts' = all embedding (zip ts ts')
couple t t' = False

dive t (Atom a ts) = any (\t' -> embedding (t,t')) ts
dive t (Conjunction ts) = any (\t' -> embedding (t,t')) ts
dive t t' = False

-- most specific generalisation

generalise t t' = generalise' t t' (vars t) [] []

generalise' (Var x) (Var x') xs s1 s2 | x==x' = (Var x,s1,s2)
generalise' (Atom a ts) (Atom a' ts') xs s1 s2 | a==a' = let (ts'',s1',s2') = foldr (\(t,t') (ts,s1,s2) -> let (t'',s1',s2') = generalise' t t' xs s1 s2
                                                                                                           in  (t'':ts,s1',s2')) ([],s1,s2) (zip ts ts')
                                                         in  (Atom a ts'',s1',s2')
generalise' (Conjunction ts) (Conjunction ts') xs s1 s2 = let ((s1',s2'),ts'') = mapAccumL (\(s1,s2) (t,t') -> let (t'',s1',s2') = generalise' t t' xs s1 s2
                                                                                                               in  ((s1',s2'),t'')) (s1,s2) (zip ts ts')
                                                          in  (Conjunction ts'',s1',s2')
generalise' t t' xs s1 s2 = case find (\(x,u) -> t==u && (lookup x s2 == Just t')) s1 of
                               Just (x,u) -> (Var x,s1,s2)
                               Nothing -> let x = renameVar (xs++fst(unzip s1)) "X"
                                          in  (Var x,(x,t):s1,(x,t'):s2)

-- split conjunction resulting from generalisation

split (Conjunction ts) s = split' (flatten ts) s
split t s = [t]

split' [] s = []
split' (Var x:ts) s = instantiate s (Var x):split' ts s
split' ts s = let (ts1,ts2) = break isVar ts
              in  Conjunction ts1:split' ts2 s

flatten [] = []
flatten (Conjunction ts:ts') = flatten (ts++ts')
flatten (t:ts) = t:flatten ts

isVar (Var x) = True
isVar t = False

-- instantiate with a substitution

instantiate s (Var x) = fromMaybe (Var x) (lookup x s)
instantiate s (Atom a ts) = Atom a (map (instantiate s) ts)
instantiate s (Conjunction ts) = Conjunction (map (instantiate s) ts)

-- evaluate a conjunction: this is a depth-first evaluation which evaluates all resultants, so may not terminate!

eval t d = let xs = vars t
           in  [map (walk e.Var) xs | e <- eval' t d [] xs]

eval' (Conjunction []) d e xs = [e]
eval' t d e xs = concat [eval' t' d e' xs' | (t',e',xs') <- unfold t d e xs]

matches t d e xs = [(h,t') | (h,t') <- map (renameClause xs) d, isJust (unify h t e)]

-- unfold a term

unfold t d e xs = concat [returnval (furtherUnfold t' [] d e' xs') | (t',e',xs') <- leftUnfold t d e xs]

leftUnfold (Conjunction []) d e xs = [(Conjunction [],e,xs)]
leftUnfold (t@(Atom _ _)) d e xs = [(t',fromJust (unify h t e),xs++vars h++vars t') | (h,t') <- matches t d e xs]
leftUnfold (Conjunction (t:ts)) d e xs = [(Conjunction (simplify (t':ts)),e',xs') | (t',e',xs') <- leftUnfold t d e xs]

-- locally unfold until global choice point

furtherUnfold (t@(Atom _ _)) m d e xs = let t' = walk e t
                                        in  case find (`couple` t') m of
                                               Just t'' -> throw t''
                                               Nothing ->  let handler t'' = if   t'==t''
                                                                             then return [(t,e,xs)]
                                                                             else throw t''
                                                           in  handle (do
                                                                       texs <- mapM (\(t,e,xs) -> furtherUnfold t (t':m) d e xs) (leftUnfold t d e xs)
                                                                       return (concat texs)
                                                                      ) handler
furtherUnfold (Conjunction ts) m d e xs = do
                                          tsexs <- furtherUnfold' ts m d e xs
                                          return [(Conjunction (simplify ts'),e',xs') | (ts',e',xs') <- tsexs]

furtherUnfold' [] m d e xs = return [([],e,xs)]
furtherUnfold' (t:ts) m d e xs = do
                                 texs <- furtherUnfold t m d e xs
                                 tsexss <- mapM (\(t',e',xs') -> furtherUnfold' ts m d e' xs') texs
                                 return [(t':ts',e'',xs'') | ((t',e',xs'),tsexs) <- zip texs tsexss, (ts',e'',xs'') <- tsexs]

-- variables in a term

vars t = vars' t []

vars' (Var x) xs = if x `elem` xs then xs else x:xs
vars' (Atom a ts) xs = foldr vars' xs ts
vars' (Conjunction ts) xs = foldr vars' xs ts

-- rename variables

renameClause xs (h,t) = let xs' = vars' t (vars h)
                            xs'' = renameVars xs xs'
                            r = zip xs' xs''
                        in  (renameTerm r h,renameTerm r t)

renameTerm r (Var x) = case lookup x r of
                          Nothing -> Var x
                          Just x' -> Var x'
renameTerm r (Atom a ts) = Atom a (map (renameTerm r) ts)

renameTerm r (Conjunction ts) = Conjunction (map (renameTerm r) ts)

renameVar xs x = if x `elem` xs then renameVar xs (x++"'") else x

renameVars xs xs' = take (length xs') (foldr (\x xs -> let x' = renameVar xs x in x':xs) xs xs')

renameAtom r (Atom a ts) = case lookup a r of
                              Nothing -> Atom a ts
                              Just a' -> Atom a' ts

atomName (Atom a ts) = a

-- simplify a conjunction to remove empty conjuncts

simplify [] = []
simplify (Conjunction []:ts) = simplify ts
simplify (t:ts) = t:simplify ts

-- pretty printing

prettyProg (t,d) = vcat (map prettyClause d) $$ text "<-" <+> prettyTerm t <> text "."

prettyClause (h,Conjunction []) = prettyTerm h <> text "."
prettyClause (h,t) = prettyTerm h <+> text "<-" <+> prettyTerm t <> text "."

prettyTerm (Var x) = text x
prettyTerm t@(Atom a ts)
   | isNat t = int (term2nat t)
   | isList t = text "[" <> term2list t <> text "]"
   | null ts = text a
   | otherwise = text a <> parens (hcat $ punctuate comma $ map prettyTerm ts)
prettyTerm (Conjunction ts) = hcat (punctuate comma (map prettyTerm ts))

isList (Atom "nil" []) = True
isList (Atom "cons" [h,t]) = True
isList _ = False

term2list (Atom "nil" [])  = text ""
term2list (Atom "cons" [h,Var x]) = prettyTerm h <> text "|" <> text x
term2list (Atom "cons" [h,Atom "nil" []]) = prettyTerm h
term2list (Atom "cons" [h,t]) = prettyTerm h <> text "," <> term2list t

isNat (Atom "zero" []) = True
isNat (Atom "succ" [n]) = isNat n
isNat _ = False

nat2term 0 = Atom "zero" []
nat2term n = Atom "succ" [nat2term (n-1)]

term2nat (Atom "zero" [])  = 0
term2nat (Atom "succ" [n]) = 1+term2nat n

-- lexing and parsing

potDef = emptyDef
         { commentStart    = "/*"
         , commentEnd      = "*/"
         , commentLine     = "--"
         , nestedComments  = True
         , identStart      = upper <|> P.char '_'
         , identLetter     = letter <|> digit <|> oneOf "_'"
         , reservedNames   = ["zero","succ","nil","cons"]
         , caseSensitive   = True
         }

lexer = T.makeTokenParser potDef

symbol     = T.symbol lexer
bracks     = T.parens lexer
comm       = T.comma lexer
identifier = T.identifier lexer
reserved   = T.reserved lexer
natural    = T.natural lexer

prog = do
       d <- many1 clause
       t <- body
       return (t,d)

clause = do
         h <- structure
         t <- body
         return (h,t)

body = do
       symbol "<-"
       ts <- sepBy1 structure (symbol ",")
       symbol "."
       return $ Conjunction ts
   <|> do
       symbol "."
       return $ Conjunction []

structure = do
            a <- atom
            ts <- args
            return $ Atom a ts

term =     do
           x <- identifier
           return $ Var x
       <|> structure
       <|> do
           n <- natural
           return $ nat2term n
       <|> do
           symbol "["
           l <- list
           symbol "]"
           return l

args =     do
           symbol "("
           ts <- sepBy1 term (symbol ",")
           symbol ")"
           return ts
       <|> do
           spaces
           return []

list =     do
           h <- term
           t <- tl
           return $ Atom "cons" [h,t]
       <|> do
           spaces
           return $ Atom "nil" []
tl =     do
         symbol ","
         list
     <|> do
         symbol "|"
         term
     <|> do
         spaces
         return $ Atom "nil" []

atom = do
       c <- lower
       cs <- many (letter <|> digit <|> oneOf "_'")
       return (c:cs)

parseTerm = parse term "(ERROR)"

parseProg = parse prog "(ERROR)"


