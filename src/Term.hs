module Term where

import Data.Char
import Data.Maybe
import Data.Foldable
import Control.Monad
import Text.PrettyPrint
import Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.ParserCombinators.Parsec.Language
import Debug.Trace

-- programs consist of a goal conjunction and a set of clauses

type Prog = ([Term],[(Term,[Term])])

showprog = render.prettyProg

-- terms are logical variables, atoms, conjunctions or truth values

data Term = Var String
          | Atom String [Term]
          | Conjunction [Term]
          | Truth Bool
          deriving Eq

instance Show Term where
   show = render.prettyTerm

-- standard unification which includes occurs check

unify t t' env = unify' (walk env t) (walk env t') env
unify' (Var x) t env = if x `elem` varsTerm t then Nothing else Just ((x,t):env)
unify' t (Var x) env = if x `elem` varsTerm t then Nothing else Just ((x,t):env)
unify' (Atom a ts) (Atom a' ts') env | a==a' = foldlM (\env (t,t') -> unify t t' env) env (zip ts ts')
unify' t t' env = Nothing

walk env (Var x) = case lookup x env of
                      Nothing -> Var x
                      Just t -> walk env t
walk env (Atom a ts) = Atom a (map (walk env) ts)

-- checking for renamings for folding

renamingTerm (Var x) (Var x') r = if x `elem` fst (unzip r)
                                  then if (x,x') `elem` r then Just r else Nothing
                                  else Just ((x,x'):r)
renamingTerm (Atom a ts) (Atom a' ts') r | a==a' = foldrM (\(t,t') r -> renamingTerm t t' r) r (zip ts ts')
renamingTerm (Conjunction []) (Conjunction []) r = Just r
renamingTerm (Conjunction (t:ts)) (Conjunction (t':ts')) r = renamingTerm t t' r >>= renamingTerm (Conjunction ts) (Conjunction ts')
renamingTerm t t' r = Nothing

-- chacking for embeddings for generalisation

embeddingTerm t u r = mplus (coupleTerm t u r) (diveTerm t u r)

coupleTerm (Var x) (Var x') r = if   x `elem` fst (unzip r)
                                then if (x,x') `elem` r then Just r else Nothing
                                else Just ((x,x'):r)
coupleTerm (Atom a ts) (Atom a' ts') r | a==a' = foldrM (\(t,t') r -> embeddingTerm t t' r) r (zip ts ts')
coupleTerm (Conjunction []) (Conjunction []) r = Just r
coupleTerm (Conjunction (t:ts)) (Conjunction (t':ts')) r = coupleTerm t t' r >>= embeddingTerm (Conjunction ts) (Conjunction ts')
coupleTerm t t' r = Nothing

diveTerm t (Atom a ts) r = msum (map (\t' -> embeddingTerm t t' r) ts)
diveTerm t (Conjunction (t':ts')) r = embeddingTerm t (Conjunction ts') r
diveTerm t t' r = Nothing

-- generalisation is either done by splitting if more conjuncts are added, or most specific generalisation otherwise

split ts [] r = ([],ts)
split (t:ts) (t':ts') r = if   isJust $ coupleTerm t' t r
                          then let (ts1,ts2) = split ts ts' r
                               in  (t:ts1,ts2)
                          else ([],t:ts)

generaliseTerm t t' = generaliseTerm' t t' (varsTerm t) [] []

generaliseTerm' (Var x) (Var x') xs s1 s2 = (Var x,s1,s2)
generaliseTerm' (Atom a ts) (Atom a' ts') xs s1 s2 | a==a' = let (ts'',s1',s2') = foldr (\(t,t') (ts,s1,s2) -> let (t'',s1',s2') = generaliseTerm' t t' xs s1 s2
                                                                                                               in  (t'':ts,s1',s2')) ([],s1,s2) (zip ts ts')
                                                             in  (Atom a ts'',s1',s2')
generaliseTerm' (Conjunction []) (Conjunction []) xs s1 s2 = (Conjunction [],s1,s2)
generaliseTerm' (Conjunction (t:ts)) (Conjunction (t':ts')) xs s1 s2 = let (t'',s1',s2') = generaliseTerm' t t' xs s1 s2
                                                                           (Conjunction ts'',s1'',s2'') = generaliseTerm' (Conjunction ts) (Conjunction ts') xs s1' s2'
                                                                       in  (Conjunction (t'':ts''),s1'',s2'')
generaliseTerm' t t' xs s1 s2 = case find (\(x,u) -> t==u && (lookup x s2 == Just t')) s1 of
                                   Just (x,u) -> (Var x,s1,s2)
                                   Nothing -> let x = renameVar (xs++fst(unzip s1)) "X"
                                              in  (Var x,(x,t):s1,(x,t'):s2)

-- evaluate a conjunction, putting the resultants in rs
-- this is a depth-first evaluation which evaluates all resultants, so may not terminate!

eval rs [] cs = [rs]
eval rs c@(t:ts) cs = let tss = [(ts',fromJust env) | (h,ts) <- cs,let (h',ts') = renameClause (varsTerm (Conjunction c)) (h,ts),let env = unify h' t [],isJust env]
                      in  concat [eval (map (instantiateTerm env) rs) (map (instantiateTerm env) (ts'++ts)) cs | (ts',env) <- tss]

-- unfold a conjunction; allow non-determinate unfolding only for the first predicate; determinate unfolding for all remaining predicates

unfold ts cs = nondetunfold ts cs (foldr varsTerm' [] ts) []
nondetunfold [] cs vs env = [([],env)]
nondetunfold (t:ts) cs vs env = let tss = [(ts',fromJust env') | (h,ts) <- cs,let (h',ts') = renameClause (vs++varsEnv env) (h,ts),let env' = unify h' t env,isJust env']
                                in  if   null tss
                                    then [([Truth False],env)]
                                    else [(ts'++ts'',env'') | (ts',env') <- tss, let (ts'',env'') = detunfold ts cs vs env']
detunfold [] cs vs env = ([],env)
detunfold (t:ts) cs vs env = let tss = [(ts',fromJust env') | (h,ts) <- cs,let (h',ts') = renameClause (vs++varsEnv env) (h,ts),let env' = unify h' t env,isJust env']
                                 (ts',env') = if length tss == 1 then head tss else ([t],env)
                                 (ts'',env'') = detunfold ts cs vs env'
                             in (ts'++ts'',env'')

-- instantiate with an environment

instantiateTerm env (Var x) = walk env (Var x)
instantiateTerm env (Atom a ts) = Atom a (map (instantiateTerm env) ts)
instantiateTerm env (Conjunction ts) = Conjunction (map (instantiateTerm env) ts)

-- return the variables


varsTerm t = varsTerm' t []
varsTerm' (Var x) xs = if x `elem` xs then xs else x:xs
varsTerm' (Atom a ts) xs = foldr varsTerm' xs ts
varsTerm' (Conjunction ts) xs = foldr varsTerm' xs ts

varsEnv env = varsEnv' env []
varsEnv' [] xs = xs
varsEnv' ((x,t):env) xs = varsEnv' env $ x:varsTerm' t xs

-- rename variables

renameClause xs (h,ts) = let xs' = foldr varsTerm' (varsTerm h) ts
                             xs'' = renameVars xs xs'
                             r = zip xs' xs''
                         in  (renameTerm r h,map (renameTerm r) ts)

renameTerm r (Var x) = case lookup x r of
                          Nothing -> Var x
                          Just x' -> Var x'
renameTerm r (Atom a ts) = Atom a (map (renameTerm r) ts)

renameTerm r (Conjunction ts) = Conjunction (map (renameTerm r) ts)

renameVar xs x = if x `elem` xs then renameVar xs (x++"'") else x

renameVars xs vs = take (length vs) (foldr (\v xs -> let v' = renameVar xs v in v':xs) xs vs)

renameAtom r (Atom a ts) = case lookup a r of
                              Nothing -> Atom a ts
                              Just a' -> Atom a' ts

atomName (Atom a ts) = a

-- pretty printing

prettyProg (ts,cs) = vcat (map prettyClause cs) $$ text "<-" <+> prettyTerm (Conjunction ts) <> text "."

prettyClause (h,[]) = prettyTerm h <> text "."
prettyClause (h,ts) = prettyTerm h <+> text "<-" <+> prettyTerm (Conjunction ts) <> text "."



prettyTerm (Var x) = text x
prettyTerm t@(Atom a ts)
   | isNat t = int (term2nat t)
   | isList t = text "[" <> term2list t <> text "]"
   | null ts = text a
   | otherwise = text a <> parens (hcat $ punctuate comma $ map prettyTerm ts)
prettyTerm (Conjunction []) = text "true"
prettyTerm (Conjunction [t]) = prettyTerm t
prettyTerm (Conjunction (t:ts)) = prettyTerm t <> text "," <> prettyTerm (Conjunction ts)
prettyTerm (Truth b) = if b then text "true" else text "false"

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
         , reservedNames   = ["true","false","zero","succ","nil","cons"]
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
       cs <- many1 clause
       ts <- conjunction
       return (ts,cs)

conjunction = do
              symbol "<-"
              ts <- sepBy1 structure (symbol ",")
              symbol "."
              return ts

clause = do
         h <- structure
         ts <- body
         return (h,ts)

body =     conjunction
       <|> do
           symbol "."
           return []

structure = do
            a <- atom
            ts <- args
            return $ Atom a ts

term =     do
           v <- identifier
           return $ Var v
       <|> structure
       <|> do
           n <- natural
           return $ nat2term n
       <|> do
           symbol "["
           l <- list
           symbol "]"
           return l
       <|> do
           reserved "true"
           return $ Truth True
       <|> do
           reserved "false"
           return $ Truth False

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


