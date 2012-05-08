{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}

module Hee.SystemFw
  ( Term(..)
  , Type(..)
  , Kind(..)
  , EUnify(..)
  , CanSubstitute
  , CanUnify
  , bool
  , tru
  , fls
  , nat
  , zero
  , succ
  , pair
  , fst
  , snd
  , sum
  , inl
  , inr
  , list
  , null
  , cons
  , unbindvar
  , normalize
  , parseKind
  , parseType
  , parseTerm
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Attoparsec.Text

import Data.List (union, (\\), foldl', elemIndex)
import Control.Applicative ((<|>), (<*), (*>), (<$>))
import Prelude hiding (succ, fst, snd, sum, null, takeWhile)

trace n msg = id

type Id = Int

data Term
  = TmVariable Id                 -- x,y         term variable
  | TmApplication Term Term       -- f e         term application
  | TmAbstraction Id Type Term    -- λx:τ. e     term abstraction
  | TmUAbstraction Id Kind Term   -- Λα:κ. e     universal abstraction
  | TmUApplication Term Type      -- f τ         universal application
  deriving (Eq, Show)

data Type
  = TVariable Id Kind             -- α,β         type variable
  | TApplication Type Type        -- τ υ         type application
  | TAbstraction Id Kind Type     -- λα:κ. τ     type abstraction
  | TQuantification Id Kind Type  -- ∀α:κ. τ     type quantification
  | TOperator Type Type           -- τ → υ       type of operator on terms
  deriving (Eq, Show)

data Kind
  = KType                         -- ★           kind of manifest type
  | KOperator Kind Kind           -- κ → ι       kind of operator on types
  deriving (Eq, Show)

---------------------------------------------------------------------------

class HasKind a where
  kind :: a -> Kind

instance HasKind Type where
  kind (TVariable _ k)         = k
  kind (TOperator t u)         = KType
  kind (TQuantification _ _ t) = kind t
  kind (TAbstraction _ k t)    = KOperator k (kind t)
  kind (TApplication t _)      = let (KOperator k l) = kind t in l

instance HasKind Kind where
  kind = id

---------------------------------------------------------------------------

-- * Can re-order directly nested quantifiers
--     (∀α:★. (∀β:★. τ)) ≡ (∀β:★. (∀α:★. τ))
--
-- * Can't re-order indirectly nested quantifiers
--     (∀α:★. υ (∀β:★. τ)) ≢  (∀β:★. υ (∀α:★. τ))
--
-- * Can lift quantifiers on the right of an arrow
--     (∀α:★. τ → (∀β:★. υ)) ≡ (∀α:★. (∀β:★. τ → υ))
--
-- * Can't lift quantifiers on the left of an arrow
--     (∀α:★. (∀β:★. τ) → τ) ≢ (∀α:★. (∀β:★. τ → τ))

normalize (TVariable a k)         = TVariable a k
normalize (TApplication t u)      = TApplication (normalize t) (normalize u)
normalize (TAbstraction a k t)    = TAbstraction a k (normalize t)
normalize (TOperator t u)         =
  case normalize u of
    (TQuantification b k u') -> normalize (TQuantification b k (TOperator (normalize t) u'))
    u'                       -> TOperator (normalize t) u'
normalize (TQuantification a k t) =
  case normalize t of
    (TQuantification b k' t') ->
      if b < a
      then normalize (TQuantification b k' (TQuantification a k  t'))
      else TQuantification a k  (TQuantification b k' t')
    t' -> TQuantification a k t'

---------------------------------------------------------------------------

type Variable       = (Id, Kind)
type Substitution a = [(Variable, a)]

unbindvar :: Variable -> Substitution a -> Substitution a
unbindvar v s = filter (\(w, _) -> w /= v) s

class CanSubstitute t where
  substitute :: Substitution t -> t -> t
  freevars   :: t -> [Variable]

instance CanSubstitute Type where
  substitute s (TOperator t u)          = TOperator (substitute s t) (substitute s u)
  substitute s (TQuantification a k t)  = TQuantification a k (substitute (unbindvar (a, k) s) t)
  substitute s (TAbstraction a k t)     = TAbstraction a k (substitute (unbindvar (a, k) s) t)
  substitute s (TApplication t u)       = TApplication (substitute s t) (substitute s u)
  substitute s (TVariable a k)          = case lookup (a, k) s of
                                            Just t  -> t
                                            Nothing -> TVariable a k

  freevars (TVariable a k)         = [(a, k)]
  freevars (TOperator t u)         = freevars t `union` freevars u
  freevars (TQuantification a k t) = freevars t \\ [(a, k)]
  freevars (TAbstraction a k t)    = freevars t \\ [(a, k)]
  freevars (TApplication t u)      = freevars t `union` freevars u

---------------------------------------------------------------------------

--instance Monad (Either a) where
--  return x        = Right x
--  (Left x) >>= f  = Left x
--  (Right x) >>= f = f x

data EUnify t
  = EOccursCheck t t
  | EKindMismatch t t
  | ETypeMismatch t t
  deriving (Eq, Show)

class CanUnify t where
  unify       :: t -> t -> Either (EUnify t) (Substitution t)
  instantiate :: t -> t -> Either (EUnify t) (Substitution t)
  bindvar     :: Variable -> t -> Either (EUnify t) (Substitution t)

instance CanUnify Type where
  unify (TVariable a k) t = bindvar (a, k) t
  unify t (TVariable a k) = bindvar (a, k) t
  unify (TOperator t u) (TOperator t' u') = undefined
  unify (TApplication t u) (TApplication t' u') = undefined
  unify (TAbstraction a k t) (TAbstraction b k' t') = undefined
  unify (TQuantification a k t) (TQuantification b k' t') = undefined
  unify t u = Left (ETypeMismatch t u)

  instantiate (TVariable a k) t = bindvar (a, k) t
  instantiate (TOperator t u) (TOperator t' u')
    | kind t /= kind t' = Left (EKindMismatch t t')
    | kind u /= kind u' = Left (EKindMismatch u u')
    | otherwise         = instantiate t t' >>= \s -> instantiate (substitute s u) (substitute s u')
  instantiate (TApplication t u) (TApplication t' u')
    | kind t /= kind t' = Left (EKindMismatch t t')
    | kind u /= kind u' = Left (EKindMismatch u u')
    | otherwise         = instantiate t t' >>= \s -> instantiate (substitute s u) (substitute s u')
  instantiate u@(TAbstraction a k t) u'@(TAbstraction b k' t')
    | kind k /= kind k' = Left (EKindMismatch u u')
    | otherwise         = instantiate t t'
  instantiate u@(TQuantification a k t) u'@(TQuantification b k' t')
    | kind k /= kind k' = Left (EKindMismatch u u')
    | otherwise         = undefined
  instantiate t u = Left (ETypeMismatch t u)

  bindvar v@(a, k) t
    | v `elem` freevars t = Left (EOccursCheck (TVariable a k) t)
    | k /= kind t         = Left (EKindMismatch (TVariable a k) t)
    | t == TVariable a k  = return []
    | otherwise           = return [(v, t)]

---------------------------------------------------------------------------

showTmId x = showId x "abcdefghijklmnopqrstuvwxyz"
showTId a  = showId a "αβγδεζηθικλμνξοπρςστυφχψω"

showId id alphabet = (alphabet !! n) : (replicate k '\'')
  where k = id `div` length alphabet
        n = id `mod` length alphabet

showTerm 0 (TmVariable x)         = showTmId x
showTerm 0 (TmUApplication e t)   = showTerm 1 e ++ " " ++ showType 1 t
showTerm 0 (TmApplication e f)    = showTerm 1 e ++ " " ++ showTerm 1 f
showTerm 0 (TmUAbstraction a k e) = "Λ" ++ showTId a  ++ ":" ++ showKind 0 k ++ ". " ++ showTerm 0 e
showTerm 0 (TmAbstraction x t e)  = "λ" ++ showTmId x ++ ":" ++ showType 1 t ++ ". " ++ showTerm 0 e

showTerm n (TmVariable x)         = showTmId x
showTerm n (TmUApplication e t)   = "(" ++ showTerm (n+1) e ++ " " ++ showType (n+1) t ++ ")"
showTerm n (TmApplication e f)    = "(" ++ showTerm (n+1) e ++ " " ++ showTerm (n+1) f ++ ")"
showTerm n (TmUAbstraction a k e) = "(Λ" ++ showTId a  ++ ":" ++ showKind (n+1) k ++ ". " ++ showTerm 0 e ++ ")"
showTerm n (TmAbstraction x t e)  = "(λ" ++ showTmId x ++ ":" ++ showType (n+1) t ++ ". " ++ showTerm 0 e ++ ")"

showType 0 (TVariable a _)          = showTId a
showType 0 (TOperator t u)          = showType 1 t ++ " → " ++ showType 1 u
showType 0 (TApplication a b)       = showType 1 a ++ " "   ++ showType 1 b
showType 0 (TAbstraction a k t)     = "λ" ++ showTId a ++ ":" ++ showKind 0 k ++ ". " ++ showType 0 t
showType 0 (TQuantification a k t)  = "∀" ++ showTId a ++ ":" ++ showKind 0 k ++ ". " ++ showType 0 t

showType n (TVariable a _)          = showTId a
showType n (TOperator t u)          = "(" ++ showType (n+1) t ++ " → " ++ showType (n+1) u ++ ")"
showType n (TApplication a b)       = "(" ++ showType (n+1) a ++ " "   ++ showType (n+1) b ++ ")"
showType n (TAbstraction a k t)     = "(λ" ++ showTId a ++ ":" ++ showKind (n+1) k ++ ". " ++ showType 0 t ++ ")"
showType n (TQuantification a k t)  = "(∀" ++ showTId a ++ ":" ++ showKind (n+1) k ++ ". " ++ showType 0 t ++ ")"

showKind 0 (KType)          = "★"
showKind 0 (KOperator k l)  = showKind 1 k ++ " → " ++ showKind 1 l

showKind n (KType)          = "★"
showKind n (KOperator k l)  = "(" ++ showKind (n+1) k ++ " → " ++ showKind (n+1) l ++ ")"

--instance Show Term where
--  show = showTerm 0
--
--instance Show Type where
--  show = showType 0
--
--instance Show Kind where
--  show = showKind 0

---------------------------------------------------------------------------

discard :: Parser a -> Parser ()
discard p = do p; return ()

parseParen :: Parser a -> Parser a
parseParen inner =
  do char '('
     e <- inner
     skipSpace
     char ')'
     return e

parseArrow :: Int -> Parser ()
parseArrow  n = trace n "parseArrow" $ discard (string "→" <|> string "->")

parseForall :: Int -> Parser ()
parseForall n = trace n "parseForall" $ discard (string "∀" <|> string "forall ")

parseLambda :: Int -> Parser ()
parseLambda n = trace n "parseLambda" $ discard (string "λ" <|> string ",\\" <|> string "lambda ")

parseLAMBDA :: Int -> Parser ()
parseLAMBDA n = trace n "parseLAMBDA" $ discard (string "Λ" <|> string "/\\" <|> string "LAMBDA ")

parseKind :: Int -> Parser Kind
parseKind n =
  do skipSpace
     (    parseKOperator (n+1)
      <|> parseParen (parseKind (n+1))
      <|> parseKType (n+1))
  where
    parseKType :: Int -> Parser Kind
    parseKType n = (char '★' <|> char '*') *> return KType

    parseKOperator :: Int -> Parser Kind
    parseKOperator n =
      do k <- (parseParen (parseKind (n+1))) <|> parseKType (n+1)
         _ <- skipSpace
         _ <- parseArrow (n+1)
         l <- parseKind (n+1)
         return $ KOperator k l

type TVariableScope = [Variable]

extendScope :: TVariableScope -> Variable -> TVariableScope
extendScope vs v = v:vs

parseTBinding :: Int -> TVariableScope -> Parser Id
parseTBinding n s =
  do a <- satisfy (inClass alphabet)
     n <- T.length `fmap` takeWhile (== '\'')
     let (Just m) = elemIndex a alphabet
     return $ m + n * length alphabet
  where
    alphabet = "αβγδεζηθικλμνξοπρςστυφχψω"

parseType :: Int -> TVariableScope -> Parser Type
parseType n s =
  do skipSpace
     (    (trace n "parseType.parseParen" $ parseParen (parseType (n+1) s))
      <|> (trace n "parseType.parseTQuantification" $ parseTQuantification (n+1) s)
      <|> (trace n "parseType.parseTApplication" $ parseTApplication (n+1) s)
      <|> (trace n "parseType.parseTAbstraction" $ parseTAbstraction (n+1) s)
      <|> (trace n "parseType.parseTOperator" $ parseTOperator (n+1) s)
      <|> (trace n "parseType.parseTVariable" $ parseTVariable (n+1) s))
  where
    -- α,β
    parseTVariable :: Int -> TVariableScope -> Parser Type
    parseTVariable n s =
      do a <- satisfy (inClass alphabet)
         n <- T.length `fmap` takeWhile (== '\'')
         let (Just m) = elemIndex a alphabet
         let id       = m + n * length alphabet
         return $ TVariable id (searchScope s id)
      where
        alphabet = "αβγδεζηθικλμνξοπρςστυφχψω"

        searchScope :: TVariableScope -> Id -> Kind
        searchScope ((b, k):vs) a
          | b == a       = k
          | otherwise    = searchScope vs a
        searchScope [] a = error "type variable is not bound"

    -- τ υ
    parseTApplication :: Int -> TVariableScope -> Parser Type
    parseTApplication n s =
      do t <- (    (trace n "parseTApplication.parseParen" $ parseParen (parseType (n+1) s))
               <|> (trace n "parseTApplication.parseTAbstraction" $ parseTAbstraction (n+1) s)
               <|> (trace n "parseTApplication.parseTQuantification" $ parseTQuantification (n+1) s)
            -- TODO: factor out indirect left recursion
            -- <|> (trace n "parseTApplication.parseTOperator" $ parseTOperator (n+1) s)
               <|> (trace n "parseTApplication.parseTVariable" $ parseTVariable (n+1) s))
         u <- trace n ("parseTApplication.parseType " ++ show t) $ parseType (n+1) s
         trace n "parseTApplication.success" $ return $ TApplication t u

    -- λα:κ. τ
    parseTAbstraction :: Int -> TVariableScope -> Parser Type
    parseTAbstraction n s =
      do parseLambda (n+1)
         a <- parseTBinding (n+1) s; char ':'
         k <- parseKind (n+1);       char '.'
         t <- parseType (n+1) $ extendScope s (a, k)
         trace n "parseTAbstraction.success" $ return $ TAbstraction a k t

    -- ∀α:κ. τ
    parseTQuantification :: Int -> TVariableScope -> Parser Type
    parseTQuantification n s =
      do trace n "parseTQuantification.parseForall" $ parseForall (n+1)
         a <- trace n "parseTQuantification.parseTBinding" $ parseTBinding (n+1) s; char ':'
         k <- trace n "parseTQuantification.parseKind" $ parseKind (n+1);       char '.'
         t <- trace n "parseTQuantification.parseType" $ parseType (n+1) $ extendScope s (a, k)
         trace (n+1) "parseTQuantification.success" $ return $ TQuantification a k t

    -- τ → υ
    parseTOperator :: Int -> TVariableScope -> Parser Type
    parseTOperator n s =
      do t <- (    (trace n "parseTOperator.parseParen" $ parseParen (parseType (n+1) s))
               <|> (trace n "parseTOperator.parseTAbstraction" $ parseTAbstraction (n+1) s)
               <|> (trace n "parseTOperator.parseTQuantification" $ parseTQuantification (n+1) s)
            -- TODO: factor out indirect left recursion
            -- <|> (trace n "parseTOperator.parseTApplication" $ parseTApplication (n+1) s)
               <|> (trace n "parseTOperator.parseTVariable" $ parseTVariable (n+1) s))
         _ <- skipSpace
         _ <- parseArrow (n+1)
         u <- parseType (n+1) s
         trace n "parseTOperator.success" $ return $ TOperator t u

parseTerm :: TVariableScope -> Parser Term
parseTerm s =
  do skipSpace
     (    parseParen (parseTerm s)
      <|> parseTmAbstraction s
      <|> parseTmUAbstraction s
      <|> parseTmVariable s
      <|> parseTmApplication s
      <|> parseTmUApplication s)
  where
    -- x,y
    parseTmBinding :: TVariableScope -> Parser Id
    parseTmBinding s =
      do x <- satisfy (inClass alphabet)
         n <- takeWhile (== '\'')
         let (Just m) = elemIndex x alphabet
         let       id = m + (length alphabet) * (T.length n)
         return $ id
      where alphabet = "abcdefghijklmnopqrstuvwxyz"

    -- x,y
    parseTmVariable :: TVariableScope -> Parser Term
    parseTmVariable s = parseTmBinding s >>= return . TmVariable

    -- f e
    parseTmApplication :: TVariableScope -> Parser Term
    parseTmApplication s =
      do f <- parseTerm s -- TODO: left factor
         e <- parseTerm s
         return $ TmApplication f e

    -- λx:τ. e
    parseTmAbstraction :: TVariableScope -> Parser Term
    parseTmAbstraction s =
      do parseLambda 0
         x <- parseTmBinding s; char ':'
         t <- parseType 0 s;      char '.'
         e <- parseTerm s
         return $ TmAbstraction x t e

    -- Λα:κ. e
    parseTmUAbstraction :: TVariableScope -> Parser Term
    parseTmUAbstraction s =
      do parseLAMBDA 0
         a <- parseTBinding 0 s; char ':'
         k <- parseKind 0;       char '.'
         e <- parseTerm $ extendScope s (a, k)
         return $ TmUAbstraction a k e

    -- f τ
    parseTmUApplication :: TVariableScope -> Parser Term
    parseTmUApplication s =
      do f <- parseTerm s -- TODO: left factor
         t <- parseType 0 s
         return $ TmUApplication f t

---------------------------------------------------------------------------

-- Bool : ★
-- Bool = ∀α:★. α → α → α
bool  = TQuantification 0 KType $
          TOperator (TVariable 0 KType) $
            TOperator (TVariable 0 KType) (TVariable 0 KType)

--tru : ∀α:★. α → α → α
--tru = Λα:★. λt:α. λf:α. t
tru = TmUAbstraction 0 KType $
         TmAbstraction 19 (TVariable 0 KType) $
           TmAbstraction 5 (TVariable 0 KType) $
             TmVariable 19

--fls : ∀α:★. α → α → α
--fls = Λα:★. λt:α. λf:α. f
fls = TmUAbstraction 0 KType $
        TmAbstraction 19 (TVariable 0 KType) $
          TmAbstraction 5 (TVariable 0 KType) $
            TmVariable 5

-- Nat : ★
-- Nat = ∀α:★. α → (α → α) → α
nat = TQuantification 0 KType $
        TOperator (TVariable 0 KType) $
          TOperator
            (TOperator (TVariable 0 KType) (TVariable 0 KType))
            (TVariable 0 KType)

-- zero : ∀α:★. α → (α → α) → α
-- zero = Λα:★. λa:α. λf:α → α. a
zero =  TmUAbstraction 0 KType $
          TmAbstraction 0 (TVariable 0 KType) $
            TmAbstraction 5 (TOperator (TVariable 0 KType) (TVariable 0 KType)) $
              TmVariable 0

-- succ : ∀α:★. α → (α → α) → α
-- succ = Λα:★. λa:α. λf:α → α. f a
succ =  TmUAbstraction 0 KType $
          TmAbstraction 0 (TVariable 0 KType) $
            TmAbstraction 5 (TOperator (TVariable 0 KType) (TVariable 0 KType)) $
              TmApplication (TmVariable 5) (TmVariable 0)

-- Product : ★ → (★ → ★)
-- Product = λα:★. λβ:★. ∀ρ:★. (α → β → ρ) → ρ
pair =  TAbstraction 0 KType $
          TAbstraction 1 KType $
            TQuantification 15 KType $
              TOperator
                (TOperator (TVariable 0 KType) $
                  TOperator (TVariable 1 KType) (TVariable 15 KType))
                (TVariable 15 KType)

-- fst : ∀α:★. ∀β:★. ∀ρ:★. (α → β → ρ) → α
-- fst = Λα:★. Λβ:★. λp:(∀ρ:★. α → β → ρ). p α (λa:α. λb:β. a)
fst = TmUAbstraction 0 KType $
        TmUAbstraction 1 KType $
          TmAbstraction 15
            (TQuantification 15 KType
              (TOperator (TVariable 0 KType)
                (TOperator (TVariable 1 KType) (TVariable 15 KType))))
            (TmApplication
              (TmUApplication (TmVariable 15) (TVariable 0 KType))
              (TmAbstraction 0 (TVariable 0 KType)
                (TmAbstraction 1 (TVariable 1 KType)
                  (TmVariable 0))))

-- snd : ∀α:★. ∀β:★. ∀ρ:★. (α → β → ρ) → β 
-- snd = Λα:★. Λβ:★. λp:(∀ρ:★. α → β → ρ). p β (λa:α. λb:β. b)
snd = TmUAbstraction 0 KType $
        TmUAbstraction 1 KType $
          TmAbstraction 15
            (TQuantification 15 KType
              (TOperator (TVariable 0 KType)
                (TOperator (TVariable 1 KType) (TVariable 15 KType))))
            (TmApplication
              (TmUApplication (TmVariable 15) (TVariable 0 KType))
              (TmAbstraction 0 (TVariable 0 KType)
                (TmAbstraction 1 (TVariable 1 KType)
                  (TmVariable 1))))

-- Sum : ★ → (★ → ★)
-- Sum = λα:★. λβ:★. ∀ρ:★. (α → ρ) → (β → ρ) → ρ
sum = TAbstraction 0 KType $
        TAbstraction 1 KType $
          TQuantification 2 KType $
            TOperator (TOperator (TVariable 0 KType) (TVariable 2 KType)) $
              TOperator (TOperator (TVariable 1 KType) (TVariable 2 KType)) $
                TVariable 2 KType

-- inL : ∀α:★. ∀β:★. α → (∀ρ:★. (α → ρ) → (β → ρ) → ρ) → α
-- inL = Λα:★. Λβ:★. ∀ρ:★. λa:α. λleft:(α → ρ). λright:(β → ρ). left a
inl = TmUAbstraction 0 KType $
        TmUAbstraction 1 KType $
          TmUAbstraction 2 KType $
            TmAbstraction 0 (TVariable 0 KType) $
              TmAbstraction 1 (TOperator (TVariable 0 KType) (TVariable 2 KType)) $
                TmAbstraction 2 (TOperator (TVariable 1 KType) (TVariable 2 KType)) $
                  TmApplication (TmVariable 1) (TmVariable 0)

-- inL : ∀α:★. ∀β:★. α → (∀ρ:★. (α → ρ) → (β → ρ) → ρ) → β
-- inR = Λα:★. Λβ:★. ∀ρ:★. λv:β. λleft:(α → ρ). λright:(β → ρ). right a
inr = TmUAbstraction 0 KType $
        TmUAbstraction 1 KType $
          TmUAbstraction 2 KType $
            TmAbstraction 0 (TVariable 1 KType) $
              TmAbstraction 1 (TOperator (TVariable 0 KType) (TVariable 2 KType)) $
                TmAbstraction 2 (TOperator (TVariable 1 KType) (TVariable 2 KType)) $
                  TmApplication (TmVariable 2) (TmVariable 0)

-- List : ★ → ★
-- List = λα:★. ∀ρ:★. (α → ρ → ρ) → ρ → ρ
list =  TAbstraction 0 KType $
          TQuantification 1 KType $
            TOperator
              (TOperator
                (TVariable 0 KType)
                (TOperator (TVariable 1 KType) (TVariable 1 KType)))
              (TOperator (TVariable 1 KType) (TVariable 1 KType))

-- null : ∀α:★. ∀ρ:★. (α → ρ → ρ) → ρ → ρ
-- null = Λα:★. ∀ρ:★. λf:(α → ρ → ρ). λr:ρ. r
null =  TmUAbstraction 0 KType $
          TmUAbstraction 1 KType $
            TmAbstraction 0
              (TOperator (TVariable 0 KType) $
                TOperator (TVariable 1 KType) (TVariable 1 KType))
              (TmAbstraction 1 (TVariable 1 KType) $
                TmVariable 1)

-- cons : ∀α:★. ∀ρ:★. α → ((α → ρ → ρ) → ρ → ρ) → ((α → ρ → ρ) → ρ → ρ)
-- cons = Λα:★. ∀ρ:★. λh:α. λt:(α → ρ → ρ) → ρ → ρ. λf:(α → ρ → ρ). λr:ρ. f h (t f r)
cons =  TmUAbstraction 0 KType $
          TmUAbstraction 1 KType $
            TmAbstraction 0 (TVariable 0 KType) $
              TmAbstraction 1
                (TOperator
                  (TOperator (TVariable 0 KType) $
                    TOperator (TVariable 1 KType) (TVariable 1 KType))
                  (TOperator (TVariable 1 KType) (TVariable 1 KType)))
                (TmAbstraction 2
                  (TOperator (TVariable 0 KType) $
                    TOperator (TVariable 1 KType) (TVariable 1 KType))
                  (TmAbstraction 3 (TVariable 1 KType) $
                    TmApplication (TmApplication (TmVariable 2) (TmVariable 0)) $
                      TmApplication (TmApplication (TmVariable 1) (TmVariable 2)) (TmVariable 3)))
