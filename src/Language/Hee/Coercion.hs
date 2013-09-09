module Hee.Language.Evidence
  where

data Coercion
  = Coercion Type Type  -- a ~ b
  deriving (Eq, Show)

data Evidence
  = Variable      Var
  | Axiom         [Evidence]        -- C γ
  | Application   Coercion Coercion -- γ γ
  | Reflexivity   Type              -- a              => a ~ a
  | Transitivity  Coercion Coercion -- a ~ b,  b ~ c  => a ~ c
  | Symmetry      Coercion          -- a ~ b          => b ~ a
  | Injectivity   Int Coercion      -- nth k γ
  | Polymorphism  Var Coercion      -- ∀a:n.γ
  | Instantiation Coercion Coercion -- γ @ τ
  deriving (Eq, Show)

