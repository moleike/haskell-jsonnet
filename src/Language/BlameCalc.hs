{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- |
module BlameCalc where

import GHC.Generics
import Unbound.Generics.LocallyNameless

data Type
  = Number
  | Dyn
  | Arr Type Type
  deriving (Eq, Show, Generic)

newtype Label = Label Int
  deriving (Eq, Show, Generic)

data TypeError
  = Mismatch Type Type
  | NotFound Var
  deriving (Show)

data Coercion
  = Id Type
  | Proj Type Label
  | Inj Type
  | Fun Coercion Coercion
  | Seq Coercion Coercion
  | Fail Type Type Label
  deriving (Eq, Show, Generic)

type Var = Name Term

type Lam = Bind (Var, Embed Type) Term

data Term
  = Num Int
  | Var Var
  | Lam Lam
  | App Term Term
  | Cast Coercion Term
  deriving (Eq, Show, Generic)

type Ctx = [(Var, Type)]

type Check = ExceptT TypeError (ReaderT Ctx (FreshMT Identity))

lookupVar :: Var -> Check Type
lookupVar v =
  lookup v <$> ask >>= \case
    Just s -> pure s
    Nothing -> throwError (NotFound v)

coerce :: Type -> Type -> Coercion
coerce a b | a == b = Id a
coerce Dyn ty = Proj ty (Label 0)
coerce ty Dyn = Inj ty
coerce arr@(Arr Dyn Dyn) Dyn = Inj arr
coerce Dyn arr@(Arr Dyn Dyn) Dyn = Proj arr (Label 0)

tc :: Term -> Check (Term, Type)
tc = \case
  n@(Num _) -> (n, Number)
  Var n -> (n,) <$> lookupVar n
  Lam l -> do
    ((n, ta), e) <- unbind l
    (e', tb) <- tc e
    pure (lam e', Arr ta tb)

--App e1 e2 -> do
--  (e1', t1) <- tc e1
--  (e2', t2) <- tc e2
--  case (t1, t2) of
--    (Dyn, )
