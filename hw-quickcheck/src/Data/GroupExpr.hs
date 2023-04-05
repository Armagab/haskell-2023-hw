{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Data.GroupExpr where

import Data.Group (Group (..))

-- | @GroupLit a@ is a type of literals in arbitrary group with
-- variables ranging in @a@.
--
-- For example, a literal \(x\) would be encoded as @Direct \'x\'@, while a
-- literal \(y^{-1}\) would be encoded as @Invert \'y\'@.
data GroupLit a = Direct !a | Invert !a deriving (Functor, Eq)

-- (0 баллов) Реализуйте более естественное и компактное строковое
-- представление для типа `GroupLit` вместо реализации по умолчанию.

instance (Show a) => (Show (GroupLit a)) where
  show (Direct a) = show a
  show (Invert a) = show a ++ "'"

-- | @GroupExpr a@ is a type of expressions in arbitrary group with
-- variables ranging in @a@.
--
-- For example, \(x y z y^{-1}\) would be encoded as
-- @GroupExpr [Direct \'x\', Direct \'y\', Direct \'z\', Invert \'y\']@.
newtype GroupExpr a = GroupExpr {getExpr :: [GroupLit a]}
  deriving (Functor)

-- (1 балл) Сделайте `GroupExpr a` представителем класса `Group`.
-- Можно пользоваться дерайвингом; никаких условий на `a` быть не должно.

instance Semigroup (GroupExpr a) where
  a <> b = GroupExpr $ getExpr a ++ getExpr b 

instance Monoid (GroupExpr a) where
  mempty = GroupExpr []

inv (Direct a) = Invert a
inv (Invert a) = Direct a

instance Group (GroupExpr a) where
  inverse (GroupExpr a) = GroupExpr $ map inv . reverse $ a
-- (1 балл) Реализуйте вычисление выражения в группе.
-- (0 баллов) Говоря терминами из алгебры, чем является `groupEval v`?

groupEval ::
  Group g =>
  -- | Variable assignment.
  (x -> g) ->
  -- | Expression to evaluate.
  GroupExpr x ->
  -- | Result of evaluation.
  g
groupEval ctx (GroupExpr l) = foldr (\el res -> case el of
   (Direct x) -> ctx x <> res
   (Invert x) -> (inverse $ ctx x) <> res) mempty l

-- (1 балл) Сделайте `GroupExpr` представителем класса `Monad`.
-- Подсказка: у (>>=) есть реализация в одну строчку.

helper (Direct f) (Direct x) = Direct $ f x
helper (Invert f) (Invert x) = Direct $ f x
helper (Direct f) (Invert x) = Invert $ f x
helper (Invert f) (Direct x) = Invert $ f x

instance Applicative GroupExpr where
  pure a = GroupExpr [Direct a]
  (<*>) (GroupExpr fs) (GroupExpr a) = GroupExpr $ map helper fs <*> a

unwrap (Direct a) = a
unwrap (Invert a) = a

instance Monad GroupExpr where
  return = pure
  (>>=) (GroupExpr a) f = GroupExpr $ a >>= (\wrapped -> case wrapped of (Direct a) -> getExpr . f $ a
                                                                         (Invert a) -> getExpr $ inverse . f $ a)

-- (2.5 балла) Реализуйте проверку выражений на равенство с точностью до
-- сокращений:
-- `GroupExpr [Direct 'y', Direct 'x', Invert 'x'] == GroupExpr [Direct 'y']`
-- должно возвращать True.

deletes a b = inv a == b

simplifyHelper (GroupExpr a) = GroupExpr $ foldr (\el res -> if res /= [] then if deletes el (head res) then tail res else el:res else el:res) [] a
simpleEq (GroupExpr l1) (GroupExpr l2) = l1 == l2
simplify c = let smplfd = simplifyHelper c in if simpleEq c smplfd then c else simplify smplfd

instance Eq a => Eq (GroupExpr a) where
  (==) a b = simpleEq (simplify a) (simplify b)

-- (1 балл) Реализуйте более естественное и компактное строковое представление
-- для типа `GroupExpr a` вместо реализации по умолчанию.
instance Show a => Show (GroupExpr a) where
  show (GroupExpr l) = if null l then "1" else concatMap show l

