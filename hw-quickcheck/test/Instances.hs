module Instances where

import Data.GroupExpr (GroupExpr(GroupExpr), GroupLit (Direct, Invert))
import Data.Var
import Test.QuickCheck (Arbitrary(arbitrary), oneof, listOf, frequency)

-- (1 балл) Сделайте `Var`, `GroupLit` и `GroupExpr` представителями класса
-- Arbitrary.
--
-- Генератор `Var` должен выбирать равновероятно одну из 3-4 выбранных Вами
-- переменных, чтобы в получающемся слове часто встречались сократимые
-- фрагменты.
--
-- Случайный `GroupLit` должен быть `Direct` с вероятностью 75%, а `Invert` ---
-- с вероятностью 25% (для этого есть готовый комбинатор, найдите его).
--
-- Генератор `GroupExpr` должен быть разумным.

instance Arbitrary Var where
  arbitrary = oneof $ map (return . Var) ['a', 'b', 'c', 'd']

instance Arbitrary a => Arbitrary (GroupLit a) where
    arbitrary = frequency [(3, (fmap Direct arbitrary)), (1, fmap Invert arbitrary)]

instance Arbitrary a => Arbitrary (GroupExpr a) where
    arbitrary = fmap GroupExpr (listOf arbitrary)
