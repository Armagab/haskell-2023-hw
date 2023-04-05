{-# LANGUAGE TemplateHaskell #-}

import Control.Monad (unless)
import Data.GroupExpr (GroupExpr)
import Data.Var (Var)
import Instances
import System.Exit (exitFailure)
import Test.QuickCheck
import Data.Group (Group(inverse))

-- (0 баллов) Выполняется ли свойство `prop_minimum`? Проверьте.
-- (0.5 балла) Объясните.

-- Выполняется
-- Это происходит потому что минимум реализован через какой-то из foldов, a fold для пары возвращает второй элемент 
prop_minimum = minimum(100, 500) === 500

-- (1 балл) Выпишите аксиомы групп в виде свойств QuickCheck и проверьте, что
-- они выполняются для Вашей реализации `GroupExpr`.

type TestExpr = GroupExpr Var

prop_commutative :: TestExpr -> TestExpr -> TestExpr -> Bool
prop_commutative a b c = (a <> b ) <> c == a <> (b <> c)

prop_neutral :: TestExpr -> Bool
prop_neutral a = (a <> mempty == a) && (mempty <> a == a) 

prop_inverse :: TestExpr -> Bool
prop_inverse a = (a <> (inverse a) == mempty) && ((inverse a) <> a == mempty)

return []

main = do
  success <- $quickCheckAll
  unless success exitFailure
