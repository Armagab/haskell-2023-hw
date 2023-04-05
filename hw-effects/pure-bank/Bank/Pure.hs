{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Bank.Pure where

import Bank (MonadBank (..))
import Data.Accounts (Account, Accounts, newAccount, balance, modifyBalance, Result)
import qualified Data.IntMap.Strict as M
import Control.Monad.Trans.Class (lift)
import Data.Monoid (Sum(Sum))
import Control.Monad.Trans.State (StateT (runStateT), evalStateT, get, put, evalState)
import Control.Monad.IO.Class (MonadIO)
import Bank.Error (BankError(NoAccount))

-- (0.5 балла) исправьте тип поля @runPureBank@, чтобы для @PureBank@ можно было
-- выписать инстанс @MonadBank@, для которого выполнены сформулированные Вами
-- законы. Рекомендуется использовать трансформеры монад.

-- | Monad for pure bank computations. @b@ is a balance type,
-- @a@ is a computation result type.
newtype PureBank b a = PureBank {runPureBank :: StateT (Accounts b) Result a} deriving newtype (Functor, Applicative, Monad)

-- (0.5 балла) сделайте @PureBank b@ монадой. Если в предыдущем задании Вы
-- использовали трансформеры, рекомендуется воспользоваться командой
-- @deriving newtype@.

-- (1 балл) сделайте @PureBank b@ представителем класса @MonadBank@. Заголовок
-- инстанса менять запрещается. Рекомендуется использовать вспомогательные
-- чистые функции, реализованные ранее.

instance (Num b, Ord b) => MonadBank Account (Sum b) (PureBank b) where
  newAccount = PureBank $ do
    state <- get
    let acc = Data.Accounts.newAccount state
    put $ M.insert acc 100 state
    return acc

  balance acc = PureBank $ do
    state <- get
    let bal = Data.Accounts.balance acc state
    case bal of
      Left err -> return 0
      Right a -> return $ Sum a

  deposit acc (Sum amount) = PureBank $ do
    state <- get
    case Data.Accounts.modifyBalance acc (\x -> Right $ x + amount) state of
      Left err -> return ()
      Right s -> put s

