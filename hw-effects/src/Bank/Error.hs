module Bank.Error where

import Control.Exception (Exception)

-- (1 балл) Перечислите всё, что может пойти не так в ходе выполнения операций
-- из @MonadBank@.

data BankError = NoFunds | NoAccount deriving (Show)

instance Exception BankError
