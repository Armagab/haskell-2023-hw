{-# LANGUAGE FunctionalDependencies #-}

module Bank where

-- | This typeclass contains operations which one should be able to perform on a
-- certain bank system. In @MonadBank a b m@, @a@ is an account type, @b@ is a
-- balance type and @m@ is a monad where all actions are performed.
--
-- If there is no account @x@, operations @balance x@, @deposit x m@ and
-- @withdraw x m@ should all error out in some way.
class Monad m => MonadBank a b m | m -> a, m -> b where
  -- | Creates a new account.
  newAccount :: m a

  -- | @balance x@ returns the balance of account @x@.
  balance :: a -> m b

  -- | @deposit x m@ adds @m@ to the balance of account @x@.
  deposit :: a -> b -> m ()

  -- | @withdraw x m@ subtracts @m@ from the balance of account @x@.
  withdraw :: a -> b -> m ()

  -- | @deleteAccount x@ deletes an account @x@.
  deleteAccount :: a -> m ()

-- (1.5 балла) Допишите ниже законы, которые, по Вашему мнению, должны
-- выполняться для разумной реализации класса @MonadBank@. Для формулировки
-- некоторых из них можно использовать ограничения @Monoid b@ и @Ord b@.

-- ^ @MonadBank@ laws:
--
-- [Starting funds] @(newAccount >>= balance) == 0@
-- [Deposit] @balance acc >>= (\oldBalance -> deposit acc n >> balance acc) == n + oldBalance@
-- [Deleted Account Balance] @newAccount >>= (\acc -> deleteAccount acc >> balance acc) != 0@

-- (0.5 балла) Найдите ошибку в реализации `transfer` и исправьте её.
-- Если аккаунт "to" не существует, то деньги с аккаунта from спишутся, а потом выкинется ошибка
-- поэтому прежде чем списывать деньги, надо проверить что аккаунт "to" существует
-- для этого проверим его баланс, если функция завершится успешно, значит аккаунт существует
transfer :: MonadBank a b m => a -> b -> a -> m ()
transfer from amount to = balance to >> withdraw from amount >> deposit to amount
