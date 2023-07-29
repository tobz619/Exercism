module BankAccountState2
  ( BankAccount,
    closeAccount,
    getBalance,
    incrementBalance,
    openAccount,
  )
where

import Control.Monad.State

type BankAccount = Maybe Integer

openAccount :: ((), BankAccount)
openAccount = (() , Just 0)

closeAccount :: BankAccount -> ((), BankAccount)
closeAccount _ = (() , Nothing)

getBalance :: BankAccount -> (BankAccount, BankAccount)
getBalance a = (a,a)


incrementBalance :: Integer -> BankAccount -> ((), BankAccount)
incrementBalance amount acc = ((), (+ amount) <$> acc)


openAccountS :: StateT BankAccount IO ()
openAccountS = state (const openAccount) >> get >>= liftIO . print

incrementBalanceS :: Integer -> StateT BankAccount IO ()
incrementBalanceS int = state (incrementBalance int) >> get >>= liftIO . print

closeAccountS :: StateT BankAccount IO ()
closeAccountS = state closeAccount >> get >>= liftIO . print

some2Funcs :: StateT BankAccount IO ()
some2Funcs = do         openAccountS
                        incrementBalanceS 10
                        incrementBalanceS 50
                        incrementBalanceS (-30)
                        closeAccountS
                        incrementBalanceS 30
                        incrementBalanceS 55
                        return ()



some2Test = evalStateT some2Funcs (Just 0)