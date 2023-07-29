module BankAccount
  ( BankAccount,
    closeAccount,
    getBalance,
    incrementBalance,
    openAccount,
  )
where

import Control.Monad.State
    ( modify, evalStateT, MonadIO(liftIO), MonadState(get), StateT )

type BankAccount = Maybe Integer

openAccount :: BankAccount
openAccount = Just 0

closeAccount :: StateT BankAccount IO ()
closeAccount = modify (const Nothing) >> getBalance >>= liftIO . print

getBalance :: StateT BankAccount IO BankAccount
getBalance = get


incrementBalance :: Integer -> StateT BankAccount IO ()
incrementBalance amount = do modify $ fmap (+ amount)
                             getBalance >>= liftIO . print



someFuncs :: StateT BankAccount IO ()
someFuncs = do  incrementBalance 10
                incrementBalance 50
                incrementBalance (-30)
                closeAccount
                incrementBalance 30
                incrementBalance 55

someTest :: IO ()
someTest = evalStateT someFuncs openAccount