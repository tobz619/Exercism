{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module BankAccountState
  ( BankAccount,
    closeAccount,
    getBalance,
    incrementBalance,
    openAccount,
  )
where

import Control.Monad.State

type BankAccount = Maybe Integer

openAccount :: BankAccount
openAccount = pure 0

closeAccount :: StateT BankAccount IO ()
closeAccount = modify (const Nothing) >> get >>= liftIO . print

getBalance ::  StateT BankAccount IO ()
getBalance = get >>= liftIO . print


incrementBalance :: Integer -> StateT BankAccount IO BankAccount
incrementBalance amount = do modify $ fmap (+ amount)
                             get >>=  liftIO . print
                             get




someFuncs = do  incrementBalance 10
                incrementBalance 50
                incrementBalance (-30)
                getBalance
                closeAccount
                incrementBalance 30
                incrementBalance 55

someTest = evalStateT someFuncs openAccount
