module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import qualified Data.Map as Map
import Control.Concurrent.STM

newtype BankAccount = BankAccount {value :: Maybe Integer}

closeAccount :: TVar BankAccount -> IO ()
closeAccount account = atomically $ writeTVar account (BankAccount (Nothing))

getBalance :: TVar BankAccount -> IO (Maybe Integer)
getBalance account = atomically $ do bal <- fmap value . readTVar $ account
                                     pure bal

incrementBalance :: TVar BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance account amount = atomically $ do
                                    current <- readTVar account
                                    newAcc <- writeTVar account $ BankAccount (fmap (+ amount) (value current))
                                    val <- fmap value . readTVar $ account
                                    pure val


openAccount :: IO (TVar BankAccount)
openAccount = atomically $ do acc <- newTVar (BankAccount (Just 0))
                              pure acc

someTest = do account <- openAccount
              a <- getBalance account
              print a
              incrementBalance account 10 >>= print
              incrementBalance account 50 >>= print
              incrementBalance account (-30) >>= print
              closeAccount account >>= print
              incrementBalance account 30 >>= print
              incrementBalance account 55 >>= print