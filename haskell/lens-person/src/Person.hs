{-# LANGUAGE TemplateHaskell #-}

module Person
  ( Address (..)
  , Born    (..)
  , Name    (..)
  , Person  (..)
  , bornStreet
  , renameStreets
  , setBirthMonth
  , setCurrentStreet
  ) where

import Control.Lens
import Data.Time.Calendar (Day)

data Person = Person { _name    :: Name
                     , _born    :: Born
                     , _address :: Address
                     }

data Name = Name { _foreNames :: String
                 , _surName   :: String
                 }

data Born = Born { _bornAt :: Address
                 , _bornOn :: Day
                 }

data Address = Address { _street      :: String
                       , _houseNumber :: Int
                       , _place       :: String
                       , _country     :: String
                       }

$(makeLenses ''Person)
$(makeLenses ''Name)
$(makeLenses ''Born)
$(makeLenses ''Address)                       

bornStreet :: Born -> String
bornStreet = view (bornAt . street) 

setCurrentStreet :: String -> Person -> Person
setCurrentStreet newStreet = over (address . street) (const newStreet)

setBirthMonth :: Int -> Person -> Person
setBirthMonth month = over (born . bornOn) (const month) 

renameStreets :: (String -> String) -> Person -> Person
renameStreets f person = error "You need to implement this function."
