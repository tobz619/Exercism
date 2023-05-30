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
import Data.Time.Calendar (Day, fromGregorian, toGregorian)

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
setCurrentStreet = set (address . street)

setBirthMonth :: Int -> Person -> Person
setBirthMonth month person = set (born . bornOn) (editDay month) person
                  where editDay m = let (y',_,d') = toGregorian . view (born . bornOn) $ person
                                     in fromGregorian y' m d'

renameStreets :: (String -> String) -> Person -> Person
renameStreets f = over (address . street) f . over (born . bornAt . street) f
