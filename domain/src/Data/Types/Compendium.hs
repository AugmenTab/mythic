module Data.Types.Compendium
  ( Compendium
  , Entry
  , GameItem
  , Img
  , ItemID
  , Permission
  ) where

import           Flipstone.Prelude
import           Data.Types.Prelude
import           Data.Types.Foundry

import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)

newtype Compendium = Compendium [Entry]

data Entry =
  Entry
    { entryId         :: ItemID
    , entryName       :: Name
    , entryType       :: ItemType
    , entryImg        :: Img
    , entryData       :: GameItem
    , entryPermission :: Permission
    }

data GameItem
  = ArmorItem     Armor
  | EquipmentItem Equipment
  | WeaponItem    Weapon

newtype Img = Img Text

newtype ItemID = ItemID Text

newtype Permission = Permission Int
