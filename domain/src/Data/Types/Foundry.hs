module Data.Types.Foundry
  ( Armor
  , Equipment
  , Weapon
  ) where

import           Flipstone.Prelude
import           Data.Types.Prelude

import qualified Data.Map as Map

data Armor =
  Armor
    { armorName        :: Name
    , armorVariant     :: Name
    , armorFaction     :: Text
    , armorDescription :: Description
    , armorPrice       :: ItemPrice
    , armorBreakPoints :: Breakpoints
    , armorTrainings   :: ItemTrainings
    , armorWeight      :: Weight
    , armorStats       :: StatAdjustments
    , armorHardpoints  :: Hardpoints
    , armorMaterial    :: Text
    , armorNotes       :: ArmorNotes
    , armorProtection  :: Protection
    , armorShields     :: Shields
    }

data Equipment =
  Equipment
    { equipmentName        :: Name
    , equipmentPrice       :: ItemPrice
    , equipmentBreakpoints :: Breakpoints
    , equipmentTrainings   :: ItemTrainings
    , equipmentWeight      :: Weight
    , equipmentDescription :: Description
    }

data Weapon =
  Weapon
    { weaponName        :: Name
    , weaponFaction     :: Text
    , weaponDescription :: Description
    , weaponPrice       :: ItemPrice
    , weaponBreakpoints :: Breakpoints
    , weaponTrainings   :: ItemTrainings
    , weaponWeight      :: Weight
    , weaponGroup       :: WeaponGroup
    , weaponTags        :: Set WeaponTag
    , weaponFireModes   :: Map.Map FireMode FireRate
    , weaponAttack      :: Attack
    , weaponReload      :: Reload
    , weaponNickname    :: Maybe Name
    , weaponType        :: WeaponType
    , weaponMagCap      :: MagazineCapacity
    , weaponAmmo        :: Ammo
    , weaponAmmoGroup   :: AmmoGroup
    , weaponScopeMag    :: Maybe ScopeMagnification
    , weaponAmmoList    :: [Ammunition]
    , weaponSettings    :: WeaponSettings
    }
