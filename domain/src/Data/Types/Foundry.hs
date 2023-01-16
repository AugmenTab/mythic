module Data.Types.Foundry
  ( Armor
  , Equipment
  , Weapon
  ) where

import           Flipstone.Prelude
import           Domain.JSON
import           Data.Types.Prelude

import qualified Data.Map as Map

data Armor =
  Armor
    { armorName        :: Name
    , armorVariant     :: Name
    , armorFaction     :: Text
    , armorDescription :: Description
    , armorPrice       :: ItemPrice
    , armorBreakpoints :: Breakpoints
    , armorTrainings   :: ItemTrainings
    , armorWeight      :: Weight
    , armorStats       :: StatAdjustments
    , armorHardpoints  :: Hardpoints
    , armorMaterial    :: Text
    , armorNotes       :: ArmorNotes
    , armorProtection  :: Protection
    , armorShields     :: Shields
    , armorSize        :: Size
    }

instance ToJSON Armor where
  toJSON a =
    object [ "faction"         .= armorFaction a
           , "description"     .= armorDescription a
           , "price"           .= armorPrice a
           , "breakPoints"     .= armorBreakpoints a
           , "trainings"       .= armorTrainings a
           , "weight"          .= armorWeight a
           , "characteristics" .= armorStats a
           , "hardpoints"      .= armorHardpoints a
           , "material"        .= armorMaterial a
           , "notes"           .= armorNotes a
           , "protection"      .= armorProtection a
           , "shields"         .= armorShields a
           , "size"            .= armorSize a
           , "variant"         .= armorVariant a
           ]

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
