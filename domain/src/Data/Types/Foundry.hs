module Data.Types.Foundry
  ( FoundryData(..)
  , Armor(..)
  , Equipment(..)
  , Weapon(..)
  ) where

import           Flipstone.Prelude
import           Domain.JSON
import           Data.Types.Prelude

data FoundryData
  = FoundryArmor     Armor
  | FoundryEquipment Equipment
  | FoundryWeapon    Weapon

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

instance ToJSON Equipment where
  toJSON e =
    object [ "price"       .= equipmentPrice e
           , "breakPoints" .= equipmentBreakpoints e
           , "trainings"   .= equipmentTrainings e
           , "weight"      .= equipmentWeight e
           , "description" .= equipmentDescription e
           ]

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
    , weaponTags        :: WeaponTags
    , weaponFireModes   :: FireModes
    , weaponAttack      :: Attack
    , weaponReload      :: Reload
    , weaponNickname    :: Maybe Name
    , weaponType        :: WeaponType
    , weaponMagCap      :: MagazineCapacity
    , weaponAmmo        :: Ammo
    , weaponAmmoGroup   :: AmmoGroup
    , weaponScopeMag    :: Maybe ScopeMagnification
    , weaponCurrentAmmo :: Name
    , weaponAmmoList    :: AmmoList
    , weaponSettings    :: WeaponSettings
    }

instance ToJSON Weapon where
  toJSON w =
    object [ "faction"            .= weaponFaction w
           , "description"        .= weaponDescription w
           , "price"              .= weaponPrice w
           , "breakPoints"        .= weaponBreakpoints w
           , "trainings"          .= weaponTrainings w
           , "weight"             .= weaponWeight w
           , "group"              .= weaponGroup w
           , "tags"               .= weaponTags w
           , "fireMode"           .= weaponFireModes w
           , "attack"             .= weaponAttack w
           , "reload"             .= weaponReload w
           , "nickname"           .= weaponNickname w
           , "type"               .= weaponType w
           , "magazineCapacity"   .= weaponMagCap w
           , "ammo"               .= weaponAmmo w
           , "ammoGroup"          .= weaponGroup w
           , "scopeMagnification" .= weaponScopeMag w
           , "currentAmmo"        .= weaponCurrentAmmo w
           , "ammoList"           .= weaponAmmoList w
           , "settings"           .= weaponSettings w
           ]
