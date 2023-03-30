module Data.Types.Foundry
  ( FoundryData(..)
  , Armor(..)
  , Equipment(..)
  , Weapon(..)
  ) where

import           Flipstone.Prelude
import           Domain.JSON
import           Data.Types.Prelude

import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T

data FoundryData
  = FoundryArmor     Armor
  | FoundryEquipment Equipment
  | FoundryWeapon    Weapon

instance CompendiumEntry FoundryData where
  named (FoundryArmor a)     = named a
  named (FoundryEquipment e) = named e
  named (FoundryWeapon w)    = named w

  imged (FoundryArmor a)     = imged a
  imged (FoundryEquipment e) = imged e
  imged (FoundryWeapon w)    = imged w

  typed (FoundryArmor a)     = typed a
  typed (FoundryEquipment e) = typed e
  typed (FoundryWeapon w)    = typed w

instance ToJSON FoundryData where
  toJSON (FoundryArmor     a) = toJSON a
  toJSON (FoundryEquipment e) = toJSON e
  toJSON (FoundryWeapon    w) = toJSON w

data Armor =
  Armor
    { armorName        :: Name
    , armorVariant     :: Name
    , armorFaction     :: T.Text
    , armorDescription :: Description
    , armorPrice       :: ItemPrice
    , armorBreakpoints :: Breakpoints
    , armorTrainings   :: ItemTrainings
    , armorWeight      :: Weight
    , armorStats       :: StatAdjustments
    , armorHardpoints  :: Hardpoints
    , armorMaterial    :: T.Text
    , armorNotes       :: ArmorNotes
    , armorProtection  :: Protection
    , armorShields     :: Shields
    , armorSize        :: Size
    }

instance CompendiumEntry Armor where
  named = armorName
  imged = const (mkImg "icons/equipment/chest/breastplate-layered-leather-green.webp")
  typed = const ItemArmor

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
    { equipmentName            :: Name
    , equipmentPrice           :: ItemPrice
    , equipmentBreakpoints     :: Breakpoints
    , equipmentTrainings       :: ItemTrainings
    , equipmentWeight          :: Weight
    , equipmentDescription     :: Description
    , equipmentShields         :: Maybe Shields
    , equipmentCharacteristics :: Maybe StatAdjustments
    }

instance CompendiumEntry Equipment where
  named = equipmentName
  imged = const (mkImg "icons/containers/chest/chest-simple-walnut.webp")
  typed = const ItemEquipment

instance ToJSON Equipment where
  toJSON e =
    object
      [ "price"           .= equipmentPrice e
      , "breakPoints"     .= equipmentBreakpoints e
      , "trainings"       .= equipmentTrainings e
      , "weight"          .= equipmentWeight e
      , "description"     .= equipmentDescription e
      , "shields"         .= fromMaybe emptyShields (equipmentShields e)
      , "characteristics" .= fromMaybe emptyStatAdjustments
                                       (equipmentCharacteristics e)
      ]

data Weapon =
  Weapon
    { weaponName            :: Name
    , weaponFaction         :: T.Text
    , weaponDescription     :: Description
    , weaponPrice           :: ItemPrice
    , weaponBreakpoints     :: Breakpoints
    , weaponTrainings       :: ItemTrainings
    , weaponWeight          :: Weight
    , weaponGroup           :: WeaponGroup
    , weaponTags            :: WeaponTags
    , weaponFireModes       :: FireModes
    , weaponAttack          :: Attack
    , weaponReload          :: Reload
    , weaponNickname        :: Maybe Name
    , weaponType            :: WeaponType
    , weaponMagCap          :: MagazineCapacity
    , weaponAmmo            :: Ammo
    , weaponAmmoGroup       :: AmmoGroup
    , weaponScopeMag        :: Maybe ScopeMagnification
    , weaponCurrentAmmo     :: Name
    , weaponAmmoList        :: AmmoList
    , weaponShields         :: Maybe Shields
    , weaponCharacteristics :: Maybe StatAdjustments
    , weaponSettings        :: WeaponSettings
    }

instance CompendiumEntry Weapon where
  named = weaponName
  imged = weaponImg
  typed = const ItemWeapon

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
           , "shields"            .= fromMaybe emptyShields (weaponShields w)
           , "characteristics"    .= fromMaybe emptyStatAdjustments
                                               (weaponCharacteristics w)
           , "settings"           .= weaponSettings w
           ]

weaponImg :: Weapon -> Img
weaponImg weapon = mkImg $
  case weaponGroup weapon of
    MeleeGroup -> "icons/containers/chest/chest-simple-walnut.webp"
    Thrown     -> "icons/weapons/thrown/bomb-fuse-blue.webp"
    Ranged
      | isJust $ Map.lookup Drawback $ fireModes $ weaponFireModes weapon ->
        "icons/weapons/bows/bow-ornamental-carved-brown.webp"

      | isJust $ Map.lookup Flintlock $ fireModes $ weaponFireModes weapon ->
        "icons/weapons/guns/gun-pistol-flintlock-metal.webp"

      | otherwise ->
        case weaponAmmoGroup weapon of
          None         -> "icons/weapons/guns/gun-pistol-flintlock-metal.webp"
          STD          -> "icons/weapons/guns/rifle-brown.webp"
          Shotgun      -> "icons/weapons/guns/gun-blunderbuss-gold.webp"
          Flamethrower -> "icons/magic/fire/blast-jet-stream-embers-red.webp"
          Sniper       -> "icons/weapons/guns/gun-topbarrel.webp"
          Grenade      -> "icons/weapons/thrown/grenade-round.webp"
          MRC          -> "icons/weapons/artillery/cannon-engraved-gold.webp"
          BruteShot    -> "icons/weapons/thrown/bomb-purple.webp"
