module Data.Types.Foundry
  ( FoundryData(..)
  , Ability(..)
  , Armor(..)
  , Equipment(..)
  , Weapon(..)
  ) where

import           Flipstone.Prelude
import           Domain.JSON
import           Data.Types.Prelude

import           Data.Maybe (fromMaybe)
import qualified Data.Text as T

data FoundryData
  = FoundryAbility   Ability
  | FoundryArmor     Armor
 -- | FoundryBestiary  Bestiary
 -- | FoundryCharacter Character
  | FoundryEquipment Equipment
 -- | FoundryFlood     Flood
 -- | FoundryVehicle   Vehicle
  | FoundryWeapon    Weapon

instance CompendiumEntry FoundryData where
  named (FoundryAbility   a) = named a
  named (FoundryArmor     a) = named a
--  named (FoundryBestiary  b) = named b
--  named (FoundryCharacter c) = named c
  named (FoundryEquipment e) = named e
--  named (FoundryFlood     f) = named f
--  named (FoundryVehicle   v) = named v
  named (FoundryWeapon    w) = named w

  imged (FoundryAbility   a) = imged a
  imged (FoundryArmor     a) = imged a
--  imged (FoundryBestiary  b) = imged b
--  imged (FoundryCharacter c) = imged c
  imged (FoundryEquipment e) = imged e
--  imged (FoundryFlood     f) = imged f
--  imged (FoundryVehicle   v) = imged v
  imged (FoundryWeapon    w) = imged w

  typed (FoundryAbility   a) = typed a
  typed (FoundryArmor     a) = typed a
--  typed (FoundryBestiary  b) = typed b
--  typed (FoundryCharacter c) = typed c
  typed (FoundryEquipment e) = typed e
--  typed (FoundryFlood     f) = typed f
--  typed (FoundryVehicle   v) = typed v
  typed (FoundryWeapon    w) = typed w

instance ToJSON FoundryData where
  toJSON (FoundryAbility   a) = toJSON a
  toJSON (FoundryArmor     a) = toJSON a
--  toJSON (FoundryBestiary  b) = toJSON b
--  toJSON (FoundryCharacter c) = toJSON c
  toJSON (FoundryEquipment e) = toJSON e
--  toJSON (FoundryFlood     f) = toJSON f
--  toJSON (FoundryVehicle   v) = toJSON v
  toJSON (FoundryWeapon    w) = toJSON w

data Ability =
  Ability
    { abilityName        :: Name
    , abilityPrereqs     :: Prerequisites
    , abilityCost        :: Int
    , abilitySummary     :: Description
    , abilityDescription :: Description
    , abilityType        :: AbilityType
    }

instance CompendiumEntry Ability where
  named = abilityName
  imged = const (mkImg "") -- TODO
  typed = const (FoundryItem ItemAbility)

instance ToJSON Ability where
  toJSON a =
    object [ "prerequisite" .= abilityPrereqs     a
           , "cost"         .= abilityCost        a
           , "summary"      .= abilitySummary     a
           , "description"  .= abilityDescription a
           , "type"         .= abilityType        a
           ]

data Armor =
  Armor
    { armorName        :: Name
    , armorVariant     :: Name
    , armorFaction     :: Faction
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
  imged = const armorImg
  typed = const (FoundryItem ItemArmor)

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

armorImg :: Img
armorImg =
  mkImg "icons/equipment/chest/breastplate-layered-leather-green.webp"

data Equipment =
  Equipment
    { equipmentName            :: Name
    , equipmentFaction         :: Faction
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
  imged = const equipmentImg
  typed = const (FoundryItem ItemEquipment)

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

equipmentImg :: Img
equipmentImg =
  mkImg "icons/containers/chest/chest-simple-walnut.webp"

data Weapon =
  Weapon
    { weaponName            :: Name
    , weaponFaction         :: Faction
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
  typed = const (FoundryItem ItemWeapon)

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
weaponImg weapon =
  mkImg $
    case T.toUpper . unWeaponType $ weaponType weapon of
      "AUTOCANNON"             -> "icons/weapons/artillery/cannon-engraved-gold.webp"
      "AXE"                    -> "icons/weapons/axes/axe-battle-blackened.webp"
      "BEAM"                   -> "icons/magic/lightning/bolt-beam-strike-blue.webp"
      "CANNON"                 -> "icons/weapons/artillery/cannon-banded.webp"
      "CARBINE"                -> "icons/weapons/guns/rifle-bayonet.webp"
      "CHEMICAL SPRAYER"       -> "icons/magic/fire/blast-jet-stream-embers-red.webp"
      "CLUB"                   -> "icons/weapons/clubs/club-heavy-barbed-brown.webp"
      "COILGUN"                -> "icons/commodities/tech/coil-steel-grey.webp"
      "DAGGER"                 -> "icons/weapons/daggers/dagger-curved-blue.webp"
      "DEMOLITION"             -> "icons/weapons/thrown/bomb-detonator.webp"
      "DEMOLITIONS"            -> "icons/weapons/thrown/bomb-detonator.webp"
      "ENERGY WEAPON"          -> "icons/commodities/tech/tube-chamber-lightning.webp"
      "FIST WEAPON"            -> "icons/weapons/fist/claw-gauntlet-gray.webp"
      "GARROTE"                -> "icons/sundries/survival/rope-noose-brown.webp"
      "GRENADE"                -> "icons/weapons/thrown/grenade-round.webp"
      "GRENADE LAUNCHER"       -> "icons/weapons/thrown/rocket.webp"
      "HAMMER"                 -> "icons/weapons/hammers/hammer-double-engraved-gold.webp"
      "HEAVY MACHINE GUN"      -> "icons/weapons/ammunition/bullets-cartridge-shell-gray.webp"
      "KNIFE"                  -> "icons/weapons/daggers/dagger-serrated-black.webp"
      "LANDMINE"               -> "icons/weapons/thrown/bomb-detonator.webp"
      "LIGHT MACHINE GUN"      -> "icons/weapons/ammunition/bullets-cartridge-shell-gray.webp"
      "MACE"                   -> "icons/weapons/maces/mace-round-spiked-black.webp"
      "MACHINE GUN"            -> "icons/weapons/ammunition/bullets-cartridge-shell-gray.webp"
      "MAGAZINE SHOTGUN"       -> "icons/weapons/guns/gun-blunderbuss-gold.webp"
      "MELEE SHIELD"           -> "icons/equipment/shield/oval-wooden-boss-steel.webp"
      "MISSILE LAUNCHER"       -> "icons/weapons/thrown/rocket.webp"
      "MORTAR CANNON"          -> "icons/skills/ranged/cannon-barrel-firing-yellow.webp"
      "NATURAL ATTACK"         -> "icons/skills/melee/unarmed-punch-fist-blue.webp"
      "ONE-HANDED SWORD"       -> "icons/weapons/swords/shortsword-guard-brass.webp"
      "ORDINANCE"              -> "icons/weapons/thrown/bomb-fuse-blue.webp"
      "PISTOL"                 -> "icons/weapons/guns/gun-pistol-flintlock-metal.webp"
      "POLEARM AXE"            -> "icons/weapons/polearms/halberd-crescent-small-spiked.webp"
      "POLEARM SPIKE"          -> "icons/weapons/polearms/spear-ornate-gold.webp"
      "RAILGUN"                -> "icons/commodities/tech/coil-steel-grey.webp"
      "RIFLE"                  -> "icons/weapons/guns/rifle-brown.webp"
      "ROCKET LAUNCHER"        -> "icons/weapons/thrown/rocket.webp"
      "SATCHEL CHARGE"         -> "icons/weapons/thrown/bomb-timer.webp"
      "SHOVEL"                 -> "icons/tools/hand/shovel-spade-steel-grey.webp"
      "SINGLE LOADING SHOTGUN" -> "icons/weapons/guns/gun-blunderbuss-bronze.webp"
      "SMG"                    -> "icons/weapons/guns/gun-pistol-brown.webp"
      "SNIPER RIFLE"           -> "icons/weapons/guns/gun-topbarrel.webp"
      "SPRAY WEAPON"           -> "icons/skills/wounds/blood-spurt-spray-red.webp"
      "TASER"                  -> "icons/magic/lightning/bolts-forked-large-blue.webp"
      "TWO-HANDED SWORD"       -> "icons/weapons/swords/greatsword-blue.webp"
      _                        -> "icons/skills/targeting/target-strike-triple-blue.webp"
