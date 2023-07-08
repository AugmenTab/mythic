module Data.Types.Foundry
  ( FoundryData(..)
  , Ability(..)
  , Armor(..)
  , Equipment(..)
  , Flood(..)
  , Weapon(..)

  -- Type Classes
  , CompendiumEntry(..)
  ) where

import           Flipstone.Prelude
import           Domain.JSON
import           Data.Types.Prelude

import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Text.Show (Show(show))

class CompendiumEntry a where
  named :: a -> Name
  imged :: a -> Img
  typed :: a -> EntryType
  token :: a -> Maybe Token
  items :: a -> [FoundryData]

data FoundryData
  = FoundryAbility   Ability
  | FoundryArmor     Armor
 -- | FoundryBestiary  Bestiary
 -- | FoundryCharacter Character
  | FoundryEquipment Equipment
  | FoundryFlood     Flood
 -- | FoundryVehicle   Vehicle
  | FoundryWeapon    Weapon

instance CompendiumEntry FoundryData where
  named (FoundryAbility   a) = named a
  named (FoundryArmor     a) = named a
--  named (FoundryBestiary  b) = named b
--  named (FoundryCharacter c) = named c
  named (FoundryEquipment e) = named e
  named (FoundryFlood     f) = named f
--  named (FoundryVehicle   v) = named v
  named (FoundryWeapon    w) = named w

  imged (FoundryAbility   a) = imged a
  imged (FoundryArmor     a) = imged a
--  imged (FoundryBestiary  b) = imged b
--  imged (FoundryCharacter c) = imged c
  imged (FoundryEquipment e) = imged e
  imged (FoundryFlood     f) = imged f
--  imged (FoundryVehicle   v) = imged v
  imged (FoundryWeapon    w) = imged w

  typed (FoundryAbility   a) = typed a
  typed (FoundryArmor     a) = typed a
--  typed (FoundryBestiary  b) = typed b
--  typed (FoundryCharacter c) = typed c
  typed (FoundryEquipment e) = typed e
  typed (FoundryFlood     f) = typed f
--  typed (FoundryVehicle   v) = typed v
  typed (FoundryWeapon    w) = typed w

  token (FoundryAbility   a) = token a
  token (FoundryArmor     a) = token a
--  token (FoundryBestiary  b) = token b
--  token (FoundryCharacter c) = token c
  token (FoundryEquipment e) = token e
  token (FoundryFlood     f) = token f
--  token (FoundryVehicle   v) = token v
  token (FoundryWeapon    w) = token w

  items (FoundryAbility   a) = items a
  items (FoundryArmor     a) = items a
--  items (FoundryBestiary  b) = items b
--  items (FoundryCharacter c) = items c
  items (FoundryEquipment e) = items e
  items (FoundryFlood     f) = items f
--  items (FoundryVehicle   v) = items v
  items (FoundryWeapon    w) = items w

instance Show FoundryData where
  show (FoundryAbility   a) = show a
  show (FoundryArmor     a) = show a
--  show (FoundryBestiary  b) = show b
--  show (FoundryCharacter c) = show c
  show (FoundryEquipment e) = show e
  show (FoundryFlood     f) = show f
--  show (FoundryVehicle   v) = show v
  show (FoundryWeapon    w) = show w

instance ToJSON FoundryData where
  toJSON (FoundryAbility   a) = toJSON a
  toJSON (FoundryArmor     a) = toJSON a
--  toJSON (FoundryBestiary  b) = toJSON b
--  toJSON (FoundryCharacter c) = toJSON c
  toJSON (FoundryEquipment e) = toJSON e
  toJSON (FoundryFlood     f) = toJSON f
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
    } deriving stock (Show)

instance CompendiumEntry Ability where
  named = abilityName
  imged = const abilityImg
  typed = const (FoundryItem ItemAbility)
  token = const Nothing
  items = const []

instance ToJSON Ability where
  toJSON a =
    object [ "prerequisite" .= abilityPrereqs     a
           , "cost"         .= abilityCost        a
           , "summary"      .= abilitySummary     a
           , "description"  .= abilityDescription a
           , "type"         .= abilityType        a
           ]

abilityImg :: Img
abilityImg = mkImg "icons/skills/targeting/crosshair-pointed-orange.webp"

data Armor =
  Armor
    { armorName        :: Name
    , armorVariant     :: Maybe Name
    , armorFaction     :: Faction
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
    } deriving stock (Show)

instance CompendiumEntry Armor where
  named = armorName
  imged = const armorImg
  typed = const (FoundryItem ItemArmor)
  token = const Nothing
  items = const []

instance ToJSON Armor where
  toJSON a =
    object [ "faction"         .= armorFaction a
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
armorImg = mkImg "icons/equipment/chest/breastplate-layered-leather-green.webp"

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
    } deriving stock (Show)

instance CompendiumEntry Equipment where
  named = equipmentName
  imged = const equipmentImg
  typed = const (FoundryItem ItemEquipment)
  token = const Nothing
  items = const []

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

data Flood =
  Flood
    { floodName                  :: Name
    , floodCharacteristics       :: Characteristics
    , floodMythicCharacteristics :: MythicCharacteristics
    , floodWounds                :: FloodWounds
    , floodSize                  :: Size
    , floodNotes                 :: Maybe T.Text
    , floodContamination         :: Contamination
    , floodSwarm                 :: Swarm
    , floodArmor                 :: CharacterArmor
    , floodShields               :: CharacterShields
    , floodSkills                :: Skills
    , floodTrainings             :: Trainings
    , floodExperience            :: ExperiencePayout
    , floodItems                 :: [FoundryData]
    } deriving stock (Show)

instance CompendiumEntry Flood where
  named = floodName
  imged = const floodImg
  typed = const (FoundryActor ActorFlood)
  token = Just . mkFloodToken
  items = floodItems

instance ToJSON Flood where
  toJSON f =
    object
      [ "weight"                .= valueInt 0
      , "experiencePayout"      .= floodExperience f
      , "characteristics"       .= floodCharacteristics f
      , "mythicCharacteristics" .= floodMythicCharacteristics f
      , "perceptiveRange"       .=
          object
            [ "base"  .= valueInt 0
            , "mod"   .= valueInt 0
            , "total" .= valueInt 0
            , "vigil" .= toJSON False
            ]
      , "wounds"        .= floodWounds f
      , "size"          .= floodSize f
      , "notes"         .= fromMaybe T.empty (floodNotes f)
      , "contamination" .= floodContamination f
      , "swarm"         .= floodSwarm f
      , "armor"         .= floodArmor f
      , "shields"       .= floodShields f
      , "movement" .=
          object
            [ "agiBonusRunCharge" .= valueInt 0
            , "jumpMultiplier"    .= valueInt 1
            , "leapAgiBonus"      .= valueInt 0
            , "leapMultiplier"    .= valueInt 1
            , "rush"              .= toJSON False
            , "blur"              .= toJSON False
            , "half"              .= valueInt 0
            , "full"              .= valueInt 0
            , "charge"            .= valueInt 0
            , "run"               .= valueInt 0
            , "sprint"            .= valueText "--"
            , "jump"              .= valueInt 0
            , "leap"              .= valueInt 0
            ]
      , "initiative" .=
          object
            [ "formula" .= T.empty
            , "mods"    .= T.empty
            ]
      , "skills"           .= floodSkills f
      , "trainings"        .= floodTrainings f
      , "carryingCapacity" .=
          object
            [ "doubleStr" .= toJSON False
            , "doubleTou" .= toJSON False
            , "strongBack" .= toJSON False
            , "mod" .= valueInt 0
            , "carry" .= valueInt 0
            , "lift" .= valueInt 0
            , "push" .= valueInt 0
            , "felt" .= valueInt 0
            , "total" .= valueInt 0
            , "character" .= valueInt 0
            , "hearingPenalty" .= toJSON False
            , "bar" .=
                object
                  [ "bgBar"  .= T.empty
                  , "bgFill" .= T.empty
                  , "width"  .= T.empty
                  , "left"   .= T.empty
                  , "tier"   .= valueText "carry"
                  ]
            ]
      ]

floodImg :: Img
floodImg = mkImg "icons/magic/death/undead-zombie-grave-green.webp"

mkFloodToken :: Flood -> Token
mkFloodToken flood =
  Token
    { tokenName = floodName flood
    , tokenType = ActorFlood
    , tokenSize = floodSize flood
    , tokenBar2 = Nothing
    }

data Weapon =
  Weapon
    { weaponName            :: Name
    , weaponFaction         :: Maybe Faction
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
    } deriving stock (Show)

instance CompendiumEntry Weapon where
  named = weaponName
  imged = weaponImg
  typed = const (FoundryItem ItemWeapon)
  token = const Nothing
  items = const []

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
           , "ammoGroup"          .= weaponAmmoGroup w
           , "scopeMagnification" .= weaponScopeMag w
           , "scopeMinimum"       .= valueRatio 1
           , "owner"              .= nullJSON
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
