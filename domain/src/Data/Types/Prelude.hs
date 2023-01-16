module Data.Types.Prelude
  ( -- Data Types
    AmmoGroup
  , Ammunition
  , ArmorAdjustment
  , ArmorNotes
  , Attack
  , Barrel
  , EquipmentTraining
  , Faction
  , FirearmType
  , FireMode
  , Grip
  , Hardpoints
  , ItemPrice
  , ItemTrainings
  , ItemType
  , Protection
  , Reload
  , Shields
  , Size
  , SpecialRule
  , StatAdjustments
  , WeaponGroup
  , WeaponRange
  , WeaponSettings
  , WeaponTag
  , Weight

    -- Newtypes
  , Ammo
  , Breakpoints
  , Description
  , FireRate
  , MagazineCapacity
  , Name
  , Reload
  , ScopeMagnification
  , WeaponType
  ) where

import           Flipstone.Prelude
import           Domain.JSON

import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           GHC.Types (Double)

newtype Ammo = Ammo Text

data AmmoGroup
  = None
  | STD
  | Shotgun
  | Flamethrower
  | Sniper
  | Grenade
  | MRC
  | BruteShot

data Ammunition =
  Ammunition
    { ammunitionAttackBonus  :: Int
    , ammunitionDiceQuantity :: Int
    , ammunitionDiceValue    :: Int
    , ammunitionBaseDamage   :: Int
    , ammunitionSTRDamage    :: Int
    , ammunitionPiercing     :: Int
    , ammunitionSTRPiercing  :: Int
    , ammunitionTarget       :: Int
    , ammunitionCurrentMag   :: Int
    , ammunitionCritsOn      :: Int
    , ammunitionRange        :: WeaponRange
    , ammunitionDescription  :: Description
    , ammunitionSpecials     :: Map.Map Text SpecialRule
    }

data ArmorAdjustment =
  ArmorAdjustment
    { armorAdjustment        :: Int
    , armorAdjustmentVariant :: Int
    , armorAdjustmentOther   :: Int
    , armorAdjustmentTotal   :: Int
    }

instance ToJSON ArmorAdjustment where
  toJSON a =
    object [ "armor"   .= armorAdjustment a
           , "variant" .= armorAdjustmentVariant a
           , "other"   .= armorAdjustmentOther a
           , "total"   .= armorAdjustmentTotal a
           ]

emptyAdjustment :: ArmorAdjustment
emptyAdjustment =
  ArmorAdjustment
    { armorAdjustment        = 0
    , armorAdjustmentVariant = 0
    , armorAdjustmentOther   = 0
    , armorAdjustmentTotal   = 0
    }

data ArmorNotes =
  ArmorNotes
    { armorNotesVariant      :: Text
    , armorNotesPermutations :: Text
    , armorNotesOther        :: Text
    }

instance ToJSON ArmorNotes where
  toJSON a =
    object [ "armor"        .= defaultArmorNotes
           , "variant"      .= armorNotesVariant a
           , "permutations" .= armorNotesPermutations a
           , "other"        .= armorNotesOther a
           ]

defaultArmorNotes :: Text
defaultArmorNotes =
  T.concat [ "<table style=\"width: 98.9267%; margin-left: auto; margin-right: "
           , "auto;\" border=\"1\"><thead><tr style=\"text-align: center;\">"
           , "<td style=\"width: 100%\" colspan=\"2\" scope=\"colgroup\">Suit "
           , "Special Abilities</td></tr></thead><tbody><tr><td style=\"width: "
           , "25%; vertical-align: middle; padding: 4px 8px;\">&nbsp;</td>"
           , "<td style=\"width: 75%; text-align: left; vertical-align: middle;"
           , " padding: 4px 8px;\">&nbsp;</td></tr><tr><td style=\"width: 25%;"
           , " vertical-align: middle; padding: 4px 8px;\"></td>"
           , "<td style=\"width: 75%; text-align: left; vertical-align: middle;"
           ," padding: 4px 8px;\">&nbsp;</td></tr><tr><td style=\"width: 25%; "
           , "vertical-align: middle; padding: 4px 8px;\"></td>"
           , "<td style=\"width: 75%; text-align: left; vertical-align: middle;"
           , "padding: 4px 8px;\">&nbsp;</td></tr></tbody></table>"
            ]

data Attack =
  Attack
    { attackFireMode :: FireMode
    , attackFireRate :: FireRate
    , attackHalf     :: Int
    , attackFull     :: Int
    , attackBonus    :: Int
    }

data Barrel
  = XS
  | S
  | M
  | L
  | XL
  | XXL

newtype Breakpoints = Breakpoints Int
  deriving newtype (ToJSON)

newtype Description = Description Text
  deriving newtype (ToJSON)

data EquipmentTraining
  = Basic
  | Infantry
  | Heavy
  | Advanced
  | Launcher
  | Range
  | Ordnance
  | Cannon
  | Melee

instance ToJSON EquipmentTraining where
  toJSON = toJSON . equipmentTrainingText

equipmentTrainingText :: EquipmentTraining -> Text
equipmentTrainingText eqTraining =
  case eqTraining of
    Basic     -> "basic"
    Infantry  -> "infantry"
    Heavy     -> "heavy"
    Advanced  -> "advanced"
    Launcher  -> "launcher"
    Range     -> "range"
    Ordnance  -> "ordnance"
    Cannon    -> "cannon"
    Melee     -> "melee"

data Faction
  = UNSC
  | Covenant
  | Forerunner

instance ToJSON Faction where
  toJSON = toJSON . factionText

factionText :: Faction -> Text
factionText faction =
  case faction of
    UNSC       -> "unsc"
    Covenant   -> "covenant"
    Forerunner -> "forerunner"

data FirearmType
  = Firearms
  | Cannons
  | Shotguns

data FireMode
  = Auto
  | Burst
  | Charge
  | Drawback
  | Flintlock
  | Pump
  | Semi
  | Sustained

newtype FireRate = FireRate Int

data Grip
  = Solid
  | Slight
  | Partial
  | Sloppy

data Hardpoints

instance ToJSON Hardpoints where
  toJSON h =
    object [ "head"     .= defaultInt 0
           , "chest"    .= defaultInt 0
           , "leftArm"  .= defaultInt 0
           , "rightArm" .= defaultInt 0
           , "leftLeg"  .= defaultInt 0
           , "rightLeg" .= defaultInt 0
           ]

data ItemPrice =
  ItemPrice
    { priceBase  :: Int
    , priceMods  :: Int
    , priceTotal :: Int
    }

instance ToJSON ItemPrice where
  toJSON p =
    object [ "base"  .= priceBase p
           , "mods"  .= priceMods p
           , "total" .= priceTotal p
           ]

data ItemTrainings =
  ItemTrainings
    { itemTrainingsEquipment :: Maybe EquipmentTraining
    , itemTrainingsFaction   :: Faction
    }

instance ToJSON ItemTrainings where
  toJSON t =
    let defTraining = maybe "" equipmentTrainingText $ itemTrainingsEquipment t
     in object [ "equipment" .= toJSON defTraining
               , "faction"   .= itemTrainingsFaction t
               ]

data ItemType
  = ItemAbility
  | ItemArmor
  | ItemEducation
  | ItemEquipment
  | ItemWeapon

instance ToJSON ItemType where
  toJSON = toJSON . itemTypeText

itemTypeText :: ItemType -> Text
itemTypeText item =
  case item of
    ItemAbility   -> "ability"
    ItemArmor     -> "armor"
    ItemEducation -> "education"
    ItemEquipment -> "equipment"
    ItemWeapon    -> "weapon"

newtype MagazineCapacity = MagazineCapacity Int

newtype Name = Name Text
  deriving newtype (ToJSON)

data Protection =
  Protection
    { protectionHead     :: ArmorAdjustment
    , protectionChest    :: ArmorAdjustment
    , protectionLeftArm  :: ArmorAdjustment
    , protectionRightArm :: ArmorAdjustment
    , protectionLeftLeg  :: ArmorAdjustment
    , protectionRightLeg :: ArmorAdjustment
    }

instance ToJSON Protection where
  toJSON p =
    object [ "head"     .= protectionHead p
           , "chest"    .= protectionChest p
           , "leftArm"  .= protectionLeftArm p
           , "rightArm" .= protectionRightArm p
           , "leftLeg"  .= protectionLeftLeg p
           , "rightLeg" .= protectionRightLeg p
           ]

newtype Reload = Reload Int

newtype ScopeMagnification = ScopeMagnification Int

data Shields =
  Shields
    { shieldsHas       :: Bool
    , shieldsIntegrity :: ArmorAdjustment
    , shieldsRecharge  :: ArmorAdjustment
    , shieldsDelay     :: ArmorAdjustment
    }

instance ToJSON Shields where
  toJSON s =
    object [ "has"       .= shieldsHas s
           , "integrity" .= shieldsIntegrity s
           , "recharge"  .= shieldsRecharge s
           , "delay"     .= shieldsDelay s
           ]

data Size
  = Mini
  | Small
  | Normal
  | Large
  | Huge
  | Hulking
  | Giant
  | Immense
  | Massive
  | Great
  | Monumental
  | Colossal
  | Vast

instance ToJSON Size where
  toJSON = toJSON . sizeText

sizeText :: Size -> Text
sizeText size =
  case size of
    Mini       -> "mini"
    Small      -> "small"
    Normal     -> "normal"
    Large      -> "large"
    Huge       -> "huge"
    Hulking    -> "hulking"
    Giant      -> "giant"
    Immense    -> "immense"
    Massive    -> "massive"
    Great      -> "great"
    Monumental -> "monumental"
    Colossal   -> "colossal"
    Vast       -> "vast"

data SpecialRule
  = Acid             Int
  | Blast            Int
  | Cauterize
  | ChargeRule       Int
  | Cryo             Text
  | DiceMinimum      Int
  | Electrified      Int
  | EMP              Int
  | Flame            Text
  | Flashbang
  | GravimetricPulse Int
  | Gravity          Int
  | Hardlight
  | Headshot
  | Homing           Int
  | Kill             Int
  | Kinetic
  | LongBarrel
  | Needle           Int
  | Nonlethal
  | Overheat         Int
  | Penetrating
  | RechargeRate     Int
  | SingleLoading
  | Slow
  | Smoke            Int
  | Spike
  | Spin             Int
  | Spread
  | Sticky
  | Stun             Int
  | TearGas
  | Tranquilize      Int
  | VehicleLock

data StatAdjustments =
  StatAdjustments
    { statAdjustmentsHas       :: Bool
    , statAdjustmentsSTR       :: Maybe ArmorAdjustment
    , statAdjustmentsAGI       :: Maybe ArmorAdjustment
    , statAdjustmentsMythicSTR :: Maybe ArmorAdjustment
    , statAdjustmentsMythicAGI :: Maybe ArmorAdjustment
    }

instance ToJSON StatAdjustments where
  toJSON s =
    let defAdjustments fn = toJSON . fromMaybe emptyAdjustment $ fn s
     in object [ "has"       .= statAdjustmentsHas s
               , "str"       .= defAdjustments statAdjustmentsSTR
               , "agi"       .= defAdjustments statAdjustmentsAGI
               , "mythicStr" .= defAdjustments statAdjustmentsMythicSTR
               , "mythicAgi" .= defAdjustments statAdjustmentsMythicAGI
               ]

data WeaponGroup
  = Ranged
  | MeleeGroup
  | Thrown

data WeaponRange =
  WeaponRange
    { weaponRangeClose :: Int
    , weaponRangeLong  :: Int
    , weaponRangeMelee :: Int
    , weaponRangeGrip  :: Grip
    }

data WeaponSettings =
  WeaponSettings
    { settingsFirearmType    :: FirearmType
    , settingsBarrel         :: Barrel
    , settingsBulletDiameter :: Double
    , settingsCaseLength     :: Double
    , settingsSingleUse      :: Bool
    }

data WeaponTag
  = UD
  | I
  | OH
  | TH
  | HW
  | SD
  | PD
  | BD

data Weight =
  Weight
    { weightEach          :: Double
    , weightSelfSupported :: Bool
    }

instance ToJSON Weight where
  toJSON w =
    object [ "total"         .= weightEach w
           , "felt"          .= if weightSelfSupported w
                                   then weightEach w
                                   else 0
           , "each"          .= weightEach w
           , "quantity"      .= defaultInt 1
           , "carried"       .= toJSON False
           , "equipped"      .= toJSON False
           , "selfSupported" .= weightSelfSupported w
           ]

newtype WeaponType = WeaponType Text
