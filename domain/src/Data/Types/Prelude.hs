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
  , Protection
  , Reload
  , Shields
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
import           GHC.Types (Double)

import qualified Data.Map as Map

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

data ArmorNotes =
  ArmorNotes
    { armorNotes             :: Text
    , armorNotesVariant      :: Text
    , armorNotesPermutations :: Text
    , armorNotesOther        :: Text
    }

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

newtype Description = Description Text

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

data Faction
  = UNSC
  | Covenant
  | Forerunner

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

data Hardpoints =
  Hardpoints
    { hardpointsHead     :: Int
    , hardpointsChest    :: Int
    , hardpointsLeftArm  :: Int
    , hardpointsRightArm :: Int
    , hardpointsLeftLeg  :: Int
    , hardpointsRightLeg :: Int
    }

data ItemPrice =
  ItemPrice
    { priceBase  :: Int
    , priceMods  :: Int
    , priceTotal :: Int
    }

data ItemTrainings =
  ItemTrainings
    { itemTrainingsEquipment :: Maybe EquipmentTraining
    , itemTrainingsFaction   :: Faction
    }

newtype MagazineCapacity = MagazineCapacity Int

newtype Name = Name Text

data Protection =
  Protection
    { protectionHead     :: ArmorAdjustment
    , protectionChest    :: ArmorAdjustment
    , protectionLeftArm  :: ArmorAdjustment
    , protectionRightArm :: ArmorAdjustment
    , protectionLeftLeg  :: ArmorAdjustment
    , protectionRightLeg :: ArmorAdjustment
    }

newtype Reload = Reload Int

newtype ScopeMagnification = ScopeMagnification Int

data Shields =
  Shields
    { shieldsHas       :: Bool
    , shieldsIntegrity :: ArmorAdjustment
    , shieldsRecharge  :: ArmorAdjustment
    , shieldsDelay     :: ArmorAdjustment
    }

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
    , statAdjustmentsSTR       :: ArmorAdjustment
    , statAdjustmentsAGI       :: ArmorAdjustment
    , statAdjustmentsMythicSTR :: ArmorAdjustment
    , statAdjustmentsMythicAGI :: ArmorAdjustment
    }

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
    { weightTotal         :: Double
    , weightFelt          :: Double
    , weightEach          :: Double
    , weightQuantity      :: Int
    , weightCarried       :: Bool
    , weightEquipped      :: Bool
    , weightSelfSupported :: Bool
    }

newtype WeaponType = WeaponType Text
