module Data.Types.Prelude
  ( -- Data Types
    AmmoGroup
  , AmmoList
  , Ammunition
  , ArmorAdjustment
  , ArmorNotes
  , Attack
  , Barrel
  , EquipmentTraining
  , Faction
  , FirearmType
  , FireMode
  , FireModes
  , Hardpoints
  , ItemPrice
  , ItemTrainings
  , ItemType
  , Protection
  , Reload
  , Shields
  , Size
  , SpecialRules
  , StatAdjustments
  , WeaponGroup
  , WeaponRange
  , WeaponSettings
  , WeaponTag
  , WeaponTags
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

import           Data.Coerce (coerce)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import qualified Data.Text as T
import           GHC.Types (Double)
import           Text.Show (Show, show)

newtype Ammo = Ammo Text
  deriving newtype (ToJSON)

data AmmoGroup
  = None
  | STD
  | Shotgun
  | Flamethrower
  | Sniper
  | Grenade
  | MRC
  | BruteShot

newtype AmmoList = AmmoList [Ammunition]

instance ToJSON AmmoList where
  toJSON (AmmoList l) =
    object $ (\a -> (keyFromText . coerce $ ammunitionName a) .= a) <$> l

data Ammunition =
  Ammunition
    { ammunitionName         :: Name
    , ammunitionAttackBonus  :: Int
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
    , ammunitionSpecials     :: SpecialRules
    }

instance ToJSON Ammunition where
  toJSON a =
    let emptyAmmoTracking =
          object [ "pool" .= valueInt 0
                 , "mags" .= valueInt 0
                 ]

     in object [ "attackBonus"  .= ammunitionAttackBonus a
               , "diceQuantity" .= ammunitionDiceQuantity a
               , "diceValue"    .= ammunitionDiceValue a
               , "baseDamage"   .= ammunitionBaseDamage a
               , "strDamage"    .= ammunitionSTRDamage a
               , "piercing"     .= ammunitionPiercing a
               , "strPiercing"  .= ammunitionSTRPiercing a
               , "target"       .= ammunitionTarget a
               , "currentMag"   .= ammunitionCurrentMag a
               , "critsOn"      .= ammunitionCritsOn a
               , "ammoTracking" .= emptyAmmoTracking
               , "range"        .= ammunitionRange a
               , "desc"         .= ammunitionDescription a
               , "special"      .= ammunitionSpecials a
               ]

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

instance ToJSON Attack where
  toJSON a =
    object [ "fireMode"    .= attackFireModeText a
           , "half"        .= attackHalf a
           , "full"        .= attackFull a
           , "attackBonus" .= attackBonus a
           ]

attackFireModeText :: Attack -> Text
attackFireModeText atk =
  T.concat [ fireModeText $ attackFireMode atk
           , "-"
           , T.pack . show $ attackFireRate atk
           ]

data Barrel
  = XS
  | S
  | M
  | L
  | XL
  | XXL

instance ToJSON Barrel where
  toJSON = toJSON . barrelText

barrelText :: Barrel -> Text
barrelText barrel =
  case barrel of
    XS  -> "xs"
    S   -> "s"
    M   -> "m"
    L   -> "l"
    XL  -> "xl"
    XXL -> "xxl"

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

instance ToJSON FirearmType where
  toJSON = toJSON . firearmTypeText

firearmTypeText :: FirearmType -> Text
firearmTypeText fType =
  case fType of
    Firearms -> "firearms"
    Cannons  -> "cannons"
    Shotguns -> "shotguns"

data FireMode
  = Auto
  | Burst
  | Charge
  | Drawback
  | Flintlock
  | Pump
  | Semi
  | Sustained
  deriving stock (Eq, Ord)

fireModeText :: FireMode -> Text
fireModeText fm =
  case fm of
    Auto      -> "auto"
    Burst     -> "burst"
    Charge    -> "charge"
    Drawback  -> "drawback"
    Flintlock -> "flintlock"
    Pump      -> "pump"
    Semi      -> "semi"
    Sustained -> "sustained"

newtype FireModes = FireModes (Map.Map FireMode FireRate)

instance ToJSON FireModes where
  toJSON (FireModes f) =
    let lookup = valueInt . maybe 0 coerce . flip Map.lookup f
     in object [ "auto"      .= lookup Auto
               , "burst"     .= lookup Burst
               , "charge"    .= lookup Charge
               , "drawback"  .= lookup Drawback
               , "flintlock" .= lookup Flintlock
               , "pump"      .= lookup Pump
               , "semi"      .= lookup Semi
               , "sustained" .= lookup Sustained
               ]

newtype FireRate = FireRate Int
  deriving newtype (Show, ToJSON)

data Hardpoints

instance ToJSON Hardpoints where
  toJSON h =
    object [ "head"     .= valueInt 0
           , "chest"    .= valueInt 0
           , "leftArm"  .= valueInt 0
           , "rightArm" .= valueInt 0
           , "leftLeg"  .= valueInt 0
           , "rightLeg" .= valueInt 0
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
     in object [ "equipment" .= defTraining
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
  deriving newtype (ToJSON)

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
  deriving newtype (ToJSON)

newtype ScopeMagnification = ScopeMagnification Int
  deriving newtype (ToJSON)

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

data SpecialRules =
  SpecialRules
    { acid             :: Maybe Int
    , blast            :: Maybe Int
    , cauterize        :: Maybe ()
    , chargeRule       :: Maybe Int
    , cryo             :: Maybe Text
    , diceMinimum      :: Maybe Int
    , electrified      :: Maybe Int
    , emp              :: Maybe Int
    , flame            :: Maybe Text
    , flashbang        :: Maybe ()
    , gravimetricPulse :: Maybe Int
    , gravity          :: Maybe Int
    , hardlight        :: Maybe ()
    , headshot         :: Maybe ()
    , homing           :: Maybe Int
    , kill             :: Maybe Int
    , kinetic          :: Maybe ()
    , longBarrel       :: Maybe ()
    , needle           :: Maybe Int
    , nonlethal        :: Maybe ()
    , overheat         :: Maybe Int
    , penetrating      :: Maybe ()
    , rechargeRate     :: Maybe Int
    , singleLoading    :: Maybe ()
    , slow             :: Maybe ()
    , smoke            :: Maybe Int
    , spike            :: Maybe ()
    , spin             :: Maybe Int
    , spread           :: Maybe ()
    , sticky           :: Maybe ()
    , stun             :: Maybe Int
    , tearGas          :: Maybe ()
    , tranquilize      :: Maybe Int
    , vehicleLock      :: Maybe ()
    }

instance ToJSON SpecialRules where
  toJSON r =
    let noneRule mbRule = object [ "has" .= isJust mbRule ]
        intRule mbInt =
          object [ "has"   .= isJust mbInt
                 , "value" .= fromMaybe 0 mbInt
                 ]

        textRule mbText =
          object [ "has"   .= isJust mbText
                 , "value" .= fromMaybe "1D5" mbText
                 ]

     in object [ "acid"             .= intRule  (acid r)
               , "blast"            .= intRule  (blast r)
               , "cauterize"        .= noneRule (cauterize r)
               , "charge"           .= intRule  (chargeRule r)
               , "cryo"             .= textRule (cryo r)
               , "diceMinimum"      .= intRule  (diceMinimum r)
               , "electrified"      .= intRule  (electrified r)
               , "emp"              .= intRule  (emp r)
               , "flame"            .= textRule (flame r)
               , "flashbang"        .= noneRule (flashbang r)
               , "gravimetricPulse" .= intRule  (gravimetricPulse r)
               , "gravity"          .= intRule  (gravity r)
               , "hardlight"        .= noneRule (hardlight r)
               , "headshot"         .= noneRule (headshot r)
               , "homing"           .= intRule  (homing r)
               , "kill"             .= intRule  (kill r)
               , "kinetic"          .= noneRule (kinetic r)
               , "longBarrel"       .= noneRule (longBarrel r)
               , "needle"           .= intRule  (needle r)
               , "nonlethal"        .= noneRule (nonlethal r)
               , "overheat"         .= intRule  (overheat r)
               , "penetrating"      .= noneRule (penetrating r)
               , "rechargeRate"     .= intRule  (rechargeRate r)
               , "singleLoading"    .= noneRule (singleLoading r)
               , "slow"             .= noneRule (slow r)
               , "smoke"            .= intRule  (smoke r)
               , "spin"             .= noneRule (spin r)
               , "spread"           .= noneRule (spread r)
               , "sticky"           .= noneRule (sticky r)
               , "stun"             .= noneRule (stun r)
               , "tearGas"          .= noneRule (tearGas r)
               , "tranquilize"      .= noneRule (tranquilize r)
               , "vehicleLock"      .= noneRule (vehicleLock r)
               ]

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
    let defAdjustments fn = fromMaybe emptyAdjustment $ fn s
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

instance ToJSON WeaponGroup where
  toJSON = toJSON . weaponGroupText

weaponGroupText :: WeaponGroup -> Text
weaponGroupText wg =
  case wg of
    Ranged     -> "ranged"
    MeleeGroup -> "melee"
    Thrown     -> "thrown"

data WeaponRange =
  WeaponRange
    { weaponRangeClose :: Int
    , weaponRangeLong  :: Int
    , weaponRangeMelee :: Int
    }

instance ToJSON WeaponRange where
  toJSON r =
    object [ "close"     .= weaponRangeClose r
           , "long"      .= weaponRangeLong r
           , "melee"     .= weaponRangeMelee r
           , "thrown"    .= valueInt 0
           , "thrownMax" .= valueInt 0
           , "grip"      .= valueText "solid"
           ]

data WeaponSettings =
  WeaponSettings
    { settingsFirearmType    :: FirearmType
    , settingsBarrel         :: Barrel
    , settingsBulletDiameter :: Double
    , settingsCaseLength     :: Double
    , settingsSingleUse      :: Bool
    }

instance ToJSON WeaponSettings where
  toJSON s =
    object [ "firearmType"    .= settingsFirearmType s
           , "barrel"         .= settingsBarrel s
           , "bulletDiameter" .= settingsBulletDiameter s
           , "caseLength"     .= settingsCaseLength s
           , "singleUse"      .= settingsSingleUse s
           ]

data WeaponTag
  = UD
  | I
  | OH
  | TH
  | HW
  | SD
  | PD
  | BD
  deriving stock (Eq, Ord)

newtype WeaponTags = WeaponTags (Set WeaponTag)

instance ToJSON WeaponTags where
  toJSON (WeaponTags t) =
    object [ "[UD]" .= Set.member UD t
           , "[I]"  .= Set.member I  t
           , "[OH]" .= Set.member OH t
           , "[TH]" .= Set.member TH t
           , "[HW]" .= Set.member HW t
           , "[SD]" .= Set.member SD t
           , "[PD]" .= Set.member PD t
           , "[BD]" .= Set.member BD t
           ]

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
           , "quantity"      .= valueInt 1
           , "carried"       .= False
           , "equipped"      .= False
           , "selfSupported" .= weightSelfSupported w
           ]

newtype WeaponType = WeaponType Text
  deriving newtype (ToJSON)
