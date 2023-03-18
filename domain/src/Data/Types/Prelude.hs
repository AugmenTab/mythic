module Data.Types.Prelude
  ( -- Data Types
    AmmoGroup(..)
  , AmmoList
  , Ammunition
  , ArmorNotes
  , Attack
  , Barrel
  , EquipmentTraining
  , Faction(..), factionText
  , FactionTraining
  , FirearmType
  , FireMode(..)
  , FireModes, fireModes
  , Hardpoints
  , ItemAdjustment, emptyItemAdjustment, basicItemAdjustment
  , ItemPrice, mkItemPrice
  , ItemTrainings, mkItemTrainings
  , ItemType(..)
  , Protection
  , Shields(..), emptyShields
  , Size
  , SpecialRules
  , StatAdjustments, emptyStatAdjustments
  , WeaponGroup(..)
  , WeaponRange
  , WeaponSettings
  , WeaponTag
  , WeaponTags
  , Weight(..)

    -- Newtypes
  , Ammo
  , Breakpoints, mkBreakpoints
  , CompendiumDetails, compendiumDetails, mkCompendiumDetails
  , Description, mkDescription
  , FireRate
  , Img, mkImg
  , MagazineCapacity
  , Name, mkName, nameText
  , Reload
  , ScopeMagnification
  , WeaponType

  -- Type Aliases
  , CompendiumData
  , CompendiumMap

  -- Type Classes
  , CompendiumEntry(..)
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

newtype Ammo = Ammo T.Text
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

data ArmorNotes =
  ArmorNotes
    { armorNotesVariant      :: T.Text
    , armorNotesPermutations :: T.Text
    , armorNotesOther        :: T.Text
    }

instance ToJSON ArmorNotes where
  toJSON a =
    object [ "armor"        .= defaultArmorNotes
           , "variant"      .= armorNotesVariant a
           , "permutations" .= armorNotesPermutations a
           , "other"        .= armorNotesOther a
           ]

defaultArmorNotes :: T.Text
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

attackFireModeText :: Attack -> T.Text
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

barrelText :: Barrel -> T.Text
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

mkBreakpoints :: Int -> Breakpoints
mkBreakpoints = Breakpoints

newtype CompendiumDetails = CompendiumDetails T.Text
  deriving newtype (Eq, Ord, Show)

mkCompendiumDetails :: T.Text -> CompendiumDetails
mkCompendiumDetails = CompendiumDetails . T.toUpper

compendiumDetails :: CompendiumDetails -> T.Text
compendiumDetails (CompendiumDetails t) = t

type CompendiumData = (Faction, CompendiumDetails)
type CompendiumMap entries = Map.Map CompendiumData entries

class CompendiumEntry a where
  named :: a -> Name
  imged :: a -> Img
  typed :: a -> ItemType

newtype Description = Description T.Text
  deriving newtype (ToJSON)

mkDescription :: T.Text -> Description
mkDescription = Description

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

equipmentTrainingText :: EquipmentTraining -> T.Text
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
  | Banished
  | Forerunner
  deriving stock (Eq, Ord, Show)

factionText :: Faction -> T.Text
factionText faction =
  case faction of
    UNSC       -> "UNSC"
    Covenant   -> "Covenant"
    Banished   -> "Banished"
    Forerunner -> "Forerunner"

factionTrainingFor :: Faction -> FactionTraining
factionTrainingFor faction =
  case faction of
    UNSC       -> UNSCTraining
    Covenant   -> CovenantTraining
    Banished   -> CovenantTraining
    Forerunner -> ForerunnerTraining

data FactionTraining
  = UNSCTraining
  | CovenantTraining
  | ForerunnerTraining

instance ToJSON FactionTraining where
  toJSON = toJSON . factionTrainingText

factionTrainingText :: FactionTraining -> T.Text
factionTrainingText faction =
  case faction of
    UNSCTraining       -> "unsc"
    CovenantTraining   -> "covenant"
    ForerunnerTraining -> "forerunner"

data FirearmType
  = Firearms
  | Cannons
  | Shotguns

instance ToJSON FirearmType where
  toJSON = toJSON . firearmTypeText

firearmTypeText :: FirearmType -> T.Text
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

fireModeText :: FireMode -> T.Text
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

fireModes :: FireModes -> Map.Map FireMode FireRate
fireModes (FireModes f) = f

newtype FireRate = FireRate Int
  deriving newtype (Show, ToJSON)

data Hardpoints

instance ToJSON Hardpoints where
  toJSON _ =
    object [ "head"     .= valueInt 0
           , "chest"    .= valueInt 0
           , "leftArm"  .= valueInt 0
           , "rightArm" .= valueInt 0
           , "leftLeg"  .= valueInt 0
           , "rightLeg" .= valueInt 0
           ]

newtype Img = Img T.Text
  deriving newtype (ToJSON)

mkImg :: T.Text -> Img
mkImg = Img

data ItemAdjustment =
  ItemAdjustment
    { itemAdjustment        :: Int
    , itemAdjustmentVariant :: Int
    , itemAdjustmentOther   :: Int
    , itemAdjustmentTotal   :: Int
    }

instance ToJSON ItemAdjustment where
  toJSON a =
    object [ "item"    .= itemAdjustment a
           , "variant" .= itemAdjustmentVariant a
           , "other"   .= itemAdjustmentOther a
           , "total"   .= itemAdjustmentTotal a
           ]

emptyItemAdjustment :: ItemAdjustment
emptyItemAdjustment = basicItemAdjustment 0

basicItemAdjustment :: Int -> ItemAdjustment
basicItemAdjustment val =
  ItemAdjustment
    { itemAdjustment        = val
    , itemAdjustmentVariant = 0
    , itemAdjustmentOther   = 0
    , itemAdjustmentTotal   = val
    }

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

mkItemPrice :: Int -> ItemPrice
mkItemPrice n =
  ItemPrice
    { priceBase  = n
    , priceMods  = 0
    , priceTotal = n
    }

data ItemTrainings =
  ItemTrainings
    { itemTrainingsEquipment :: Maybe EquipmentTraining
    , itemTrainingsFaction   :: FactionTraining
    }

instance ToJSON ItemTrainings where
  toJSON t =
    let defTraining = maybe "" equipmentTrainingText $ itemTrainingsEquipment t
     in object [ "equipment" .= defTraining
               , "faction"   .= itemTrainingsFaction t
               ]

mkItemTrainings :: Faction -> Maybe EquipmentTraining -> ItemTrainings
mkItemTrainings faction mbEquipmentTraining =
  ItemTrainings mbEquipmentTraining $ factionTrainingFor faction

data ItemType
  = ItemAbility
  | ItemArmor
  | ItemEducation
  | ItemEquipment
  | ItemWeapon

instance ToJSON ItemType where
  toJSON = toJSON . itemTypeText

itemTypeText :: ItemType -> T.Text
itemTypeText item =
  case item of
    ItemAbility   -> "ability"
    ItemArmor     -> "armor"
    ItemEducation -> "education"
    ItemEquipment -> "equipment"
    ItemWeapon    -> "weapon"

newtype MagazineCapacity = MagazineCapacity Int
  deriving newtype (ToJSON)

newtype Name = Name T.Text
  deriving newtype (Eq, Ord, ToJSON)

mkName :: T.Text -> Name
mkName = Name

nameText :: Name -> T.Text
nameText (Name t) = t

data Protection =
  Protection
    { protectionHead     :: ItemAdjustment
    , protectionChest    :: ItemAdjustment
    , protectionLeftArm  :: ItemAdjustment
    , protectionRightArm :: ItemAdjustment
    , protectionLeftLeg  :: ItemAdjustment
    , protectionRightLeg :: ItemAdjustment
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
    , shieldsIntegrity :: ItemAdjustment
    , shieldsRecharge  :: ItemAdjustment
    , shieldsDelay     :: ItemAdjustment
    }

instance ToJSON Shields where
  toJSON s =
    object [ "has"       .= shieldsHas s
           , "integrity" .= shieldsIntegrity s
           , "recharge"  .= shieldsRecharge s
           , "delay"     .= shieldsDelay s
           ]

emptyShields :: Shields
emptyShields =
  Shields
    { shieldsHas       = False
    , shieldsIntegrity = emptyItemAdjustment
    , shieldsRecharge  = emptyItemAdjustment
    , shieldsDelay     = emptyItemAdjustment
    }

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

sizeText :: Size -> T.Text
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
    , cryo             :: Maybe T.Text
    , diceMinimum      :: Maybe Int
    , electrified      :: Maybe Int
    , emp              :: Maybe Int
    , flame            :: Maybe T.Text
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
               , "spike"            .= noneRule (spike r)
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
    , statAdjustmentsSTR       :: ItemAdjustment
    , statAdjustmentsAGI       :: ItemAdjustment
    , statAdjustmentsMythicSTR :: ItemAdjustment
    , statAdjustmentsMythicAGI :: ItemAdjustment
    }

instance ToJSON StatAdjustments where
  toJSON s =
    object [ "has"       .= statAdjustmentsHas s
           , "str"       .= statAdjustmentsSTR s
           , "agi"       .= statAdjustmentsAGI s
           , "mythicStr" .= statAdjustmentsMythicSTR s
           , "mythicAgi" .= statAdjustmentsMythicAGI s
           ]

emptyStatAdjustments :: StatAdjustments
emptyStatAdjustments =
  StatAdjustments
    { statAdjustmentsHas       = False
    , statAdjustmentsSTR       = emptyItemAdjustment
    , statAdjustmentsAGI       = emptyItemAdjustment
    , statAdjustmentsMythicSTR = emptyItemAdjustment
    , statAdjustmentsMythicAGI = emptyItemAdjustment
    }

data WeaponGroup
  = Ranged
  | MeleeGroup
  | Thrown

instance ToJSON WeaponGroup where
  toJSON = toJSON . weaponGroupText

weaponGroupText :: WeaponGroup -> T.Text
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

newtype WeaponTags = WeaponTags (Set.Set WeaponTag)

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

newtype WeaponType = WeaponType T.Text
  deriving newtype (ToJSON)
