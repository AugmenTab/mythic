module Data.Types.Prelude
  ( -- Data Types
    AbilityType(..)
  , ActorType(..)
  , AmmoGroup(..)
  , AmmoList, mkAmmoList
  , Ammunition(..)
  , ArmorNotes(..)
  , Attack, emptyAttack
  , Barrel
  , CarryingCapacity(..)
  , CharacterArmor, emptyCharacterArmor
  , Characteristic, mkCharacteristic
  , Characteristics(..)
  , Characteristics_Flood(..)
  , CharacterShields, emptyCharacterShields, hasShields, mkCharacterShields
  , Difficulty(..)
  , EntryType(..), entryTypeText
  , EquipmentTraining(..), allEquipmentTrainings
  , ExperienceDifficulty, emptyExperienceDifficulty, mkExperienceDifficulty
  , ExperiencePayout(..)
  , Faction(..), factions, compendiumFactionFromText, factionFromText, factionText
  , FactionTraining
  , FirearmType
  , FireMode(..), fireModeFromText
  , FireModes, fireModes, mkFireModes
  , Hardpoints, emptyHardpoints
  , ItemAdjustment(..), emptyItemAdjustment, basicItemAdjustment
  , ItemPrice, mkItemPrice
  , ItemTrainings, mkItemTrainings
  , ItemType(..)
  , Luck, mkLuck
  , Movement(..)
  , MythicCharacteristics(..)
  , MythicCharacteristics_Flood(..)
  , Protection(..)
  , Shields(..), emptyShields
  , Size(..), sizeFromText
  , SpecialRules_Vehicle(..), emptyVehicleSpecialRules
  , SpecialRules_Weapon(..), emptyWeaponSpecialRules
  , StatAdjustments(..), emptyStatAdjustments
  , StrengthMultiplier(..), strengthMultiplierFromText
  , Swarm, mkSwarm
  , Token(..)
  , Trainings(..), emptyTrainings
  , WeaponGroup(..)
  , WeaponRange(..), emptyWeaponRange
  , WeaponSettings, emptyWeaponSettings
  , WeaponTag(..), weaponTagFromText
  , WeaponTags(..)
  , WeaponTrainings, emptyWeaponTrainings
  , Weight(..), emptyWeight
  , Wounds(..)

    -- Newtypes
  , Ammo, mkAmmo
  , ArmorAdjustment, mkArmorAdjustment
  , Breakpoints, mkBreakpoints
  , CompendiumDetails, compendiumDetails, mkCompendiumDetails
  , Contamination(..)
  , Description, mkDescription
  , FireRate, mkFireRate
  , FloodWounds(..)
  , Img, mkImg
  , MagazineCapacity, mkMagazineCapacity
  , Name, mkName, nameText
  , Prerequisites, mkPrereqs
  , Reload, mkReload
  , ScopeMagnification, mkScopeMagnification
  , Skills, characterSkillList, floodSkillList
  , WeaponType(..)

  -- Type Aliases
  , CompendiumData
  , CompendiumMap
  , DifficultyTierValuesWithSingleLevelFlag
  ) where

import           Flipstone.Prelude
import           Domain.JSON

import qualified Data.Bool as B
import           Data.Coerce (coerce)
import qualified Data.List.Extra as L
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, isJust)
import           Data.Monoid (mappend)
import           Data.Ratio ((%))
import qualified Data.Set as Set
import qualified Data.Text as T
import           GHC.Types (Double)
import           Text.Show (Show, show)

data AbilityType
  = TrueAbility
  | RacialTrait
  | Trait
  | Augmentation

instance ToJSON AbilityType where
  toJSON = toJSON . abilityTypeToText

abilityTypeToText :: AbilityType -> T.Text
abilityTypeToText at =
  case at of
    TrueAbility  -> "ability"
    RacialTrait  -> "racial"
    Trait        -> "trait"
    Augmentation -> "augmentation"

data ActorType
  = ActorBestiary
  | ActorFlood
  | ActorNamed
  | ActorVehicle
  deriving stock (Eq)

instance ToJSON ActorType where
  toJSON = toJSON . actorTypeText

actorTypeText :: ActorType -> T.Text
actorTypeText actor =
  case actor of
    ActorBestiary -> "Bestiary Character"
    ActorFlood    -> "Flood"
    ActorNamed    -> "Named Character"
    ActorVehicle  -> "Vehicle"

newtype Ammo = Ammo T.Text
  deriving newtype (ToJSON)

mkAmmo :: T.Text -> Ammo
mkAmmo = Ammo

data AmmoGroup
  = None
  | STD
  | Shotgun
  | Flamethrower
  | Sniper
  | Grenade
  | MRC
  | BruteShot

instance ToJSON AmmoGroup where
  toJSON = toJSON . ammoGroupToText

ammoGroupToText :: AmmoGroup -> T.Text
ammoGroupToText ag =
  case ag of
    None         -> "none"
    STD          -> "std"
    Shotgun      -> "shotgun"
    Flamethrower -> "flamethrower"
    Sniper       -> "sniper"
    Grenade      -> "grenade"
    MRC          -> "mrc"
    BruteShot    -> "bruteShot"

newtype AmmoList = AmmoList [Ammunition]

instance ToJSON AmmoList where
  toJSON (AmmoList l) =
    object $ (\a -> (keyFromText . coerce $ ammunitionName a) .= a) <$> l

mkAmmoList :: Ammunition -> AmmoList
mkAmmoList = AmmoList . L.snoc []

data Ammunition =
  Ammunition
    { ammunitionName         :: Name
    , ammunitionAttackBonus  :: Int
    , ammunitionDiceQuantity :: Int
    , ammunitionDiceValue    :: Int
    , ammunitionBaseDamage   :: Int
    , ammunitionSTRDamage    :: Maybe StrengthMultiplier
    , ammunitionPiercing     :: Int
    , ammunitionSTRPiercing  :: Maybe StrengthMultiplier
    , ammunitionTarget       :: Int
    , ammunitionCurrentMag   :: Int
    , ammunitionCritsOn      :: Int
    , ammunitionRange        :: WeaponRange
    , ammunitionDescription  :: Description
    , ammunitionSpecials     :: SpecialRules_Weapon
    }

instance ToJSON Ammunition where
  toJSON a =
    let strMultFrom fn = maybe 0 strengthMultiplier $ fn a
        emptyAmmoTracking =
          object [ "pool" .= valueInt 0
                 , "mags" .= valueInt 0
                 ]

     in object [ "attackBonus"  .= ammunitionAttackBonus a
               , "diceQuantity" .= ammunitionDiceQuantity a
               , "diceValue"    .= ammunitionDiceValue a
               , "baseDamage"   .= ammunitionBaseDamage a
               , "strDamage"    .= strMultFrom ammunitionSTRDamage
               , "piercing"     .= ammunitionPiercing a
               , "strPiercing"  .= strMultFrom ammunitionSTRPiercing
               , "target"       .= ammunitionTarget a
               , "currentMag"   .= ammunitionCurrentMag a
               , "critsOn"      .= ammunitionCritsOn a
               , "ammoTracking" .= emptyAmmoTracking
               , "range"        .= ammunitionRange a
               , "desc"         .= ammunitionDescription a
               , "special"      .= ammunitionSpecials a
               ]

newtype ArmorAdjustment =
  ArmorAdjustment ItemAdjustment

instance ToJSON ArmorAdjustment where
  toJSON (ArmorAdjustment a) =
    object [ "armor"   .= itemAdjustment a
           , "variant" .= itemAdjustmentVariant a
           , "other"   .= itemAdjustmentOther a
           , "total"   .= itemAdjustmentTotal a
           ]

mkArmorAdjustment :: ItemAdjustment -> ArmorAdjustment
mkArmorAdjustment = ArmorAdjustment

data ArmorNotes =
  ArmorNotes
    { armorNotesDefault      :: Maybe T.Text
    , armorNotesVariant      :: Maybe T.Text
    , armorNotesPermutations :: Maybe T.Text
    , armorNotesOther        :: Maybe T.Text
    }

instance ToJSON ArmorNotes where
  toJSON a =
    let orEmpty fn = fromMaybe T.empty $ fn a
     in object
          [ "armor"        .= fromMaybe defaultArmorNotes (armorNotesDefault a)
          , "variant"      .= orEmpty armorNotesVariant
          , "permutations" .= orEmpty armorNotesPermutations
          , "other"        .= orEmpty armorNotesOther
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

emptyAttack :: Attack
emptyAttack =
  Attack
    { attackFireMode = NoFireMode
    , attackFireRate = mkFireRate 0
    , attackHalf     = 0
    , attackFull     = 0
    , attackBonus    = 0
    }

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

data CarryingCapacity =
  CarryingCapacity
    { carryingCapacityDblSTR :: Bool
    , carryingCapacityDblTOU :: Bool
    , carryingCapacityMod    :: Int
    }

instance ToJSON CarryingCapacity where
  toJSON cc =
    object
      [ "bar" .=
          object
            [ "bgBar"  .= valueText "transparent"
            , "bgFill" .= valueText "rgba(0, 0, 0, 0.5)"
            , "left"   .= valueText "0.3em"
            , "tier"   .= valueText "carry"
            , "width"  .= valueText "0.0%"
            ]
      , "carry"          .= valueInt 0
      , "character"      .= valueInt 0
      , "doubleStr"      .= carryingCapacityDblSTR cc
      , "doubleTou"      .= carryingCapacityDblTOU cc
      , "felt"           .= valueInt 0
      , "hearing"        .= valueInt 0
      , "hearingPenalty" .= False
      , "imposing"       .= False
      , "lift"           .= valueInt 0
      , "mod"            .= carryingCapacityMod cc
      , "overencumbered" .= False
      , "push"           .= valueInt 0
      , "strongBack"     .= False
      , "strongman"      .= False
      , "total"          .= valueInt 0
      ]

data CharacterArmor =
  CharacterArmor
    { characterArmorHead     :: Int
    , characterArmorChest    :: Int
    , characterArmorLeftArm  :: Int
    , characterArmorRightArm :: Int
    , characterArmorLeftLeg  :: Int
    , characterArmorRightLeg :: Int
    }

instance ToJSON CharacterArmor where
  toJSON a =
    let mkArmorFor fn =
          object
            [ "protection" .= fn a
            , "resistance" .= fn a
            ]

     in object
          [ "head"     .= mkArmorFor characterArmorHead
          , "chest"    .= mkArmorFor characterArmorChest
          , "leftArm"  .= mkArmorFor characterArmorLeftArm
          , "rightArm" .= mkArmorFor characterArmorRightArm
          , "leftLeg"  .= mkArmorFor characterArmorLeftLeg
          , "rightLeg" .= mkArmorFor characterArmorRightLeg
          ]

emptyCharacterArmor :: CharacterArmor
emptyCharacterArmor =
  CharacterArmor
    { characterArmorHead     = 0
    , characterArmorChest    = 0
    , characterArmorLeftArm  = 0
    , characterArmorRightArm = 0
    , characterArmorLeftLeg  = 0
    , characterArmorRightLeg = 0
    }

data Characteristic =
  Characteristic
    { characteristicValue    :: Int
    , characteristicAdvances :: Bool
    }

instance ToJSON Characteristic where
  toJSON c =
    object
      [ "abilityPool"  .= valueInt 0
      , "advancements" .= valueInt 0
      , "advances"     .= characteristicAdvances c
      , "background"   .= valueInt 0
      , "difficulty"   .= valueInt 0
      , "equipment"    .= valueInt 0
      , "medical"      .= valueInt 0
      , "other"        .= valueInt 0
      , "penalty"      .= valueInt 0
      , "roll"         .= valueInt 0
      , "soldierType"  .= characteristicValue c
      , "total"        .= valueInt 0
      ]

mkCharacteristic :: Int -> Bool -> Characteristic
mkCharacteristic val advances =
  Characteristic
    { characteristicValue    = val
    , characteristicAdvances = advances
    }

data Characteristics =
  Characteristics
    { characteristicsSTR :: Characteristic
    , characteristicsTOU :: Characteristic
    , characteristicsAGI :: Characteristic
    , characteristicsWFR :: Characteristic
    , characteristicsWFM :: Characteristic
    , characteristicsINT :: Characteristic
    , characteristicsPER :: Characteristic
    , characteristicsCRG :: Characteristic
    , characteristicsCHA :: Characteristic
    , characteristicsLDR :: Characteristic
    }

instance ToJSON Characteristics where
  toJSON c =
    object
      [ "str" .= characteristicsSTR c
      , "tou" .= characteristicsTOU c
      , "agi" .= characteristicsAGI c
      , "wfr" .= characteristicsWFR c
      , "wfm" .= characteristicsWFM c
      , "int" .= characteristicsINT c
      , "per" .= characteristicsPER c
      , "crg" .= characteristicsCRG c
      , "cha" .= characteristicsCHA c
      , "ldr" .= characteristicsLDR c
      ]

data Characteristics_Flood =
  Characteristics_Flood
    { floodCharacteristicsSTR :: Int
    , floodCharacteristicsTOU :: Int
    , floodCharacteristicsAGI :: Int
    , floodCharacteristicsWFR :: Int
    , floodCharacteristicsWFM :: Int
    , floodCharacteristicsINT :: Int
    , floodCharacteristicsPER :: Int
    }

instance ToJSON Characteristics_Flood where
  toJSON c =
    let mkChar fn =
          object
            [ "base"      .= fn c
            , "equipment" .= valueInt 0
            , "medical"   .= valueInt 0
            , "other"     .= valueInt 0
            , "total"     .= valueInt 0
            , "roll"      .= valueInt 0
            ]

     in object
          [ "str" .= mkChar floodCharacteristicsSTR
          , "tou" .= mkChar floodCharacteristicsTOU
          , "agi" .= mkChar floodCharacteristicsAGI
          , "wfr" .= mkChar floodCharacteristicsWFR
          , "wfm" .= mkChar floodCharacteristicsWFM
          , "int" .= mkChar floodCharacteristicsINT
          , "per" .= mkChar floodCharacteristicsPER
          ]

data CharacterShields =
  CharacterShields
    { characterShieldsValue    :: Int
    , characterShieldsMax      :: Int
    , characterShieldsRecharge :: Int
    , characterShieldsDelay    :: Int
    }

instance ToJSON CharacterShields where
  toJSON s =
    object
      [ "value"    .= characterShieldsValue s
      , "max"      .= characterShieldsMax s
      , "recharge" .= characterShieldsRecharge s
      , "delay"    .= characterShieldsDelay s
      ]

mkCharacterShields :: Int -> Int -> Int -> CharacterShields
mkCharacterShields integrity recharge delay =
  CharacterShields
    { characterShieldsValue    = integrity
    , characterShieldsMax      = integrity
    , characterShieldsRecharge = recharge
    , characterShieldsDelay    = delay
    }

emptyCharacterShields :: CharacterShields
emptyCharacterShields =
  CharacterShields
    { characterShieldsValue    = 0
    , characterShieldsMax      = 0
    , characterShieldsRecharge = 0
    , characterShieldsDelay    = 0
    }

hasShields :: CharacterShields -> Bool
hasShields = (> 0) . characterShieldsMax

newtype CompendiumDetails = CompendiumDetails T.Text
  deriving newtype (Eq, Ord)

mkCompendiumDetails :: T.Text -> CompendiumDetails
mkCompendiumDetails = CompendiumDetails

compendiumDetails :: CompendiumDetails -> T.Text
compendiumDetails (CompendiumDetails t) = t

type CompendiumData = (Maybe Faction, CompendiumDetails)
type CompendiumMap entries = Map.Map CompendiumData entries

newtype Contamination = Contamination Int
  deriving newtype (ToJSON)

newtype Description = Description T.Text
  deriving newtype (ToJSON)

mkDescription :: T.Text -> Description
mkDescription = Description

data Difficulty =
  Difficulty
    { difficultyAdvancesMythics :: Bool
    , difficultyNormalOnly      :: Bool
    }

instance ToJSON Difficulty where
  toJSON d =
    let normalOnly = difficultyNormalOnly d
     in object
          [ "advancesMythics" .= difficultyAdvancesMythics d
          , "normalOnly"      .= normalOnly
          , "tier"            .= valueText (B.bool "0" "1" normalOnly)
          ]

type DifficultyTierValues = (Int, Int, Int, Int, Int)
type DifficultyTierValuesWithSingleLevelFlag = (Bool, DifficultyTierValues)

data EntryType
  = FoundryActor ActorType
  | FoundryItem  ItemType
  deriving stock (Eq)

instance ToJSON EntryType where
  toJSON (FoundryActor a) = toJSON a
  toJSON (FoundryItem  i) = toJSON i

entryTypeText :: EntryType -> T.Text
entryTypeText et =
  case et of
    FoundryActor _ -> "Actor"
    FoundryItem  _ -> "Item"

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
  deriving stock (Eq, Ord)

instance ToJSON EquipmentTraining where
  toJSON = toJSON . equipmentTrainingText

allEquipmentTrainings :: Set.Set EquipmentTraining
allEquipmentTrainings =
  Set.fromList
    [ Basic
    , Infantry
    , Heavy
    , Advanced
    , Launcher
    , Range
    , Ordnance
    , Cannon
    , Melee
    ]

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

data ExperienceDifficulty =
  ExperienceDifficulty
    { expEasy      :: Int
    , expNormal    :: Int
    , expHeroic    :: Int
    , expLegendary :: Int
    , expNemesis   :: Int
    }

instance ToJSON ExperienceDifficulty where
  toJSON exp =
    object
      [ "easy"      .= expEasy exp
      , "normal"    .= expNormal exp
      , "heroic"    .= expHeroic exp
      , "legendary" .= expLegendary exp
      , "nemesis"   .= expNemesis exp
      ]

mkExperienceDifficulty :: DifficultyTierValues -> ExperienceDifficulty
mkExperienceDifficulty (easy, normal, heroic, legendary, nemesis) =
  ExperienceDifficulty
    { expEasy       = easy
    , expNormal     = normal
    , expHeroic     = heroic
    , expLegendary  = legendary
    , expNemesis    = nemesis
    }

emptyExperienceDifficulty :: ExperienceDifficulty
emptyExperienceDifficulty =
  ExperienceDifficulty
    { expEasy       = 0
    , expNormal     = 0
    , expHeroic     = 0
    , expLegendary  = 0
    , expNemesis    = 0
    }

data ExperiencePayout =
  ExperiencePayout
    { expBase       :: Int
    , expDifficulty :: ExperienceDifficulty
    }

instance ToJSON ExperiencePayout where
  toJSON exp =
    object
      [ "base"           .= expBase exp
      , "difficulty"     .= expDifficulty exp
      , "tier"           .= T.empty
      , "diffMultiplier" .= valueInt 1
      , "kit"            .= valueInt 0
      , "total"          .= valueInt 0
      ]

data Faction
  = UNSC
  | Covenant
  | Banished
  | Forerunner
  -- This catch-all is used to create a compendium for outlier items in
  -- otherwise faction-specific compendium packs.
  | OtherFaction
  deriving stock (Eq, Ord)

instance ToJSON Faction where
  toJSON = toJSON . factionText

factions :: [Faction]
factions =
  [ UNSC
  , Covenant
  , Banished
  , Forerunner
  ]

compendiumFactionFromText :: T.Text -> Faction
compendiumFactionFromText = fromRight OtherFaction . factionFromText

factionFromText :: T.Text -> Either T.Text Faction
factionFromText txt =
  case txt of
    "UNSC"       -> Right UNSC
    "Covenant"   -> Right Covenant
    "Banished"   -> Right Banished
    "Forerunner" -> Right Forerunner
    _            -> Left $ "Unknown Faction " <> txt <> "."

factionText :: Faction -> T.Text
factionText faction =
  case faction of
    UNSC         -> "UNSC"
    Covenant     -> "Covenant"
    Banished     -> "Banished"
    Forerunner   -> "Forerunner"
    OtherFaction -> "Other"

factionTrainingFor :: Faction -> FactionTraining
factionTrainingFor faction =
  case faction of
    UNSC       -> UNSCTraining
    Covenant   -> CovenantTraining
    Banished   -> CovenantTraining
    Forerunner -> ForerunnerTraining
    -- An unfortunate side-effect of having an "Other" catch-all faction is
    -- not having a good mapping for it. In this case, we've elected to default
    -- to the UNSC faction training since that's what would be automatically
    -- populated by the UI upon render.
    OtherFaction -> UNSCTraining

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
  = NoFireMode
  | Auto
  | Burst
  | Charge
  | Drawback
  | Flintlock
  | Pump
  | Semi
  | Sustained
  deriving stock (Eq, Ord)

fireModeFromText :: T.Text -> Maybe FireMode
fireModeFromText txt =
  case txt of
   "Auto"      -> Just Auto
   "Burst"     -> Just Burst
   "Charge"    -> Just Charge
   "Drawback"  -> Just Drawback
   "Flintlock" -> Just Flintlock
   "Pump"      -> Just Pump
   "Semi"      -> Just Semi
   "Sustained" -> Just Sustained
   _           -> Nothing

fireModeText :: FireMode -> T.Text
fireModeText fm =
  case fm of
    NoFireMode -> T.empty
    Auto       -> "auto"
    Burst      -> "burst"
    Charge     -> "charge"
    Drawback   -> "drawback"
    Flintlock  -> "flintlock"
    Pump       -> "pump"
    Semi       -> "semi"
    Sustained  -> "sustained"

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

mkFireModes :: Map.Map FireMode FireRate -> FireModes
mkFireModes = FireModes

newtype FireRate = FireRate Int
  deriving newtype (Show, ToJSON)

mkFireRate :: Int -> FireRate
mkFireRate = FireRate

newtype FloodWounds = FloodWounds Int

instance ToJSON FloodWounds where
  toJSON (FloodWounds w) =
    object
      [ "value" .= w
      , "max"   .= w
      , "base"  .= w
      , "mod"   .= valueInt 0
      ]

data Hardpoints =
  Hardpoints
    { hardpointsHead     :: Int
    , hardpointsChest    :: Int
    , hardpointsLeftArm  :: Int
    , hardpointsRightArm :: Int
    , hardpointsLeftLeg  :: Int
    , hardpointsRightLeg :: Int
    }

instance ToJSON Hardpoints where
  toJSON hp =
    object [ "head"     .= hardpointsHead     hp
           , "chest"    .= hardpointsChest    hp
           , "leftArm"  .= hardpointsLeftArm  hp
           , "rightArm" .= hardpointsRightArm hp
           , "leftLeg"  .= hardpointsLeftLeg  hp
           , "rightLeg" .= hardpointsRightLeg hp
           ]

emptyHardpoints :: Hardpoints
emptyHardpoints =
  Hardpoints
    { hardpointsHead     = 0
    , hardpointsChest    = 0
    , hardpointsLeftArm  = 0
    , hardpointsRightArm = 0
    , hardpointsLeftLeg  = 0
    , hardpointsRightLeg = 0
    }

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
    , itemTrainingsFaction   :: Maybe FactionTraining
    }

instance ToJSON ItemTrainings where
  toJSON t =
    object
      [ "equipment" .= maybe "" equipmentTrainingText (itemTrainingsEquipment t)
      , "faction"   .= maybe "" factionTrainingText (itemTrainingsFaction t)
      ]

mkItemTrainings :: Maybe Faction -> Maybe EquipmentTraining -> ItemTrainings
mkItemTrainings faction mbEquipmentTraining =
  ItemTrainings mbEquipmentTraining $ factionTrainingFor <$> faction

data ItemType
  = ItemAbility
  | ItemArmor
  | ItemEducation
  | ItemEquipment
  | ItemWeapon
  deriving stock (Eq)

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

data Luck =
  Luck
    { luckEasy      :: Int
    , luckNormal    :: Int
    , luckHeroic    :: Int
    , luckLegendary :: Int
    , luckNemesis   :: Int
    }

instance ToJSON Luck where
  toJSON l =
    object
      [ "advancements" .= valueInt 0
      , "burnt"        .= valueInt 0
      , "max"          .= valueInt 0
      , "other"        .= valueInt 0
      , "starting"     .= valueInt 0

      , "difficulty" .=
          object
            [ "easy"      .= luckEasy l
            , "normal"    .= luckNormal l
            , "heroic"    .= luckHeroic l
            , "legendary" .= luckLegendary l
            , "nemesis"   .= luckNemesis l
            ]
      ]

mkLuck :: DifficultyTierValues -> Luck
mkLuck (easy, normal, heroic, legendary, nemesis) =
  Luck
    { luckEasy      = easy
    , luckNormal    = normal
    , luckHeroic    = heroic
    , luckLegendary = legendary
    , luckNemesis   = nemesis
    }

newtype MagazineCapacity = MagazineCapacity Int
  deriving newtype (ToJSON)

mkMagazineCapacity :: Int -> MagazineCapacity
mkMagazineCapacity = MagazineCapacity

data Movement =
  Movement
    { movementRunChargeBonus :: Int
    , movementJumpMultiplier :: Int
    , movementLeapBonus      :: Int
    , movementLeapMultiplier :: Int
    }

instance ToJSON Movement where
  toJSON m =
    object
      [ "agiBonusRunCharge" .= movementRunChargeBonus m
      , "blur"              .= False
      , "charge"            .= valueInt 0
      , "full"              .= valueInt 0
      , "half"              .= valueInt 0
      , "jump"              .= valueInt 0
      , "jumpMultiplier"    .= movementJumpMultiplier m
      , "leap"              .= valueInt 0
      , "leapAgiBonus"      .= movementLeapBonus m
      , "leapMultiplier"    .= movementLeapMultiplier m
      ]

data MythicCharacteristics =
  MythicCharacteristics
    { mythicSTR :: Int
    , mythicTOU :: Int
    , mythicAGI :: Int
    }

instance ToJSON MythicCharacteristics where
  toJSON m =
    let mkMythic fn =
          object
            [ "advancements" .= valueInt 0
            , "difficulty"   .= valueInt 0
            , "equipment"    .= valueInt 0
            , "other"        .= valueInt 0
            , "soldierType"  .= fn m
            , "total"        .= valueInt 0
            ]

     in object
          [ "str"   .= mkMythic mythicSTR
          , "tou"   .= mkMythic mythicTOU
          , "agi"   .= mkMythic mythicAGI
          , "notes" .= T.empty
          ]

data MythicCharacteristics_Flood =
  MythicCharacteristics_Flood
    { floodMythicSTR :: Int
    , floodMythicTOU :: Int
    , floodMythicAGI :: Int
    }

instance ToJSON MythicCharacteristics_Flood where
  toJSON m =
    let mkMythic fn =
          object
            [ "base"      .= fn m
            , "equipment" .= valueInt 0
            , "other"     .= valueInt 0
            , "total"     .= valueInt 0
            ]

     in object
          [ "str" .= mkMythic floodMythicSTR
          , "tou" .= mkMythic floodMythicTOU
          , "agi" .= mkMythic floodMythicAGI
          ]

newtype Name = Name T.Text
  deriving newtype (Eq, Ord, ToJSON)

mkName :: T.Text -> Name
mkName = Name

nameText :: Name -> T.Text
nameText (Name t) = t

newtype Prerequisites = Prerequisites T.Text
  deriving newtype (ToJSON)

mkPrereqs :: T.Text -> Prerequisites
mkPrereqs = Prerequisites

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

instance ToJSON Reload where
  toJSON (Reload r) =
    object [ "base"  .= r
           , "total" .= r
           ]

mkReload :: Int -> Reload
mkReload = Reload

newtype ScopeMagnification = ScopeMagnification Int
  deriving newtype (ToJSON)

mkScopeMagnification :: Int -> ScopeMagnification
mkScopeMagnification = ScopeMagnification

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
  deriving stock (Eq, Ord)

instance ToJSON Size where
  toJSON = toJSON . sizeText

sizeFromText :: T.Text -> Either T.Text Size
sizeFromText txt =
  case T.toLower txt of
    "mini"       -> Right Mini
    "small"      -> Right Small
    "normal"     -> Right Normal
    "large"      -> Right Large
    "huge"       -> Right Huge
    "hulking"    -> Right Hulking
    "giant"      -> Right Giant
    "immense"    -> Right Immense
    "massive"    -> Right Massive
    "great"      -> Right Great
    "monumental" -> Right Monumental
    "colossal"   -> Right Colossal
    "vast"       -> Right Vast
    _            -> Left $ "Unrecognized Size: " <> txt

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

sizeToInt :: Size -> Int
sizeToInt size =
  case size of
    Mini       ->  0
    Small      ->  1
    Normal     ->  1
    Large      ->  2
    Huge       ->  3
    Hulking    ->  3
    Giant      ->  4
    Immense    ->  4
    Massive    ->  5
    Great      ->  6
    Monumental ->  7
    Colossal   -> 10
    Vast       -> 25

newtype Skills = Skills [T.Text]

instance ToJSON Skills where
  toJSON (Skills skills) =
    object
      . mappend [ "notes" .= T.empty ]
      . flip fmap skills
      $ \skill ->
        keyFromText skill .=
          object
            [ "characteristic" .= T.empty
            , "mods"           .= valueInt 0
            , "roll"           .= valueInt 0
            , "training" .=
                object
                  [ "tier"    .= T.empty
                  , "penalty" .= valueInt 1
                  ]
            ]

characterSkillList :: Skills
characterSkillList =
  Skills
    [ "appeal"
    , "athletics"
    , "camouflage"
    , "command"
    , "cryptography"
    , "deception"
    , "demolition"
    , "evasion"
    , "gambling"
    , "interrogation"
    , "intimidation"
    , "investigation"
    , "medHuman"
    , "medCovenant"
    , "medXenophile"
    , "navGroundAir"
    , "navSpace"
    , "negotiation"
    , "pilotGround"
    , "pilotAir"
    , "pilotSpace"
    , "security"
    , "stunting"
    , "survival"
    , "techHuman"
    , "techCovenant"
    , "techForerunner"
    ]

floodSkillList :: Skills
floodSkillList =
  Skills
    [ "athletics"
    , "camouflage"
    , "cryptography"
    , "demolition"
    , "evasion"
    , "intimidation"
    , "investigation"
    , "navGroundAir"
    , "navSpace"
    , "pilotGround"
    , "pilotAir"
    , "pilotSpace"
    , "security"
    , "stunting"
    , "survival"
    , "techHuman"
    , "techCovenant"
    , "techForerunner"
    ]

data SpecialRules_Vehicle =
  SpecialRules_Vehicle
    { allTerrain      :: Maybe ()
    , antiGrav        :: Maybe ()
    , autoloader      :: Maybe ()
    , boost           :: Maybe Int
    , continuousTrack :: Maybe ()
    , heavyPlating    :: Maybe ()
    , neuralInterface :: Maybe ()
    , openTop         :: Maybe ()
    , slipspace       :: Maybe ()
    , walkerStomp     :: Maybe ()
    }

instance ToJSON SpecialRules_Vehicle where
  toJSON r =
    let open = openTop r
        noneRule mbRule = object [ "has" .= isJust mbRule ]
        intRule mbInt =
          object [ "has"   .= isJust mbInt
                 , "value" .= fromMaybe 0 mbInt
                 ]

     in object [ "allTerrain"      .= noneRule (allTerrain r)
               , "antiGrav"        .= noneRule (antiGrav r)
               , "autoloader"      .= noneRule (autoloader r)
               , "boost"           .= intRule  (boost r)
               , "continuousTrack" .= noneRule (continuousTrack r)
               , "enclosedTop"     .= object [ "has" .= not (isJust open) ]
               , "heavyPlating"    .= noneRule (heavyPlating r)
               , "neuralInterface" .= noneRule (neuralInterface r)
               , "openTop"         .= noneRule open
               , "slipspace"       .= noneRule (slipspace r)
               , "walkerStomp"     .= noneRule (walkerStomp r)
               ]

emptyVehicleSpecialRules :: SpecialRules_Vehicle
emptyVehicleSpecialRules =
  SpecialRules_Vehicle
    { allTerrain      = Nothing
    , antiGrav        = Nothing
    , autoloader      = Nothing
    , boost           = Nothing
    , continuousTrack = Nothing
    , heavyPlating    = Nothing
    , neuralInterface = Nothing
    , openTop         = Nothing
    , slipspace       = Nothing
    , walkerStomp     = Nothing
    }


data SpecialRules_Weapon =
  SpecialRules_Weapon
    { acid             :: Maybe Int
    , blast            :: Maybe Int
    , cauterize        :: Maybe ()
    , chargeRule       :: Maybe Int
    , cryo             :: Maybe T.Text
    , diceMinimum      :: Maybe Int
    , electrified      :: Maybe T.Text
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
    , linked           :: Maybe Int
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

instance ToJSON SpecialRules_Weapon where
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
               , "electrified"      .= textRule (electrified r)
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
               , "linked"           .= intRule  (linked r)
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

emptyWeaponSpecialRules :: SpecialRules_Weapon
emptyWeaponSpecialRules =
  SpecialRules_Weapon
    { acid             = Nothing
    , blast            = Nothing
    , cauterize        = Nothing
    , chargeRule       = Nothing
    , cryo             = Nothing
    , diceMinimum      = Nothing
    , electrified      = Nothing
    , emp              = Nothing
    , flame            = Nothing
    , flashbang        = Nothing
    , gravimetricPulse = Nothing
    , gravity          = Nothing
    , hardlight        = Nothing
    , headshot         = Nothing
    , homing           = Nothing
    , kill             = Nothing
    , kinetic          = Nothing
    , linked           = Nothing
    , longBarrel       = Nothing
    , needle           = Nothing
    , nonlethal        = Nothing
    , overheat         = Nothing
    , penetrating      = Nothing
    , rechargeRate     = Nothing
    , singleLoading    = Nothing
    , slow             = Nothing
    , smoke            = Nothing
    , spike            = Nothing
    , spin             = Nothing
    , spread           = Nothing
    , sticky           = Nothing
    , stun             = Nothing
    , tearGas          = Nothing
    , tranquilize      = Nothing
    , vehicleLock      = Nothing
    }

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

data StrengthMultiplier
  = NoMultiplier
  | HalfStrength
  | FullStrength
  | DoubleStrength

strengthMultiplierFromText :: T.Text -> Maybe StrengthMultiplier
strengthMultiplierFromText txt =
  case txt of
    "N/A"    -> Just NoMultiplier
    "Half"   -> Just HalfStrength
    "Full"   -> Just FullStrength
    "Double" -> Just DoubleStrength
    _        -> Nothing

strengthMultiplier :: StrengthMultiplier -> Double
strengthMultiplier sm =
  case sm of
    NoMultiplier   -> 0
    HalfStrength   -> 0.5
    FullStrength   -> 1
    DoubleStrength -> 2

data Swarm =
  Swarm
    { swarmWillSwarm :: Bool
    , swarmValue     :: Int
    }

instance ToJSON Swarm where
  toJSON s =
    let willSwarm = swarmWillSwarm s
        swarmVal = B.bool 0 (swarmValue s) willSwarm
     in object
          [ "base"      .= swarmVal
          , "mod"       .= valueInt 0
          , "total"     .= swarmVal
          , "willSwarm" .= willSwarm
          ]

mkSwarm :: Int -> Bool -> Swarm
mkSwarm val willSwarm =
  Swarm
    { swarmWillSwarm = willSwarm
    , swarmValue     = val
    }

data Token =
  Token
    { tokenName :: Name
    , tokenType :: ActorType
    , tokenSize :: Size
    , tokenBar2 :: Maybe T.Text
    }

instance ToJSON Token where
  toJSON token =
    let isNamed = tokenType token == ActorNamed
        mkBar = object . L.singleton . ("attribute" .=)
        (size, scale) =
          case tokenSize token of
            Mini  -> (1, 1 % 2)
            Small -> (1, 4 % 5)
            tSize -> (sizeToInt tSize, 1 % 1)

        texture =
          object
            [ "src"      .= ("icons/svg/mystery-man.svg" :: T.Text)
            , "scaleX"   .= valueRatio scale
            , "scaleY"   .= valueRatio scale
            , "offsetX"  .= valueInt 0
            , "offsetY"  .= valueInt 0
            , "rotation" .= valueInt 0
            , "tint"     .= nullJSON
            ]

        animation =
          object
            [ "type"      .= nullJSON
            , "speed"     .= valueInt 5
            , "intensity" .= valueInt 5
            , "reverse"   .= False
            ]

        darkness =
          object
            [ "min" .= valueInt 0
            , "max" .= valueInt 1
            ]

        light =
          object
            [ "alpha"       .= valueRatio (1 % 2)
            , "angle"       .= valueInt 360
            , "bright"      .= valueInt 0
            , "color"       .= nullJSON
            , "coloration"  .= valueInt 1
            , "dim"         .= valueInt 0
            , "attenuation" .= valueRatio (1 % 2)
            , "luminosity"  .= valueRatio (1 % 2)
            , "saturation"  .= valueInt 0
            , "contrast"    .= valueInt 0
            , "shadows"     .= valueInt 0
            , "animation"   .= animation
            , "darkness"    .= darkness
            ]

        sight =
          object
            [ "enabled"     .= False
            , "range"       .= nullJSON
            , "angle"       .= valueInt 360
            , "visionMode"  .= ("basic" :: T.Text)
            , "color"       .= nullJSON
            , "attenuation" .= valueRatio (1 % 10)
            , "brightness"  .= valueInt 0
            , "saturation"  .= valueInt 0
            , "contrast"    .= valueInt 0
            ]

     in object
          [ "name"           .= tokenName token
          , "displayName"    .= valueInt (B.bool 0 1 isNamed)
          , "actorLink"      .= isNamed
          , "texture"        .= texture
          , "width"          .= valueInt size
          , "height"         .= valueInt size
          , "lockRotation"   .= False
          , "rotation"       .= valueInt 0
          , "alpha"          .= valueInt 1
          , "disposition"    .= valueInt (negate 1)
          , "displayBars"    .= valueInt 50
          , "bar1"           .= mkBar (Just "wounds")
          , "bar2"           .= mkBar (tokenBar2 token)
          , "light"          .= light
          , "sight"          .= sight
          , "detectionModes" .= emptyArray
          , "flags"          .= emptyObject
          , "randomImg"      .= False
          ]

data Trainings =
  Trainings
    { trainingsEquipment :: Set.Set EquipmentTraining
    , trainingsFaction   :: Set.Set Faction
    , trainingsWeapon    :: WeaponTrainings
    , trainingsAlienTech :: Bool
    }

instance ToJSON Trainings where
  toJSON t =
    let equipment = trainingsEquipment t
        factionTrainingss = trainingsFaction t
     in object
          [ "equipment" .=
              object
                [ "basic"    .= Set.member Basic    equipment
                , "infantry" .= Set.member Infantry equipment
                , "heavy"    .= Set.member Heavy    equipment
                , "advanced" .= Set.member Advanced equipment
                , "launcher" .= Set.member Launcher equipment
                , "range"    .= Set.member Range    equipment
                , "ordnance" .= Set.member Ordnance equipment
                , "cannon"   .= Set.member Cannon   equipment
                , "melee"    .= Set.member Melee    equipment
                ]
          , "faction" .=
              object
                [ "unsc"       .= Set.member UNSC       factionTrainingss
                , "covenant"   .= Set.member Covenant   factionTrainingss
                , "forerunner" .= Set.member Forerunner factionTrainingss
                ]
          , "weapons"   .= trainingsWeapon t
          , "alienTech" .= False
          ]

emptyTrainings :: Trainings
emptyTrainings =
  Trainings
    { trainingsEquipment = Set.empty
    , trainingsFaction   = Set.empty
    , trainingsWeapon    = emptyWeaponTrainings
    , trainingsAlienTech = False
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

emptyWeaponRange :: WeaponRange
emptyWeaponRange =
  WeaponRange
    { weaponRangeClose = 0
    , weaponRangeLong  = 0
    , weaponRangeMelee = 0
    }

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

emptyWeaponSettings :: WeaponSettings
emptyWeaponSettings =
  WeaponSettings
    { settingsFirearmType    = Firearms
    , settingsBarrel         = XS
    , settingsBulletDiameter = 0
    , settingsCaseLength     = 0
    , settingsSingleUse      = False
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
  deriving stock (Eq, Ord)

weaponTagFromText :: T.Text -> Maybe WeaponTag
weaponTagFromText tag =
  case tag of
    "[UD]" -> Just UD
    "[I]"  -> Just I
    "[OH]" -> Just OH
    "[TH]" -> Just TH
    "[HW]" -> Just HW
    "[SD]" -> Just SD
    "[PD]" -> Just PD
    "[BD]" -> Just BD
    _      -> Nothing

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

data WeaponTrainings =
  WeaponTrainings
    { weaponTrainingsHTH              :: Bool
    , weaponTrainingsMAC              :: Bool
    , weaponTrainingsRapidReload      :: Bool
    , weaponTrainingsUnarmedCombatant :: Bool
    }

instance ToJSON WeaponTrainings where
  toJSON t =
    object
      [ "hth"              .= weaponTrainingsHTH t
      , "mac"              .= weaponTrainingsMAC t
      , "rapidReload"      .= weaponTrainingsRapidReload t
      , "unarmedCombatant" .= weaponTrainingsUnarmedCombatant t
      ]

emptyWeaponTrainings :: WeaponTrainings
emptyWeaponTrainings =
  WeaponTrainings
    { weaponTrainingsHTH              = False
    , weaponTrainingsMAC              = False
    , weaponTrainingsRapidReload      = False
    , weaponTrainingsUnarmedCombatant = False
    }

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

emptyWeight :: Weight
emptyWeight =
  Weight
    { weightEach          = 0
    , weightSelfSupported = False
    }

newtype WeaponType = WeaponType { unWeaponType :: T.Text }
  deriving newtype (ToJSON)

data Wounds =
  Wounds
    { woundsMod    :: Int
    , woundsDblTOU :: Bool
    }

instance ToJSON Wounds where
  toJSON w =
    object
      [ "advancements" .= valueInt 0
      , "aiDegen"      .= valueInt 0
      , "difficulty"   .= valueInt 0
      , "doubleTou"    .= woundsDblTOU w
      , "max"          .= valueInt 0
      , "other"        .= woundsMod w
      , "value"        .= valueInt 0
      ]
