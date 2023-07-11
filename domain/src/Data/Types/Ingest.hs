module Data.Types.Ingest
  ( RawData(..)
  , RawAbility(..)
  , RawArmor(..)
  , RawBestiary(..)
  , RawEquipment(..)
  , RawFlood(..)
  , RawMeleeBase(..)
  , RawMeleeWeapon(..)
  , RawPermutation(..)
  , RawRangedBase(..)
  , RawRangedWeapon(..)
  , RawVehicle(..)
  ) where

import           Flipstone.Prelude

import           Control.Monad.Extra (mapMaybeM)
import qualified Data.Csv as CSV
import           Data.Csv ((.:))
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           GHC.Types (Double)
import           Text.Read (readMaybe)
import           Text.Show (show)

data RawData
  = AbilityData     RawAbility
  | ArmorData       RawArmor
  | BestiaryData    RawBestiary
  | EquipmentData   RawEquipment
  | FloodData       RawFlood
  | MeleeData       RawMeleeWeapon
  | PermutationData RawPermutation
  | RangedData      RawRangedWeapon
  | VehicleData     RawVehicle

data RawAbility =
  RawAbility
    { rawAbilityName        :: T.Text
    , rawAbilityPrereqs     :: T.Text
    , rawAbilityCost        :: Int
    , rawAbilitySummary     :: T.Text
    , rawAbilityDescription :: T.Text
    }

instance CSV.FromNamedRecord RawAbility where
  parseNamedRecord a =
    RawAbility <$> a .: "Name"
               <*> a .: "COMP_prerequisite"
               <*> a .: "Comp_experience"
               <*> a .: "Comp_aditional_info"
               <*> a .: "COMP_description"

data RawArmor =
  RawArmor
    { rawArmorName          :: T.Text
    , rawArmorFaction       :: T.Text
    , rawArmorHead          :: Int
    , rawArmorArms          :: Int
    , rawArmorChest         :: Int
    , rawArmorLegs          :: Int
    , rawArmorIntegrity     :: Int
    , rawArmorDelay         :: Int
    , rawArmorRecharge      :: Int
    , rawArmorVariant       :: Bool
    , rawArmorWeight        :: Double
    , rawArmorSelfSupported :: Bool
    , rawArmorStats         :: T.Text
    , rawArmorDescription   :: Maybe T.Text
    , rawArmorPrice         :: Int
    }

instance CSV.FromNamedRecord RawArmor where
  parseNamedRecord a =
    RawArmor <$> a .: "Name"
             <*> a .: "COMP_faction"
             <*> a .: "COMP_armor_head"
             <*> a .: "COMP_armor_arms"
             <*> a .: "COMP_armor_chest"
             <*> a .: "COMP_armor_legs"
             <*> (defaultZero <$> a .: "COMP_shield_integrity")
             <*> (defaultZero <$> a .: "COMP_shield_delay")
             <*> (defaultZero <$> a .: "COMP_shield_recharge")
             <*> (parseBool =<< a .: "COMP_variant")
             <*> a .: "COMP_weight"
             <*> (parseBool =<< a .: "self_supporting")
             <*> a .: "stat_adjustments"
             <*> (nonEmptyText <$> a .: "COMP_description")
             <*> a .: "COMP_price"

data RawBestiary =
  RawBestiary
    { rawBestiaryName         :: T.Text
    , rawBestiaryFaction      :: T.Text
    , rawBestiarySize         :: T.Text
    , rawBestiarySTR          :: Int
    , rawBestiaryTOU          :: Int
    , rawBestiaryAGI          :: Int
    , rawBestiaryWFR          :: Int
    , rawBestiaryWFM          :: Int
    , rawBestiaryINT          :: Int
    , rawBestiaryPER          :: Int
    , rawBestiaryCRG          :: Int
    , rawBestiaryCHA          :: Int
    , rawBestiaryLDR          :: Int
    , rawBestiaryMythicSTR    :: Int
    , rawBestiaryMythicTOU    :: Int
    , rawBestiaryMythicAGI    :: Int
    , rawBestiaryStaticStats  :: T.Text
    , rawBestiaryExperience   :: T.Text
    , rawBestiaryLuck         :: T.Text
    , rawBestiaryArmor        :: Maybe Int
    , rawBestiaryIntegrity    :: Maybe Int
    , rawBestiaryDelay        :: Maybe Int
    , rawBestiaryRecharge     :: Maybe Int
    , rawBestiaryChargeRunMod :: Int
    , rawBestiaryJumpMod      :: Int
    , rawBestiaryLeapMod      :: Int
    , rawBestiaryLeapAdd      :: Int
    , rawBestiaryCarryStrMod  :: Int
    , rawBestiaryCarryTouMod  :: Int
    , rawBestiaryCarryCap     :: Maybe Int
    , rawBestiaryDescription  :: T.Text
    , rawBestiaryAbilities    :: [RawAbility]
    }

instance CSV.FromNamedRecord RawBestiary where
  parseNamedRecord b =
    let indices = [ 0..10 ] :: [Int]
        mkAbility n = do
          name <- b .: ("Comp_aditional_info[" <> n <> "]")
          if T.null name
             then
               pure Nothing
             else
               Just . RawAbility name "Bestiary" 0 T.empty
                 <$> b .: ("COMP_description[" <> n <> "]")

     in RawBestiary
          <$> b .: "Name"
          <*> b .: "faction"
          <*> b .: "COMP_size"
          <*> b .: "COMP_strength"
          <*> b .: "COMP_toughness"
          <*> b .: "COMP_agility"
          <*> b .: "COMP_warfare ranged"
          <*> b .: "COMP_warfare melee"
          <*> b .: "COMP_intelligence"
          <*> b .: "COMP_perception"
          <*> b .: "COMP_courage"
          <*> b .: "COMP_charisma"
          <*> b .: "COMP_leadership"
          <*> b .: "COMP_mythic_strength"
          <*> b .: "COMP_mythic_toughness"
          <*> b .: "COMP_mythic_agility"
          <*> b .: "static_stats"
          <*> b .: "COMP_experience"
          <*> b .: "COMP_luck"
          <*> (positiveInt <$> b .: "COMP_armor_head")
          <*> (positiveInt <$> b .: "COMP_shield_integrity")
          <*> (positiveInt <$> b .: "COMP_shield_delay")
          <*> (positiveInt <$> b .: "COMP_shield_recharge")
          <*> b .: "COMP_charge_move_add"
          <*> b .: "COMP_jump_move_mod"
          <*> b .: "COMP_leap_mod"
          <*> b .: "COMP_leap_add"
          <*> b .: "COMP_str_carry_mod"
          <*> b .: "COMP_tou_carry_mod"
          <*> (positiveInt <$> b .: "COMP_carry_add")
          <*> b .: "COMP_armor_description"
          <*> mapMaybeM (mkAbility . TE.encodeUtf8 . T.pack . show) indices

data RawEquipment =
  RawEquipment
    { rawEquipmentName        :: T.Text
    , rawEquipmentFaction     :: T.Text
    , rawEquipmentDescription :: T.Text
    , rawEquipmentWeight      :: Double
    , rawEquipmentPrice       :: Int
    }

instance CSV.FromNamedRecord RawEquipment where
  parseNamedRecord e =
    RawEquipment <$> e .: "Name"
                 <*> e .: "COMP_faction"
                 <*> e .: "COMP_description"
                 <*> (defaultZeroDbl <$> e .: "COMP_weight")
                 <*> e .: "COMP_price"

data RawFlood =
  RawFlood
    { rawFloodName       :: T.Text
    , rawFloodSize       :: T.Text
    , rawFloodSTR        :: Int
    , rawFloodTOU        :: Int
    , rawFloodAGI        :: Int
    , rawFloodWFR        :: Int
    , rawFloodWFM        :: Int
    , rawFloodINT        :: Int
    , rawFloodPER        :: Int
    , rawFloodMythicSTR  :: Int
    , rawFloodMythicTOU  :: Int
    , rawFloodMythicAGI  :: Int
    , rawFloodExperience :: Int
    , rawFloodWounds     :: Int
    , rawFloodAbilities  :: [RawAbility]
    }

instance CSV.FromNamedRecord RawFlood where
  parseNamedRecord f =
    let indices = [ 0..5 ] :: [Int]
        mkAbility n = do
          name <- f .: ("Comp_aditional_info[" <> n <> "]")
          if T.null name
             then
               pure Nothing
             else
               Just . RawAbility name "Flood" 0 T.empty
                 <$> f .: ("COMP_description[" <> n <> "]")

     in RawFlood
          <$> f .: "Name"
          <*> f .: "COMP_size"
          <*> f .: "COMP_strength"
          <*> f .: "COMP_toughness"
          <*> f .: "COMP_agility"
          <*> f .: "COMP_warfare ranged"
          <*> f .: "COMP_warfare melee"
          <*> f .: "COMP_intelligence"
          <*> f .: "COMP_perception"
          <*> f .: "COMP_mythic_strength"
          <*> f .: "COMP_mythic_toughness"
          <*> f .: "COMP_mythic_agility"
          <*> f .: "COMP_experience"
          <*> f .: "COMP_wounds"
          <*> mapMaybeM (mkAbility . TE.encodeUtf8 . T.pack . show) indices

data RawMeleeBase =
  RawMeleeBase
    { rawMeleeBaseName         :: T.Text
    , rawMeleeBaseType         :: T.Text
    , rawMeleeBaseAttr         :: T.Text
    , rawMeleeBaseRange        :: Int
    , rawMeleeBaseDamageRoll   :: T.Text
    , rawMeleeBaseDamageBase   :: Int
    , rawMeleeBasePierce       :: Int
    , rawMeleeBaseHitMod       :: Int
    , rawMeleeBaseBreakpoints  :: Int
    , rawMeleeBaseBaseAdd      :: T.Text
    , rawMeleeBasePierceAdd    :: T.Text
    , rawMeleeBaseSpecialRules :: T.Text
    , rawMeleeBaseDescription  :: T.Text
    }

data RawMeleeWeapon =
  RawMeleeWeapon
    { rawMeleeName    :: T.Text
    , rawMeleeBases   :: NE.NonEmpty RawMeleeBase
    , rawMeleeFaction :: T.Text
    , rawMeleeWeight  :: Double
    , rawMeleePrice   :: Int
    }

instance CSV.FromNamedRecord RawMeleeWeapon where
  parseNamedRecord m = do
    let mkBase n =
          RawMeleeBase
            <$> m .: ("Comp_name[" <> n <> "]")
            <*> m .: ("COMP_type[" <> n <> "]")
            <*> m .: ("COMP_attribute[" <> n <> "]")
            <*> (defaultZero <$> m .: ("COMP_range[" <> n <> "]"))
            <*> m .: ("COMP_damage_roll[" <> n <> "]")
            <*> m .: ("COMP_damage_base[" <> n <> "]")
            <*> m .: ("COMP_pierce[" <> n <> "]")
            <*> m .: ("COMP_hitmod[" <> n <> "]")
            <*> m .: ("COMP_breakpoints[" <> n <> "]")
            <*> m .: ("COMP_base_add[" <> n <> "]")
            <*> m .: ("COMP_pierce_add[" <> n <> "]")
            <*> m .: ("COMP_special_rules[" <> n <> "]")
            <*> m .: ("COMP_description[" <> n <> "]")

    base <- NE.singleton <$> mkBase "0"
    variantName <- m .: "Comp_name[1]"
    variant <-
      if T.null variantName
         then pure []
         else L.singleton <$> mkBase "1"

    RawMeleeWeapon
      <$> m .: "Name"
      <*> pure (NE.appendList base variant)
      <*> m .: "COMP_faction"
      <*> m .: "COMP_weight"
      <*> m .: "COMP_price"

data RawPermutation =
  RawPermutation
    { rawPermutationName        :: T.Text
    , rawPermutationFaction     :: T.Text
    , rawPermutationDescription :: T.Text
    , rawPermutationLocation    :: T.Text
    , rawPermutationPrice       :: Int
    }

instance CSV.FromNamedRecord RawPermutation where
  parseNamedRecord p =
    RawPermutation
      <$> p .: "Name"
      <*> p .: "COMP_faction"
      <*> p .: "COMP_description"
      <*> p .: "COMP_hpt_location"
      <*> p .: "COMP_price"

data RawRangedBase =
  RawRangedBase
    { rawRangedBaseName         :: T.Text
    , rawRangedBaseType         :: T.Text
    , rawRangedBaseAttr         :: T.Text
    , rawRangedBaseRange        :: T.Text
    , rawRangedBaseDamageRoll   :: T.Text
    , rawRangedBaseDamageBase   :: Int
    , rawRangedBasePierce       :: Int
    , rawRangedBaseHitMod       :: Int
    , rawRangedBaseMagazine     :: Int
    , rawRangedBaseROF          :: T.Text
    , rawRangedBaseReload       :: T.Text
    , rawRangedBaseSpecialRules :: T.Text
    , rawRangedBaseDescription  :: T.Text
    }

data RawRangedWeapon =
  RawRangedWeapon
    { rawRangedName    :: T.Text
    , rawRangedBases   :: NE.NonEmpty RawRangedBase
    , rawRangedFaction :: T.Text
    , rawRangedWeight  :: Double
    , rawRangedPrice   :: Int
    }

instance CSV.FromNamedRecord RawRangedWeapon where
  parseNamedRecord r = do
    let mkBase n =
          RawRangedBase
            <$> r .: ("COMP_name[" <> n <> "]")
            <*> r .: ("COMP_type[" <> n <> "]")
            <*> r .: ("COMP_attribute[" <> n <> "]")
            <*> r .: ("COMP_range[" <> n <> "]")
            <*> r .: ("COMP_damage_roll[" <> n <> "]")
            <*> r .: ("COMP_damage_base[" <> n <> "]")
            <*> r .: ("COMP_pierce[" <> n <> "]")
            <*> r .: ("COMP_hitmod[" <> n <> "]")
            <*> r .: ("COMP_magazine[" <> n <> "]")
            <*> r .: ("COMP_rof[" <> n <> "]")
            <*> r .: ("COMP_reload[" <> n <> "]")
            <*> r .: ("COMP_special_rules[" <> n <> "]")
            <*> r .: ("COMP_description[" <> n <> "]")

    base <- NE.singleton <$> mkBase "0"
    variantName <- r .: "COMP_name[1]"
    variant <-
      if T.null variantName
         then pure []
         else L.singleton <$> mkBase "1"

    RawRangedWeapon
      <$> r .: "Name"
      <*> pure (NE.appendList base variant)
      <*> r .: "COMP_faction"
      <*> r .: "COMP_weight"
      <*> r .: "COMP_price"

data RawVehicle =
  RawVehicle
    { rawVehicleName                :: T.Text
    , rawVehicleFaction             :: T.Text
    , rawVehiclePrice               :: Int
    , rawVehicleExperience          :: Int
    , rawVehicleLength              :: Double
    , rawVehicleWidth               :: Double
    , rawVehicleHeight              :: Double
    , rawVehicleWeight              :: T.Text
    , rawVehicleCrew                :: Maybe T.Text
    , rawVehicleComplement          :: Maybe T.Text
    , rawVehicleSizePoints          :: Int
    , rawVehicleWeaponPoints        :: Int
    , rawVehicleSize                :: T.Text
    , rawVehicleAccelerate          :: Maybe Int
    , rawVehicleBrake               :: Maybe Int
    , rawVehicleTopSpeed            :: Maybe Int
    , rawVehicleManeuver            :: Maybe Int
    , rawVehicleAGI                 :: Maybe Int
    , rawVehicleMythicAGI           :: Maybe Int
    , rawVehicleSTR                 :: Maybe Int
    , rawVehicleMythicSTR           :: Maybe Int
    , rawVehicleBreakpointsWeapon   :: Int
    , rawVehicleBreakpointsMobility :: Int
    , rawVehicleBreakpointsEngine   :: Int
    , rawVehicleBreakpointsOptics   :: Int
    , rawVehicleBreakpointsHull     :: Int
    , rawVehicleArmorFront          :: Int
    , rawVehicleArmorBack           :: Int
    , rawVehicleArmorSide           :: Int
    , rawVehicleArmorTop            :: Int
    , rawVehicleArmorBottom         :: Int
    , rawVehicleShieldIntegrity     :: Maybe Int
    , rawVehicleShieldDelay         :: Maybe Int
    , rawVehicleShieldRecharge      :: Maybe Int
    , rawVehicleAdditionalInfo      :: T.Text
    , rawVehicleDescription         :: T.Text
    , rawVehicleMeleeWeapons        :: [RawMeleeWeapon]
    , rawVehicleRangedWeapons       :: [RawRangedWeapon]
    }

instance CSV.FromNamedRecord RawVehicle where
  parseNamedRecord v = do
    RawVehicle
      <$> v .: "Name"
      <*> v .: "COMP_faction"
      <*> v .: "COMP_price"
      <*> v .: "Comp_experience"
      <*> v .: "COMP_length"
      <*> v .: "Comp_width"
      <*> v .: "Comp_height"
      <*> v .: "Comp_weight"
      <*> (nonEmptyText <$> v .: "Comp_crew")
      <*> (nonEmptyText <$> v .: "Comp_complement")
      <*> v .: "Comp_size_points"
      <*> v .: "Comp_weapon_points"
      <*> v .: "Comp_size_category"
      <*> (positiveInt <$> v .: "Comp_accelerate")
      <*> (positiveInt <$> v .: "Comp_brake")
      <*> (positiveInt <$> v .: "Comp_topspeed")
      <*> (positiveInt <$> v .: "Comp_maneuver")
      <*> (positiveInt <$> v .: "Comp_AGI")
      <*> (positiveInt <$> v .: "Comp_AGI_mythic")
      <*> (positiveInt <$> v .: "Comp_STR")
      <*> (positiveInt <$> v .: "Comp_STR_mythic")
      <*> v .: "Comp_weapon_bpts"
      <*> v .: "Comp_mobility_bpts"
      <*> v .: "Comp_engine_bpts"
      <*> v .: "Comp_optics_bpts"
      <*> v .: "Comp_hull_bpts"
      <*> v .: "Comp_front_armor"
      <*> v .: "Comp_back_armor"
      <*> v .: "Comp_side_armor"
      <*> v .: "Comp_top_armor"
      <*> v .: "Comp_bottom_armor"
      <*> (positiveInt <$> v .: "Comp_shield_rating")
      <*> (positiveInt <$> v .: "Comp_shield_delay")
      <*> (positiveInt <$> v .: "Comp_shield_rate")
      <*> v .: "Comp_aditional_info"
      <*> v .: "COMP_description"
      <*> pure [] -- TODO
      <*> pure [] -- TODO

--
-- Helpers
--
defaultZero :: String -> Int
defaultZero = fromMaybe 0 . readMaybe

defaultZeroDbl :: String -> Double
defaultZeroDbl = fromMaybe 0 . readMaybe

nonEmptyText :: T.Text -> Maybe T.Text
nonEmptyText txt =
  if T.null txt
     then Nothing
     else Just txt

parseBool :: MonadFail m => Int -> m Bool
parseBool 0 = pure False
parseBool 1 = pure True
parseBool x = fail $ "Unrecognized value " <> show x

positiveInt :: String -> Maybe Int
positiveInt str =
  case readMaybe str of
    Just n | n >= 1 -> Just n
    _ -> Nothing
