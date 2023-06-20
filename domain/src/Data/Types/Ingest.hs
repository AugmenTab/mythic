module Data.Types.Ingest
  ( RawData(..)
  , RawAbility(..)
  , RawArmor(..)
  , RawEquipment(..)
  , RawMeleeWeapon(..)
  , RawRangedBase(..)
  , RawRangedWeapon(..)
  ) where

import           Flipstone.Prelude

import qualified Data.Csv as CSV
import           Data.Csv ((.:))
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           GHC.Types (Double)
import           Text.Read (readMaybe)
import           Text.Show (show)

data RawData
  = AbilityData   RawAbility
  | ArmorData     RawArmor
  | EquipmentData RawEquipment
  | MeleeData     RawMeleeWeapon
  | RangedData    RawRangedWeapon

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
    { rawArmorName        :: T.Text
    , rawArmorFaction     :: T.Text
    , rawArmorHead        :: Int
    , rawArmorArms        :: Int
    , rawArmorChest       :: Int
    , rawArmorLegs        :: Int
    , rawArmorIntegrity   :: Int
    , rawArmorDelay       :: Int
    , rawArmorRecharge    :: Int
    , rawArmorVariant     :: Bool
    , rawArmorWeight      :: Double
    , rawArmorDescription :: T.Text
    , rawArmorPrice       :: Int
    }

instance CSV.FromNamedRecord RawArmor where
  parseNamedRecord a =
    RawArmor <$> a .: "Name"
             <*> a .: "COMP_faction"
             <*> a .: "COMP_armor_head"
             <*> a .: "COMP_armor_arms"
             <*> a .: "COMP_armor_chest"
             <*> a .: "COMP_armor_legs"
             <*> a .: "COMP_shield_integrity"
             <*> a .: "COMP_shield_delay"
             <*> a .: "COMP_shield_recharge"
             <*> (parseBool =<< a .: "COMP_variant")
             <*> a .: "COMP_weight"
             <*> a .: "COMP_description"
             <*> a .: "COMP_price"

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
                 <*> (defaultZero <$> e .: "COMP_weight")
                 <*> e .: "COMP_price"

data RawMeleeWeapon =
  RawMeleeWeapon
    { rawMeleeName         :: T.Text
    , rawMeleeFaction      :: T.Text
    , rawMeleeType         :: T.Text
    , rawMeleeAttr         :: T.Text
    , rawMeleeRange        :: Int
    , rawMeleeDamageRoll   :: T.Text
    , rawMeleeDamageBase   :: Int
    , rawMeleePierce       :: Int
    , rawMeleeHitMod       :: Int
    , rawMeleeBreakpoints  :: Int
    , rawMeleeBaseMod      :: T.Text
    , rawMeleePierceMod    :: T.Text
    , rawMeleeWeight       :: Double
    , rawMeleeSpecialRules :: T.Text
    , rawMeleeDescription  :: T.Text
    , rawMeleePrice        :: Int
    }

instance CSV.FromNamedRecord RawMeleeWeapon where
  parseNamedRecord m =
    RawMeleeWeapon <$> m .: "Name"
                   <*> m .: "COMP_faction"
                   <*> m .: "COMP_type"
                   <*> m .: "COMP_attribute"
                   <*> m .: "COMP_range"
                   <*> m .: "COMP_damage_roll"
                   <*> m .: "COMP_damage_base"
                   <*> m .: "COMP_pierce"
                   <*> m .: "COMP_hitmod"
                   <*> m .: "COMP_breakpoints"
                   <*> m .: "COMP_base_mod"
                   <*> m .: "COMP_pierce_mod"
                   <*> m .: "COMP_weight"
                   <*> m .: "COMP_special_rules"
                   <*> m .: "COMP_description"
                   <*> m .: "COMP_price"

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

--
-- Helpers
--
defaultZero :: String -> Double
defaultZero = fromMaybe 0 . readMaybe

parseBool :: MonadFail m => Int -> m Bool
parseBool 0 = pure False
parseBool 1 = pure True
parseBool x = fail $ "Unrecognized value " <> show x
