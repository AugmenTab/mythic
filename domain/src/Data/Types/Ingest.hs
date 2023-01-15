module Data.Types.Ingest
  ( RawArmor(..)
  , RawEquipment(..)
  , RawMeleeWeapon(..)
  , RawRangedWeapon(..)
  ) where

import           Flipstone.Prelude

import qualified Data.Csv as CSV
import           Data.Csv ((.:))
import           GHC.Types (Double)
import           Text.Read (read)
import           Text.Show (show)

data RawArmor =
  RawArmor
    { rawArmorName        :: Text
    , rawArmorHead        :: Int
    , rawArmorArms        :: Int
    , rawArmorChest       :: Int
    , rawArmorLegs        :: Int
    , rawArmorIntegrity   :: Int
    , rawArmorDelay       :: Int
    , rawArmorRecharge    :: Int
    , rawArmorVariant     :: Bool
    , rawArmorWeight      :: Double
    , rawArmorDescription :: Text
    , rawArmorPrice       :: Int
    }

instance CSV.FromNamedRecord RawArmor where
  parseNamedRecord a =
    RawArmor <$> a .: "COMP_name"
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
    { rawEquipmentName        :: Text
    , rawEquipmentField       :: Text
    , rawEquipmentDescription :: Text
    , rawEquipmentWeight      :: Double
    , rawEquipmentPrice       :: Int
    }

instance CSV.FromNamedRecord RawEquipment where
  parseNamedRecord e =
    RawEquipment <$> e .: "COMP_name"
                 <*> e .: "COMP_field"
                 <*> e .: "COMP_description"
                 <*> (defaultZero <$> e .: "COMP_weight")
                 <*> e .: "COMP_price"

data RawMeleeWeapon =
  RawMeleeWeapon
    { rawMeleeName         :: Text
    , rawMeleeType         :: Text
    , rawMeleeRange        :: Int
    , rawMeleeDamageRoll   :: Text
    , rawMeleeDamageBase   :: Int
    , rawMeleePierce       :: Int
    , rawMeleeHitMod       :: Int
    , rawMeleeROF          :: Double
    , rawMeleeBreakpoints  :: Int
    , rawMeleeBaseMod      :: Text
    , rawMeleePierceMod    :: Text
    , rawMeleeWeight       :: Double
    , rawMeleeSpecialRules :: Text
    , rawMeleeDescription  :: Text
    , rawMeleePrice        :: Int
    }

instance CSV.FromNamedRecord RawMeleeWeapon where
  parseNamedRecord m =
    RawMeleeWeapon <$> m .: "COMP_name"
                   <*> m .: "COMP_type"
                   <*> m .: "COMP_range"
                   <*> m .: "COMP_damage_roll"
                   <*> m .: "COMP_damage_base"
                   <*> m .: "COMP_pierce"
                   <*> m .: "COMP_hitmod"
                   <*> m .: "COMP_rof"
                   <*> m .: "COMP_breakpoints"
                   <*> m .: "COMP_base-mod"
                   <*> m .: "COMP_pierce-mod"
                   <*> m .: "COMP_weight"
                   <*> m .: "COMP_special_rules"
                   <*> m .: "COMP_description"
                   <*> m .: "COMP_price"

data RawRangedWeapon =
  RawRangedWeapon
    { rawRangedName         :: Text
    , rawRangedType         :: Text
    , rawRangedRange        :: Text
    , rawRangedDamageRoll   :: Text
    , rawRangedDamageBase   :: Int
    , rawRangedPierce       :: Int
    , rawRangedHitMod       :: Int
    , rawRangedMagazine     :: Text
    , rawRangedROF          :: Text
    , rawRangedReload       :: Text
    , rawRangedWeight       :: Double
    , rawRangedSpecialRules :: Text
    , rawRangedDescription  :: Text
    , rawRangedPrice        :: Int
    }

instance CSV.FromNamedRecord RawRangedWeapon where
  parseNamedRecord r =
    RawRangedWeapon <$> r .: "COMP_name"
                    <*> r .: "COMP_type"
                    <*> r .: "COMP_range"
                    <*> r .: "COMP_damage_roll"
                    <*> r .: "COMP_damage_base"
                    <*> r .: "COMP_pierce"
                    <*> r .: "COMP_hitmod"
                    <*> r .: "COMP_magazine"
                    <*> r .: "COMP_rof"
                    <*> r .: "COMP_reload"
                    <*> r .: "COMP_weight"
                    <*> r .: "COMP_special_rules"
                    <*> r .: "COMP_description"
                    <*> r .: "COMP_price"

defaultZero :: String -> Double
defaultZero ""  = 0
defaultZero str = read str

parseBool :: MonadFail m => Int -> m Bool
parseBool 0 = pure False
parseBool 1 = pure True
parseBool x = fail $ "Unrecognized value " <> show x
