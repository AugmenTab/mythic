module Data.Types.Ingest
  ( RawData(..)
  , RawAbility(..)
  , RawArmor(..)
  , RawEquipment(..)
  , RawFlood(..)
  , RawMeleeBase(..)
  , RawMeleeWeapon(..)
  , RawPermutation(..)
  , RawRangedBase(..)
  , RawRangedWeapon(..)
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
  | EquipmentData   RawEquipment
  | FloodData       RawFlood
  | MeleeData       RawMeleeWeapon
  | PermutationData RawPermutation
  | RangedData      RawRangedWeapon

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
    { rawMeleeName         :: T.Text
    , rawMeleeBases        :: NE.NonEmpty RawMeleeBase
    , rawMeleeFaction      :: T.Text
    , rawMeleeWeight       :: Double
    , rawMeleePrice        :: Int
    }

instance CSV.FromNamedRecord RawMeleeWeapon where
  parseNamedRecord m = do
    let mkBase n =
          RawMeleeBase
            <$> m .: ("Comp_name[" <> n <> "]")
            <*> m .: ("COMP_type[" <> n <> "]")
            <*> m .: ("COMP_attribute[" <> n <> "]")
            <*> m .: ("COMP_range[" <> n <> "]")
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
