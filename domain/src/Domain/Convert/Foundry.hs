module Domain.Convert.Foundry
  ( toFoundry
  ) where

import           Flipstone.Prelude
import           Data.Types

import           Data.Traversable (for, sequence)

toFoundry :: CompendiumMap [RawData]
          -> Either Text (CompendiumMap [FoundryData])
toFoundry = traverse mkFoundry

mkFoundry :: [RawData] -> Either Text [FoundryData]
mkFoundry rawData = for rawData $ \raw ->
  case raw of
 -- ArmorData     a -> mkArmor     a
    EquipmentData e -> mkEquipment e
 -- MeleeData     m -> mkMelee     m
 -- RangedData    r -> mkRanged    r

mkEquipment :: RawEquipment -> Either Text FoundryData
mkEquipment raw = do
  pure $ FoundryEquipment $
    Equipment
      { equipmentName        = mkName $ rawEquipmentName raw
      , equipmentPrice       = undefined
      , equipmentBreakpoints = mkBreakpoints 0
      , equipmentTrainings   = undefined
      , equipmentWeight      = undefined
      , equipmentDescription = mkDescription $ rawEquipmentDescription raw
      }
