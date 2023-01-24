module Domain.Convert.Foundry
  ( toFoundry
  ) where

import           Flipstone.Prelude
import           Data.Types

import qualified Data.Map as Map
import           Data.Traversable (for)

toFoundry :: CompendiumMap [RawData]
          -> Either Text (CompendiumMap [FoundryData])
toFoundry = Map.traverseWithKey mkFoundry

mkFoundry :: CompendiumData -> [RawData] -> Either Text [FoundryData]
mkFoundry (faction, _) rawData = for rawData $ \raw ->
  case raw of
    ArmorData     _ -> Left "Not yet implemented" -- TODO: mkArmor     faction a
    EquipmentData e -> mkEquipment faction e
    MeleeData     _ -> Left "Not yet implemented" -- TODO: mkMelee     faction m
    RangedData    _ -> Left "Not yet implemented" -- TODO: mkRanged    faction r

mkEquipment :: Faction -> RawEquipment -> Either Text FoundryData
mkEquipment faction raw = Right $ FoundryEquipment $
  let desc = rawEquipmentDescription raw
      weight =
        Weight
          { weightEach = rawEquipmentWeight raw
          -- No Item supports its own weight as far as the descriptions are
          -- concerned. This can be updated if/when a user points one out or one
          -- gets added to teh game.
          , weightSelfSupported = False
          }

   in Equipment
        { equipmentName        = mkName $ rawEquipmentName raw
        , equipmentPrice       = mkItemPrice $ rawEquipmentPrice raw
        , equipmentBreakpoints = mkBreakpoints 0
        , equipmentTrainings   = mkItemTrainings faction Nothing
        , equipmentWeight      = weight
        , equipmentDescription = mkDescription desc
        }
