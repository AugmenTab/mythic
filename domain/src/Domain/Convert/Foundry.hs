module Domain.Convert.Foundry
  ( toFoundry
  ) where

import           Flipstone.Prelude
import           Data.Types
import           Domain.Helpers (tryParseInt)

import qualified Data.List as L
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Traversable (for)
import           Data.Tuple (snd)

toFoundry :: CompendiumMap [RawData]
          -> Either T.Text (CompendiumMap [FoundryData])
toFoundry = Map.traverseWithKey mkFoundry

mkFoundry :: CompendiumData -> [RawData] -> Either T.Text [FoundryData]
mkFoundry (faction, _) rawData = for rawData $ \raw ->
  case raw of
    ArmorData     _ -> Left "Not yet implemented" -- TODO: mkArmor     faction a
    EquipmentData e -> mkEquipment faction e
    MeleeData     _ -> Left "Not yet implemented" -- TODO: mkMelee     faction m
    RangedData    _ -> Left "Not yet implemented" -- TODO: mkRanged    faction r

mkEquipment :: Faction -> RawEquipment -> Either T.Text FoundryData
mkEquipment faction raw = Right $ FoundryEquipment $
  let desc = rawEquipmentDescription raw
      weight =
        Weight
          { weightEach = rawEquipmentWeight raw
          -- No Item supports its own weight as far as the descriptions are
          -- concerned. This can be updated if/when a user points one out or one
          -- gets added to the game.
          , weightSelfSupported = False
          }

   in Equipment
        { equipmentName        = mkName $ rawEquipmentName raw
        , equipmentPrice       = mkItemPrice $ rawEquipmentPrice raw
        , equipmentBreakpoints = mkBreakpoints 0
        , equipmentTrainings   = mkItemTrainings faction Nothing
        , equipmentWeight      = weight
        , equipmentDescription = mkDescription desc
        , equipmentShields     = findShieldsIn desc

        -- At present, there are no Equipment Items that can modify an Actor's
        -- characteristics. This may change in the future, so the value will be
        -- kept hardcoded to `Nothing` here for now.
        , equipmentCharacteristics = Nothing
        }

--
-- Helpers
--
findDelayIn :: [T.Text] -> Maybe ItemAdjustment
findDelayIn txts
  | L.length txts < 3              = Nothing
  | "delay" : "of" : v : _ <- txts = basicItemAdjustment <$> tryParseInt v
  | "delay" : v    : _     <- txts = basicItemAdjustment <$> tryParseInt v
  | otherwise                      = findDelayIn . snd =<< L.uncons txts

findIntegrityIn :: [T.Text] -> Maybe ItemAdjustment
findIntegrityIn txts
  | L.length txts < 3                  = Nothing
  | "rating"    : "of" : v : _ <- txts = basicItemAdjustment <$> tryParseInt v
  | "integrity" : "of" : v : _ <- txts = basicItemAdjustment <$> tryParseInt v
  | "integrity" : v    : _     <- txts = basicItemAdjustment <$> tryParseInt v
  | otherwise                          = findIntegrityIn . snd =<< L.uncons txts

findRechargeIn :: [T.Text] -> Maybe ItemAdjustment
findRechargeIn txts
  | L.length txts < 3                   = Nothing
  | "recharge" : "rate" : v        : _ <- txts = basicItemAdjustment <$> tryParseInt v
  | "recharge" : "of"   : v        : _ <- txts = basicItemAdjustment <$> tryParseInt v
  | "takes"    : v      : "rounds" : _ <- txts = basicItemAdjustment <$> tryParseInt v
  | otherwise                                  = findRechargeIn . snd =<< L.uncons txts

findShieldsIn :: T.Text -> Maybe Shields
findShieldsIn desc =
  let words = T.words $ T.toLower desc
   in case (findIntegrityIn words, findRechargeIn words, findDelayIn words) of
        (Just integrity, mbRecharge, mbDelay) ->
          Just $
            Shields
              { shieldsHas       = True
              , shieldsIntegrity = integrity
              , shieldsRecharge  = fromMaybe emptyItemAdjustment mbRecharge
              , shieldsDelay     = fromMaybe emptyItemAdjustment mbDelay
              }

        _ ->
          Nothing
