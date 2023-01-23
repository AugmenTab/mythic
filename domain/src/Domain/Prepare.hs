module Domain.Prepare
  ( prepareSheet
  ) where

import           Flipstone.Prelude
import qualified Domain.Request as Request
import           Data.Types

import           Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text as Atto
import qualified Data.List as L
import qualified Data.Text as T

prepareSheet :: Request.SheetSubject
             -> [Text]
             -> Either Text [CompendiumData Text]
prepareSheet subject [] =
  Left $ "No content in " <> Request.sheetSubjectText subject

prepareSheet subject (header:lines) =
  let (separateSheet, startingContent) =
        case subject of
      --  Request.ArmorSheet        -> startingArmor -- TODO
          Request.EquipmentSheet    -> (separateEquipment, startingEquipment)
      --  Request.MeleeWeaponSheet  -> startingMelee -- TODO
      --  Request.RangedWeaponSheet -> startingRanged -- TODO

   in Right $ separateSheet header UNSC startingContent [ header ] [] lines

startingEquipment :: CompendiumContent
startingEquipment = mkCompendiumContent "HELMET AND FACIAL EQUIPMENT"

separateEquipment :: Text
                  -> Faction
                  -> CompendiumContent
                  -> [Text]
                  -> [CompendiumData Text]
                  -> [Text]
                  -> [CompendiumData Text]
separateEquipment _      faction content sheet acc [] =
  CompendiumData (faction, content, T.unlines sheet) : acc

separateEquipment header faction content sheet acc (line:lines) =
  case tryParseFactionOrContent line of
    Just (Left  f) ->
      separateEquipment
        header
        f
        content
        [ header ]
        (CompendiumData (faction, content, T.unlines sheet) : acc)
        lines

    Just (Right c)
      | L.length sheet > 1 ->
        separateEquipment
          header
          faction
          c
          [ header ]
          (CompendiumData (faction, content, T.unlines sheet) : acc)
          lines

      | otherwise ->
        separateEquipment header faction c sheet acc lines

    Nothing ->
      separateEquipment header faction content (sheet <> [ line ]) acc lines

tryParseFactionOrContent :: Text -> Maybe (Either Faction CompendiumContent)
tryParseFactionOrContent txt =
  let parser = do
        _ <- Atto.string ","
        faction <-     Atto.string "Equipment"
                   <|> Atto.string "COVENANT"
        content <-     Atto.string "COVENANT"
                   <|> Atto.takeTill (== ',')
        pure (T.strip faction, T.strip content)

   in case Atto.parseOnly parser txt of
        Right ("COVENANT", _) -> Just $ Left Covenant
        Right (_, c) -> Just . Right $ mkCompendiumContent c
        _ -> Nothing
