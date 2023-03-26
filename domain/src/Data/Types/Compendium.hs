module Data.Types.Compendium
  ( Compendium(..)
  , Entry(..)
  , ItemID, mkItemID, idText
  , Label, mkCompendiumLabel, labelText
  , mkCompendiumName
  , mkCompendiumPath
  ) where

import           Flipstone.Prelude
import           Data.Types.Prelude
import           Domain.JSON

import qualified Data.Bool as B
import qualified Data.Char as C
import qualified Data.List as L
import           Data.List ((!!))
import qualified Data.Text as T
import           GHC.IO (FilePath)
import           System.Random (mkStdGen, randomRs)

data Compendium item =
  Compendium
    { compendiumName    :: Name
    , compendiumLabel   :: Label
    , compendiumPath    :: FilePath
    , compendiumEntries :: [Entry item]
    }

instance ToJSON (Compendium item) where
  toJSON c =
    object [ "name"    .= compendiumName  c
           , "label"   .= compendiumLabel c
           , "path"    .= compendiumPath  c
           , "private" .= False
           , "type"    .= ("Item" :: T.Text)
           , "system"  .= ("mythic" :: T.Text)
           ]

mkCompendiumName :: Faction -> CompendiumDetails -> Name
mkCompendiumName faction details =
  mkName $ T.toLower $ T.intercalate "-"
    [ T.map (\c -> B.bool c '-' $ C.isSpace c) $ compendiumDetails details
    , factionText faction
    ]

mkCompendiumPath :: Name -> FilePath
mkCompendiumPath name =
  T.unpack $ "packs/" <> nameText name <> ".db"

data Entry item =
  Entry
    { entryId   :: ItemID
    , entryName :: Name
    , entryImg  :: Img
    , entryType :: ItemType
    , entryData :: item
    }

instance (CompendiumEntry item, ToJSON item) => ToJSON (Entry item) where
  toJSON e =
    let ItemID itemId = entryId e
        permissions   =
          object [ "default"          .= valueInt 0
                 , keyFromText itemId .= valueInt 3
                 ]

     in object [ "_id"        .= entryId e
               , "name"       .= entryName e
               , "img"        .= entryImg e
               , "type"       .= entryType e
               , "system"     .= entryData e
               , "flags"      .= emptyObject
               , "effects"    .= emptyArray
               , "permission" .= permissions
               ]

newtype ItemID = ItemID T.Text
  deriving newtype (ToJSON)

idText :: ItemID -> T.Text
idText (ItemID i) = i

-- This represents the range of characters to use when constructing an ID.
idCharacters :: [Char]
idCharacters = [ '0'..'9' ] <> [ 'A'..'Z' ] <> [ 'a'..'z' ]

-- This represents the range of pseudorandom Ints to pick when generating an ID.
-- The numbers chosen from this range will be used as the index to select values
-- from idCharacters to build a seemingly random but reproducible ID.
idRange :: (Int, Int)
idRange = (0, L.length idCharacters - 1)

-- Despite using a "random" numbers, we define the seed with the item's name
-- and label, which allows us to reliably produce the same ID every time the
-- packs are generated.
mkItemID :: Label -> Name -> ItemID
mkItemID (Label label) name = do
  let mkSeed :: T.Text -> Int
      mkSeed txt = T.length txt * sum (C.ord <$> T.unpack txt)
   in   ItemID
      . T.pack
      . fmap (idCharacters !!)
      . L.take 16
      . randomRs idRange
      . mkStdGen
      . mkSeed
      $ label <> " - " <> nameText name

newtype Label = Label T.Text
  deriving newtype (ToJSON)

mkCompendiumLabel :: Faction -> CompendiumDetails -> Label
mkCompendiumLabel faction content =
  Label $ compendiumDetails content <> " - " <> factionText faction

labelText :: Label -> T.Text
labelText (Label l) = l
