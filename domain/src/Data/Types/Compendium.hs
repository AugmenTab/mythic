module Data.Types.Compendium
  ( Compendium(..)
  , Entry(..)
  , EntryID, mkEntryID, idText
  , Label, mkCompendiumLabel, mkItemLabel, labelText
  , mkCompendiumName
  , mkCompendiumPath
  ) where

import           Flipstone.Prelude
import           Data.Types.Prelude
import           Data.Types.Foundry (CompendiumEntry(..), FoundryData)
import           Domain.JSON

import qualified Data.Bool as B
import qualified Data.Char as C
import           Data.Hashable (hash)
import qualified Data.List as L
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import           Data.Vector (Vector, (!))
import qualified Data.Vector as V
import           GHC.IO (FilePath)
import           System.Random (mkStdGen, randomRs)

data Compendium item =
  Compendium
    { compendiumName    :: Name
    , compendiumLabel   :: Label
    , compendiumPath    :: FilePath
    , compendiumType    :: EntryType
    , compendiumEntries :: [Entry item]
    }

instance ToJSON (Compendium item) where
  toJSON c =
    object [ "name"    .= compendiumName  c
           , "label"   .= compendiumLabel c
           , "path"    .= compendiumPath  c
           , "private" .= False
           , "type"    .= entryTypeText (compendiumType c)
           , "system"  .= ("mythic" :: T.Text)
           ]

mkCompendiumName :: Maybe Faction -> CompendiumDetails -> Name
mkCompendiumName mbFaction details =
  mkName
    . T.toLower
    . T.intercalate "-"
    $ catMaybes
        [ Just
            . T.map (\c -> B.bool c '-' $ C.isSpace c)
            $ compendiumDetails details
        , factionText <$> mbFaction
        ]

mkCompendiumPath :: Name -> FilePath
mkCompendiumPath name =
  T.unpack $ "packs/" <> nameText name <> ".db"

data Entry entry =
  Entry
    { entryId    :: EntryID
    , entryName  :: Name
    , entryImg   :: Img
    , entryType  :: EntryType
    , entryData  :: entry
    , entryToken :: Maybe Token
    , entryItems :: [Entry FoundryData]
    }

instance (CompendiumEntry item, ToJSON item) => ToJSON (Entry item) where
  toJSON e =
    let EntryID _id = entryId e
        ownership =
          object [ "default"       .= valueInt 0
                 , keyFromText _id .= valueInt 3
                 ]

     in case entryType e of
          FoundryActor _ ->
            object
              [ "_id"            .= entryId e
              , "name"           .= entryName e
              , "img"            .= entryImg e
              , "type"           .= entryType e
              , "system"         .= entryData e
              , "prototypeToken" .= entryToken e
              , "items"          .= entryItems e
              , "flags"          .= emptyObject
              , "effects"        .= emptyArray
              , "folder"         .= nullJSON
              , "sort"           .= valueInt 0
              , "ownership"      .= ownership
              ]

          FoundryItem _ ->
            object
              [ "_id"       .= entryId e
              , "name"      .= entryName e
              , "img"       .= entryImg e
              , "type"      .= entryType e
              , "system"    .= entryData e
              , "flags"     .= emptyObject
              , "effects"   .= emptyArray
              , "ownership" .= ownership
              ]

newtype EntryID = EntryID T.Text
  deriving newtype (ToJSON)

idText :: EntryID -> T.Text
idText (EntryID i) = i

-- This represents the range of characters to use when constructing an ID.
idCharacters :: Vector Char
idCharacters = V.fromList $ [ '0'..'9' ] <> [ 'A'..'Z' ] <> [ 'a'..'z' ]

-- This represents the range of pseudorandom Ints to pick when generating an ID.
-- The numbers chosen from this range will be used as the index to select values
-- from idCharacters to build a seemingly random but reproducible ID.
idRange :: (Int, Int)
idRange = (0, V.length idCharacters - 1)

-- Despite using a "random" numbers, we define the seed with a hash built from
-- the label and name, which allows us to reliably produce the same ID every
-- time the packs are generated as long as the item and compendium names remain
-- the same. This also allows us to generate different IDs for the same item if
-- it appears in different compendia.
mkEntryID :: Label -> Name -> EntryID
mkEntryID (Label label) name = do
  EntryID
    . T.pack
    . fmap (idCharacters !)
    . L.take 16
    . randomRs idRange
    . mkStdGen
    . hash
    $ label <> " - " <> nameText name

newtype Label = Label T.Text
  deriving newtype (ToJSON)

mkCompendiumLabel :: Maybe Faction -> CompendiumDetails -> Label
mkCompendiumLabel mbFaction content =
  Label
    . T.intercalate " - "
    $ catMaybes
        [ Just $ compendiumDetails content
        , factionText <$> mbFaction
        ]

mkItemLabel :: Label -> Name -> Label
mkItemLabel (Label label) name =
  Label $ T.intercalate " - " [ label, nameText name ]

labelText :: Label -> T.Text
labelText (Label l) = l
