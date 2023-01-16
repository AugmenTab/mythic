module Data.Types.Compendium
  ( Compendium
  , Entry
  , Img
  , ItemID
  ) where

import           Flipstone.Prelude
import           Data.Types.Prelude
import           Domain.JSON

newtype Compendium item = Compendium [Entry item]

data Entry item =
  Entry
    { entryId   :: ItemID
    , entryName :: Name
    , entryImg  :: Img
    , entryType :: ItemType
    , entryData :: item
    }

instance ToJSON item => ToJSON (Entry item) where
  toJSON e =
    object [ "_id"        .= entryId e
           , "name"       .= entryName e
           , "img"        .= entryImg e
           , "type"       .= entryType e
           , "system"     .= entryData e
           , "flags"      .= emptyObject
           , "effects"    .= emptyArray
           , "permission" .= object [ "default" .= defaultInt 0 ]
           ]

newtype Img = Img Text
  deriving newtype (ToJSON)

newtype ItemID = ItemID Text
  deriving newtype (ToJSON)
