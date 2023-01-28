module Domain.Persist
  ( writeCompendium
  , writeManifest
  ) where

import           Flipstone.Prelude
import           Data.Types
import           Domain.JSON

import qualified Data.LanguageCodes as LanguageCodes
import qualified Data.List as L
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as IO

writeCompendium :: Compendium FoundryData -> IO ()
writeCompendium compendium = do
  IO.writeFile ("../" <> compendiumPath compendium)
    . T.unlines
    . fmap encodeLine
    . L.sortOn entryName
    $ compendiumEntries compendium

writeManifest :: [Compendium FoundryData] -> IO ()
writeManifest = IO.writeFile "../system.json" . encodePage . mythicManifest

data Manifest =
  Manifest
    { systemId                :: Text
    , title                   :: Text
    , description             :: Text
    , version                 :: Text
    , authors                 :: [Author]
    , esmodules               :: [Text]
    , styles                  :: [Text]
    , packs                   :: [Compendium FoundryData]
    , languages               :: [Language]
    , initiative              :: Text
    , gridDistance            :: Int
    , gridUnits               :: Text
    , primaryTokenAttribute   :: Text
    , secondaryTokenAttribute :: Maybe Text
    , compatibility           :: Compatibility
    , url                     :: Text
    , manifest                :: Text
    , download                :: Text
    , license                 :: Text
    , readme                  :: Text
    , changelog               :: Text
    }

instance ToJSON Manifest where
  toJSON m =
    object [ "id"                      .= systemId                m
           , "title"                   .= title                   m
           , "description"             .= description             m
           , "version"                 .= version                 m
           , "authors"                 .= authors                 m
           , "esmodules"               .= esmodules               m
           , "styles"                  .= styles                  m
           , "packs"                   .= packs                   m
           , "languages"               .= languages               m
           , "initiative"              .= initiative              m
           , "gridDistance"            .= gridDistance            m
           , "gridUnits"               .= gridUnits               m
           , "primaryTokenAttribute"   .= primaryTokenAttribute   m
           , "secondaryTokenAttribute" .= secondaryTokenAttribute m
           , "compatibility"           .= compatibility           m
           , "url"                     .= url                     m
           , "manifest"                .= manifest                m
           , "download"                .= download                m
           , "license"                 .= license                 m
           , "readme"                  .= readme                  m
           , "changelog"               .= changelog               m
           ]

data Author =
  Author
    { name    :: Text
    , website :: Maybe Text
    , email   :: Maybe Text
    , discord :: Maybe Text
    }

instance ToJSON Author where
  toJSON a =
    object $
      catMaybes [ Just ("name" .= name a)
                , (.=) "url"     <$> website a
                , (.=) "email"   <$> email   a
                , (.=) "discord" <$> discord a
                ]

mythicAuthor :: Author
mythicAuthor =
  Author
    { name    = "Tyler Baum"
    , website = Just "https://github.com/AugmenTab"
    , email   = Nothing
    , discord = Just "∀ugmenTab#5063"
    }

data Language =
  Language
    { code :: LanguageCodes.ISO639_1
    , path :: Text
    }

instance ToJSON Language where
  toJSON l =
    object [ "lang" .= T.toLower (languageCode $ code l)
           , "name" .= languageName l
           , "path" .= path l
           ]

languageCode :: LanguageCodes.ISO639_1 -> Text
languageCode language =
  let (f, s) = LanguageCodes.toChars language
   in T.pack [ f, s ]

languageName :: Language -> Text
languageName = T.pack . LanguageCodes.language . code

languagePath :: LanguageCodes.ISO639_1 -> Text
languagePath language =
  "lang/" <> languageCode language <> ".json"

mkLanguage :: LanguageCodes.ISO639_1 -> Language
mkLanguage =
  Language <$> id
           <*> languagePath

data Compatibility =
  Compatibility
    { minimumVersion  :: Text
    , verifiedVersion :: Text
    , maximumVersion  :: Text
    }

instance ToJSON Compatibility where
  toJSON c =
    object [ "minimum"  .= minimumVersion  c
           , "verified" .= verifiedVersion c
           , "maximum"  .= maximumVersion  c
           ]

mythicCompatibility :: Compatibility
mythicCompatibility =
  Compatibility
    { minimumVersion  = "10"
    , verifiedVersion = "10"
    , maximumVersion  = "10"
    }

mythicManifest :: [Compendium FoundryData] -> Manifest
mythicManifest compendia =
  let mythicDescription =
        T.unwords [ "An unofficial system implementation for playing the"
                  , "fan-made Halo: Mythic game on Foundry Virtual Tabletop."
                  ]

   in Manifest
        { systemId                = "mythic"
        , title                   = "Mythic 6.0"
        , description             = mythicDescription
        , version                 = "0.2.4"
        , authors                 = [ mythicAuthor ]
        , esmodules               = [ "mythic.js" ]
        , styles                  = [ "mythic.css" ]
        , packs                   = compendia
        , languages               = [ mkLanguage LanguageCodes.EN ]
        , initiative              = "@initiative.formula"
        , gridDistance            = 1
        , gridUnits               = "m"
        , primaryTokenAttribute   = "wounds"
        , secondaryTokenAttribute = Nothing
        , compatibility           = mythicCompatibility
        , url                     = "https://github.com/AugmenTab/mythic"
        , manifest                = "https://raw.githubusercontent.com/AugmenTab/mythic/main/system.json"
        , download                = "https://github.com/AugmenTab/mythic/releases/latest/download/mythic.zip"
        , license                 = "LICENSE"
        , readme                  = "README.md"
        , changelog               = "https://github.com/AugmenTab/mythic/blob/main/CHANGELOG.md"
        }
