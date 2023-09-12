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
    { systemId                :: T.Text
    , title                   :: T.Text
    , description             :: T.Text
    , version                 :: T.Text
    , authors                 :: [Author]
    , esmodules               :: [T.Text]
    , styles                  :: [T.Text]
    , packs                   :: [Compendium FoundryData]
    , languages               :: [Language]
    , initiative              :: T.Text
    , gridDistance            :: Int
    , gridUnits               :: T.Text
    , primaryTokenAttribute   :: T.Text
    , secondaryTokenAttribute :: Maybe T.Text
    , compatibility           :: Compatibility
    , url                     :: T.Text
    , manifest                :: T.Text
    , download                :: T.Text
    , license                 :: T.Text
    , readme                  :: T.Text
    , changelog               :: T.Text
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
    { name    :: T.Text
    , website :: Maybe T.Text
    , email   :: Maybe T.Text
    , discord :: Maybe T.Text
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
    , discord = Just "augmentab"
    }

data Language =
  Language
    { code :: LanguageCodes.ISO639_1
    , path :: T.Text
    }

instance ToJSON Language where
  toJSON l =
    object [ "lang" .= T.toLower (languageCode $ code l)
           , "name" .= languageName l
           , "path" .= path l
           ]

languageCode :: LanguageCodes.ISO639_1 -> T.Text
languageCode language =
  let (f, s) = LanguageCodes.toChars language
   in T.pack [ f, s ]

languageName :: Language -> T.Text
languageName = T.pack . LanguageCodes.language . code

languagePath :: LanguageCodes.ISO639_1 -> T.Text
languagePath language =
  "lang/" <> languageCode language <> ".json"

mkLanguage :: LanguageCodes.ISO639_1 -> Language
mkLanguage =
  Language <$> id
           <*> languagePath

data Compatibility =
  Compatibility
    { minimumVersion  :: T.Text
    , verifiedVersion :: T.Text
    , maximumVersion  :: T.Text
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
    , verifiedVersion = "11"
    , maximumVersion  = "11"
    }

mythicManifest :: [Compendium FoundryData] -> Manifest
mythicManifest compendia =
  let release = "0.3.5"
      mythicDescription =
        T.unwords [ "An unofficial system implementation for playing the"
                  , "fan-made Mythic game on Foundry Virtual Tabletop."
                  ]

   in Manifest
        { systemId                = "mythic"
        , title                   = "Mythic"
        , description             = mythicDescription
        , version                 = release
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
        , download                = "https://github.com/AugmenTab/mythic/releases/download/v" <> release <> "/mythic.zip"
        , license                 = "LICENSE"
        , readme                  = "README.md"
        , changelog               = "https://github.com/AugmenTab/mythic/blob/main/CHANGELOG.md"
        }
