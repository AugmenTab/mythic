module Domain.Convert.Foundry
  ( toFoundry
  ) where

import           Flipstone.Prelude
import           Data.Types
import           Domain.Helpers (tryParseInt)

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Extra as L
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Traversable (for)
import           Data.Tuple (fst, snd)
import           Text.Read (read)

toFoundry :: CompendiumMap [RawData]
          -> Either T.Text (CompendiumMap [FoundryData])
toFoundry = Map.traverseWithKey mkFoundry

mkFoundry :: CompendiumData -> [RawData] -> Either T.Text [FoundryData]
mkFoundry (faction, _) rawData = for rawData $ \raw ->
  case raw of
    ArmorData     _ -> Left "Not yet implemented" -- TODO: mkArmor     faction a
    EquipmentData e -> mkEquipment faction e
    MeleeData     _ -> Left "Not yet implemented" -- TODO: mkMelee     faction m
    RangedData    r -> mkRanged    faction r

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

mkRanged :: Faction -> RawRangedWeapon -> Either T.Text FoundryData
mkRanged faction raw = Right $ FoundryWeapon $
  let weaponType = rawRangedType raw
      weaponDetails = Map.lookup (T.toUpper weaponType) weaponDetailsMap
      weight =
        Weight
          { weightEach = rawRangedWeight raw
          -- No Item supports its own weight as far as the descriptions are
          -- concerned. This can be updated if/when a user points one out or one
          -- gets added to the game.
          , weightSelfSupported = False
          }

      (weaponTags, specials) =
        fromMaybe ([], T.empty)
          $ L.unsnoc
          $ fmap T.strip
          $ T.split (== ']')
          $ T.filter (/= '[')
          $ rawRangedSpecialRules raw

      reload =
        mkReload
          $ maybe 0 (read . T.unpack)
          $ listToMaybe
          $ T.words
          $ rawRangedReload raw

   in Weapon
        { weaponName        = mkName $ rawRangedName raw
        , weaponFaction     = factionText faction
        , weaponDescription = mkDescription $ rawRangedDescription raw
        , weaponPrice       = mkItemPrice $ rawRangedPrice raw
        , weaponBreakpoints = mkBreakpoints 0
        , weaponTrainings   = mkItemTrainings faction $ fst <$> weaponDetails
        , weaponWeight      = weight
        , weaponGroup       = fromMaybe Ranged $ snd <$> weaponDetails
        , weaponTags        = buildWeaponTags weaponTags
        , weaponFireModes   = mkFireModeMap $ rawRangedROF raw
        , weaponAttack      = emptyAttack
        , weaponReload      = reload
        , weaponNickname    = Nothing
        , weaponType        = mkWeaponType weaponType
        , weaponMagCap      = mkMagazineCapacity $ rawRangedMagazine raw
        , weaponAmmo        = mkAmmo ""
        , weaponAmmoGroup   = None
        , weaponScopeMag    = Just $ mkScopeMagnification 1
        , weaponCurrentAmmo = mkName "STD"
        , weaponAmmoList    = mkRangedSTDAmmo raw $ buildSpecials specials
        , weaponSettings    = emptyWeaponSettings

        -- At present, there are no ranged Weapon Items that either confer a
        -- shield to their wielder or modify their wielder's characteristics.
        -- This may change in the future, so the value will be kept hardcoded to
        -- `Nothing` here for now.
        , weaponShields         = Nothing
        , weaponCharacteristics = Nothing
        }

--
-- Helpers
--
buildWeaponTags :: [T.Text] -> WeaponTags
buildWeaponTags tags =
  let updateTags set = maybe set (flip Set.insert set) . weaponTagFromText
   in WeaponTags $ L.foldl' updateTags Set.empty tags

buildSpecials :: T.Text -> SpecialRules
buildSpecials specials =
  let extract = T.takeWhile (/= ')') . T.drop 1 . T.dropWhile (/= '(')
      extractInt = tryParseInt . extract
      updateSpecialRules rules txt =
        case T.strip . T.toLower <$> T.words txt of
          -- TODO: Ensure the checks for each special rule before completing.
          "acid"        : _ -> rules { acid             = extractInt txt }
          "blast"       : _ -> rules { blast            = extractInt txt }
          "cauterize"   : _ -> rules { cauterize        = Just () }
          "charge"      : _ -> rules { chargeRule       = extractInt txt }
          "cryo"        : _ -> rules { cryo             = Just $ extract txt }
          "dice"        : _ -> rules { diceMinimum      = extractInt txt }
          "electrified" : _ -> rules { electrified      = Just $ extract txt }
          "emp"         : _ -> rules { emp              = extractInt txt }
          "flame"       : _ -> rules { flame            = Just $ extract txt }
          "flashbang"   : _ -> rules { flashbang        = Just () }
          "gravimetric" : _ -> rules { gravimetricPulse = extractInt txt }
          "gravity"     : _ -> rules { gravity          = extractInt txt }
          "hardlight"   : _ -> rules { hardlight        = Just () }
          "headshot"    : _ -> rules { headshot         = Just () }
          "homing"      : _ -> rules { homing           = extractInt txt }
          "kill"        : _ -> rules { kill             = extractInt txt }
          "kinetic"     : _ -> rules { kinetic          = Just () }
          "long"        : _ -> rules { longBarrel       = Just () }
          "needle"      : _ -> rules { needle           = extractInt txt }
          "nonlethal"   : _ -> rules { nonlethal        = Just () }
          "overheat"    : _ -> rules { overheat         = extractInt txt }
          "penetrating" : _ -> rules { penetrating      = Just () }
          "recharge"    : _ -> rules { rechargeRate     = extractInt txt }
          "single"      : _ -> rules { singleLoading    = Just () }
          "slow"        : _ -> rules { slow             = Just () }
          "smoke"       : _ -> rules { smoke            = extractInt txt }
          "spike"       : _ -> rules { spike            = Just () }
          "spin"        : _ -> rules { spin             = extractInt txt }
          "spread"      : _ -> rules { spread           = Just () }
          "sticky"      : _ -> rules { sticky           = Just () }
          "stun"        : _ -> rules { stun             = extractInt txt }
          "tear"        : _ -> rules { tearGas          = Just () }
          "tranquilize" : _ -> rules { tranquilize      = extractInt txt }
          "vehicle"     : _ -> rules { vehicleLock      = Just () }
          _                 -> rules

   in L.foldl' updateSpecialRules emptySpecialRules $ T.split (== ',') specials

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

mkFireModeMap :: T.Text -> FireModes
mkFireModeMap modes =
  let updateFireModeMap modeMap txt =
        case T.strip <$> T.words txt of
          mbMode : mbRate : _
            | Just mode <- fireModeFromText mbMode
            , Just rate <- mkFireRate <$> tryParseInt mbRate ->
              Map.insert mode rate modeMap

          _ -> modeMap

   in mkFireModes $ foldl' updateFireModeMap Map.empty $ T.split (== ',') modes

mkRangedSTDAmmo :: RawRangedWeapon -> SpecialRules -> AmmoList
mkRangedSTDAmmo raw specials =
  let (diceQuantity, diceValue) = parseDiceValues $ rawRangedDamageRoll raw
      range =
        case T.split (== '-') $ T.filter (/= 'm') $ rawRangedRange raw of
          close : long : []
            | L.all (not . T.null) [ close, long ] ->
              emptyWeaponRange
                { weaponRangeClose = read $ T.unpack close
                , weaponRangeLong  = read $ T.unpack long
                }

          _ ->
            emptyWeaponRange

   in mkAmmoList $
        Ammunition
          { ammunitionName         = mkName "STD"
          , ammunitionAttackBonus  = rawRangedHitMod raw
          , ammunitionDiceQuantity = diceQuantity
          , ammunitionDiceValue    = diceValue
          , ammunitionBaseDamage   = rawRangedDamageBase raw
          , ammunitionSTRDamage    = 0
          , ammunitionPiercing     = rawRangedPierce raw
          , ammunitionSTRPiercing  = 0
          , ammunitionTarget       = 0
          , ammunitionCurrentMag   = rawRangedMagazine raw
          , ammunitionCritsOn      = 10
          , ammunitionRange        = range
          , ammunitionDescription  = mkDescription ""
          , ammunitionSpecials     = specials
          }

parseDiceValues :: T.Text -> (Int, Int)
parseDiceValues dmgRoll =
  case T.split (== 'D') dmgRoll of
    qty : val : _
      | L.all T.null [ qty, val ] -> (1, 10)
      | qty == "0.5", val == "10" -> (1, 5)
      | otherwise ->
        ( read $ T.unpack qty
        , read $ T.unpack $ T.takeWhile C.isDigit val
        )

    _ -> (0, 0)

weaponDetailsMap :: Map.Map T.Text (EquipmentTraining, WeaponGroup)
weaponDetailsMap =
  Map.fromList
    [ ( "AUTOCANNON"            , (Cannon  , Ranged) )
    , ( "BEAM"                  , (Advanced, Ranged) )
    , ( "CANNON"                , (Cannon  , Ranged) )
    , ( "CARBINE"               , (Infantry, Ranged) )
    , ( "CHEMICAL SPRAYER"      , (Advanced, Ranged) )
    , ( "COILGUN"               , (Cannon  , Ranged) )
    , ( "DEMOLITION"            , (Ordnance, Thrown) )
    , ( "DEMOLITIONS"           , (Ordnance, Thrown) )
    , ( "ENERGY WEAPON"         , (Advanced, Ranged) )
    , ( "GRENADE"               , (Infantry, Thrown) )
    , ( "GRENADE LAUNCHER"      , (Launcher, Ranged) )
    , ( "HEAVY MACHINE GUN"     , (Heavy   , Ranged) )
    , ( "KNIFE"                 , (Basic   , MeleeGroup) )
    , ( "LANDMINE"              , (Ordnance, Thrown) )
    , ( "LIGHT MACHINE GUN"     , (Heavy   , Ranged) )
    , ( "MACHINE GUN"           , (Heavy   , Ranged) )
    , ( "MAGAZINE SHOTGUN"      , (Basic   , Ranged) )
    , ( "MISSILE LAUNCHER"      , (Launcher, Ranged) )
    , ( "MORTAR CANNON"         , (Cannon  , Ranged) )
    , ( "ORDINANCE"             , (Ordnance, Ranged) )
    , ( "PISTOL"                , (Basic   , Ranged) )
    , ( "RAILGUN"               , (Advanced, Ranged) )
    , ( "RIFLE"                 , (Infantry, Ranged) )
    , ( "ROCKET LAUNCHER"       , (Launcher, Ranged) )
    , ( "SATCHEL CHARGE"        , (Ordnance, Thrown) )
    , ( "SINGLE LOADING SHOTGUN", (Basic   , Ranged) )
    , ( "SMG"                   , (Infantry, Ranged) )
    , ( "SNIPER RIFLE"          , (Range   , Ranged) )
    ]
