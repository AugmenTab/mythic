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
import           Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
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
    ArmorData     a -> mkArmor     faction a
    EquipmentData e -> mkEquipment faction e
    MeleeData     m -> mkMelee     faction m
    RangedData    r -> mkRanged    faction r

mkArmor :: Faction -> RawArmor -> Either T.Text FoundryData
mkArmor _ _ =
  Left "Not yet implemeted"

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
        , equipmentFaction     = faction
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

mkMelee :: Faction -> RawMeleeWeapon -> Either T.Text FoundryData
mkMelee faction raw = Right $ FoundryWeapon $
  let weaponType = rawMeleeType raw
      weaponDetails = Map.lookup (T.toUpper weaponType) weaponDetailsMap
      fireModeMap = Map.singleton NoFireMode $ mkFireRate 0
      weight =
        Weight
          { weightEach = rawMeleeWeight raw
          -- No Item supports its own weight as far as the descriptions are
          -- concerned. This can be updated if/when a user points one out or one
          -- gets added to the game.
          , weightSelfSupported = False
          }

      specialsTxt = rawMeleeSpecialRules raw
      (tags, specials) =
        fromMaybe ([], T.empty)
          $ L.unsnoc
          $ fmap T.strip
          $ T.split (== ']')
          $ T.filter (/= '[') specialsTxt

      (tagSpecials, weaponTags) =
        L.unzip $ mapMaybe (L.unsnoc . T.split (== ' ')) tags

      (unknownSpecials, specialRules) =
        buildSpecials $ T.split (== ',') specials <> fmap T.unwords tagSpecials

      description =
        mkDescription $ T.concat
          [ rawMeleeDescription raw
          , "&#13;&#13;"
          , T.intercalate ". " $ Set.toList unknownSpecials
          ]

   in Weapon
        { weaponName        = mkName $ rawMeleeName raw
        , weaponFaction     = faction
        , weaponDescription = description
        , weaponPrice       = mkItemPrice $ rawMeleePrice raw
        , weaponBreakpoints = mkBreakpoints 0
        , weaponTrainings   = mkItemTrainings faction $ fst <$> weaponDetails
        , weaponWeight      = weight
        , weaponGroup       = fromMaybe MeleeGroup $ snd <$> weaponDetails
        , weaponTags        = buildWeaponTags weaponTags $ rawMeleeAttr raw
        , weaponFireModes   = mkFireModes fireModeMap
        , weaponAttack      = emptyAttack
        , weaponReload      = mkReload 0
        , weaponNickname    = Nothing
        , weaponType        = WeaponType weaponType
        , weaponMagCap      = mkMagazineCapacity 0
        , weaponAmmo        = mkAmmo ""
        , weaponAmmoGroup   = None
        , weaponScopeMag    = Nothing
        , weaponCurrentAmmo = mkName "STD"
        , weaponAmmoList    = mkMeleeSTDAmmo raw specialRules
        , weaponSettings    = emptyWeaponSettings
        , weaponShields     = findShieldsIn specialsTxt

        -- At present, there are no ranged Weapon Items that modify their
        -- wielder's characteristics. This may change in the future, so the
        -- value will be kept hardcoded to `Nothing` here for now.
        , weaponCharacteristics = Nothing
        }

mkRanged :: Faction -> RawRangedWeapon -> Either T.Text FoundryData
mkRanged faction raw = Right $ FoundryWeapon $
  let (name, nickname) = mkNameAndNickname $ rawRangedName raw
      weaponType = rawRangedType raw
      weaponDetails = Map.lookup (T.toUpper weaponType) weaponDetailsMap
      weight =
        Weight
          { weightEach = rawRangedWeight raw
          -- No Item supports its own weight as far as the descriptions are
          -- concerned. This can be updated if/when a user points one out or one
          -- gets added to the game.
          , weightSelfSupported = False
          }

      specialsTxt = rawRangedSpecialRules raw
      (tags, specials) =
        fromMaybe ([], T.empty)
          $ L.unsnoc
          $ fmap T.strip
          $ T.split (== ']')
          $ T.filter (/= '[') specialsTxt

      (tagSpecials, weaponTags) =
        L.unzip $ mapMaybe (L.unsnoc . T.split (== ' ')) tags

      (unknownSpecials, specialRules) =
        buildSpecials $ T.split (== ',') specials <> fmap T.unwords tagSpecials

      description =
        mkDescription $ T.concat
          [ rawRangedDescription raw
          , "&#13;&#13;"
          , T.intercalate ". " $ Set.toList unknownSpecials
          ]

      reload =
        mkReload
          $ maybe 0 (read . T.unpack)
          $ listToMaybe
          $ T.words
          $ rawRangedReload raw

   in Weapon
        { weaponName        = name
        , weaponFaction     = faction
        , weaponDescription = description
        , weaponPrice       = mkItemPrice $ rawRangedPrice raw
        , weaponBreakpoints = mkBreakpoints 0
        , weaponTrainings   = mkItemTrainings faction $ fst <$> weaponDetails
        , weaponWeight      = weight
        , weaponGroup       = fromMaybe Ranged $ snd <$> weaponDetails
        , weaponTags        = buildWeaponTags weaponTags $ rawRangedAttr raw
        , weaponFireModes   = mkFireModeMap $ rawRangedROF raw
        , weaponAttack      = emptyAttack
        , weaponReload      = reload
        , weaponNickname    = nickname
        , weaponType        = WeaponType weaponType
        , weaponMagCap      = mkMagazineCapacity $ rawRangedMagazine raw
        , weaponAmmo        = mkAmmo ""
        , weaponAmmoGroup   = None
        , weaponScopeMag    = Just $ mkScopeMagnification 1
        , weaponCurrentAmmo = mkName "STD"
        , weaponAmmoList    = mkRangedSTDAmmo raw specialRules
        , weaponSettings    = emptyWeaponSettings
        , weaponShields     = findShieldsIn specialsTxt

        -- At present, there are no ranged Weapon Items that modify their
        -- wielder's characteristics. This may change in the future, so the
        -- value will be kept hardcoded to `Nothing` here for now.
        , weaponCharacteristics = Nothing
        }

--
-- Helpers
--
buildWeaponTags :: [T.Text] -> T.Text -> WeaponTags
buildWeaponTags tags attr =
  let updateTags set = maybe set (flip Set.insert set) . weaponTagFromText
      startingSet =
        case attr of
          "WFR" -> Set.insert UD Set.empty
          _     -> Set.empty

   in WeaponTags $ L.foldl' updateTags startingSet tags

buildSpecials :: [T.Text] -> (Set.Set T.Text, SpecialRules)
buildSpecials specials =
  let extract = T.takeWhile (/= ')') . T.drop 1 . T.dropWhile (/= '(')
      extractInt = tryParseInt . extract
      updateSpecialRules (unk, rules) txt =
        case T.strip . T.toLower <$> T.words txt of
          "acid"        : _ -> (unk, rules { acid             = extractInt txt })
          "blast"       : _ -> (unk, rules { blast            = extractInt txt })
          "cauterise"   : _ -> (unk, rules { cauterize        = Just () })
          "cauterize"   : _ -> (unk, rules { cauterize        = Just () })
          "charge"      : _ -> (unk, rules { chargeRule       = extractInt txt })
          "cryo"        : _ -> (unk, rules { cryo             = Just $ extract txt })
          "dice"        : _ -> (unk, rules { diceMinimum      = extractInt txt })
          "electrified" : _ -> (unk, rules { electrified      = Just $ extract txt })
          "emp"         : _ -> (unk, rules { emp              = extractInt txt })
          "flame"       : _ -> (unk, rules { flame            = Just $ extract txt })
          "flashbang"   : _ -> (unk, rules { flashbang        = Just () })
          "gravimetric" : _ -> (unk, rules { gravimetricPulse = extractInt txt })
          "gravity"     : _ -> (unk, rules { gravity          = extractInt txt })
          "hardlight"   : _ -> (unk, rules { hardlight        = Just () })
          "headshot"    : _ -> (unk, rules { headshot         = Just () })
          "homing"      : _ -> (unk, rules { homing           = extractInt txt })
          "kill"        : _ -> (unk, rules { kill             = extractInt txt })
          "kinetic"     : _ -> (unk, rules { kinetic          = Just () })
          "long"        : _ -> (unk, rules { longBarrel       = Just () })
          "needle"      : _ -> (unk, rules { needle           = extractInt txt })
          "nonlethal"   : _ -> (unk, rules { nonlethal        = Just () })
          "overheat"    : _ -> (unk, rules { overheat         = extractInt txt })
          "penetrating" : _ -> (unk, rules { penetrating      = Just () })
          "recharge"    : _ -> (unk, rules { rechargeRate     = extractInt txt })
          "single"      : _ -> (unk, rules { singleLoading    = Just () })
          "slow"        : _ -> (unk, rules { slow             = Just () })
          "smoke"       : _ -> (unk, rules { smoke            = extractInt txt })
          "spike"       : _ -> (unk, rules { spike            = Just () })
          "spin"        : _ -> (unk, rules { spin             = extractInt txt })
          "spread"      : _ -> (unk, rules { spread           = Just () })
          "sticky"      : _ -> (unk, rules { sticky           = Just () })
          "stun"        : _ -> (unk, rules { stun             = extractInt txt })
          "tear"        : _ -> (unk, rules { tearGas          = Just () })
          "tranquilize" : _ -> (unk, rules { tranquilize      = extractInt txt })
          "vehicle"     : _ -> (unk, rules { vehicleLock      = Just () })
          _                 -> (Set.insert txt unk, rules)

   in L.foldl' updateSpecialRules (Set.empty, emptySpecialRules) specials

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
  | "rate"     : "of"   : v        : _ <- txts = basicItemAdjustment <$> tryParseInt v
  | "recharge" : "of"   : v        : _ <- txts = basicItemAdjustment <$> tryParseInt v
  | "recharge" : "rate" : v        : _ <- txts = basicItemAdjustment <$> tryParseInt v
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

mkMeleeSTDAmmo :: RawMeleeWeapon -> SpecialRules -> AmmoList
mkMeleeSTDAmmo raw specials =
  let (diceQuantity, diceValue) = parseDiceValues $ rawMeleeDamageRoll raw
      range = emptyWeaponRange { weaponRangeMelee = rawMeleeRange raw }
   in mkAmmoList $
        Ammunition
          { ammunitionName         = mkName "STD"
          , ammunitionAttackBonus  = rawMeleeHitMod raw
          , ammunitionDiceQuantity = diceQuantity
          , ammunitionDiceValue    = diceValue
          , ammunitionBaseDamage   = rawMeleeDamageBase raw
          , ammunitionSTRDamage    = strengthMultiplierFromText
                                   $ rawMeleeBaseMod raw
          , ammunitionPiercing     = rawMeleePierce raw
          , ammunitionSTRPiercing  = strengthMultiplierFromText
                                   $ rawMeleePierceMod raw
          , ammunitionTarget       = 0
          , ammunitionCurrentMag   = 0
          , ammunitionCritsOn      = 10
          , ammunitionRange        = range
          , ammunitionDescription  = mkDescription ""
          , ammunitionSpecials     = specials
          }

mkNameAndNickname :: T.Text -> (Name, Maybe Name)
mkNameAndNickname txt =
  -- It is unfortunate that we have to use the partial `T.splitOn` here, but
  -- some weapon names naturally contain a hyphen so we can't rely on any one
  -- specific character for delimiting the weapon name. However, since
  -- `T.splitOn` raising an exception would mean the name itself is empty, it's
  -- acceptable that the program would crash on an uncaught exception here
  -- since we should never accept an empty value for the weapon name.
  let prepare =
        L.unsnoc
          . fmap (T.strip . T.filter (/= '\"'))
          . L.filter (not . T.null)
          . T.splitOn " - "

   in case prepare txt of
        Just (name, nick)
          | L.null name ->
            (mkName nick, Nothing)

          | otherwise ->
            (mkName $ T.intercalate " - " name, Just $ mkName nick)

        Nothing ->
          (mkName txt, Nothing)

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

      finalSpecials =
        if T.isPrefixOf "SINGLE LOADING" $ T.toUpper $ rawRangedType raw
           then specials { singleLoading = Just () }
           else specials

   in mkAmmoList $
        Ammunition
          { ammunitionName         = mkName "STD"
          , ammunitionAttackBonus  = rawRangedHitMod raw
          , ammunitionDiceQuantity = diceQuantity
          , ammunitionDiceValue    = diceValue
          , ammunitionBaseDamage   = rawRangedDamageBase raw
          , ammunitionSTRDamage    = Nothing
          , ammunitionPiercing     = rawRangedPierce raw
          , ammunitionSTRPiercing  = Nothing
          , ammunitionTarget       = 0
          , ammunitionCurrentMag   = rawRangedMagazine raw
          , ammunitionCritsOn      = 10
          , ammunitionRange        = range
          , ammunitionDescription  = mkDescription ""
          , ammunitionSpecials     = finalSpecials
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
    , ( "AXE"                   , (Melee   , MeleeGroup) )
    , ( "BEAM"                  , (Advanced, Ranged) )
    , ( "CANNON"                , (Cannon  , Ranged) )
    , ( "CARBINE"               , (Infantry, Ranged) )
    , ( "CHEMICAL SPRAYER"      , (Advanced, Ranged) )
    , ( "CLUB"                  , (Melee   , MeleeGroup) )
    , ( "COILGUN"               , (Cannon  , Ranged) )
    , ( "DAGGER"                , (Melee   , MeleeGroup) )
    , ( "DEMOLITION"            , (Ordnance, Thrown) )
    , ( "DEMOLITIONS"           , (Ordnance, Thrown) )
    , ( "ENERGY WEAPON"         , (Advanced, Ranged) )
    , ( "FIST WEAPON"           , (Melee   , MeleeGroup) )
    , ( "GARROTE"               , (Melee   , MeleeGroup) )
    , ( "GRENADE"               , (Infantry, Thrown) )
    , ( "GRENADE LAUNCHER"      , (Launcher, Ranged) )
    , ( "HAMMER"                , (Melee   , MeleeGroup) )
    , ( "HEAVY MACHINE GUN"     , (Heavy   , Ranged) )
    , ( "KNIFE"                 , (Basic   , MeleeGroup) )
    , ( "LANDMINE"              , (Ordnance, Thrown) )
    , ( "LIGHT MACHINE GUN"     , (Heavy   , Ranged) )
    , ( "MACE"                  , (Melee   , MeleeGroup) )
    , ( "MACHINE GUN"           , (Heavy   , Ranged) )
    , ( "MAGAZINE SHOTGUN"      , (Basic   , Ranged) )
    , ( "MELEE SHIELD"          , (Melee   , MeleeGroup) )
    , ( "MISSILE LAUNCHER"      , (Launcher, Ranged) )
    , ( "MORTAR CANNON"         , (Cannon  , Ranged) )
    , ( "ONE-HANDED SWORD"      , (Melee   , MeleeGroup) )
    , ( "ORDINANCE"             , (Ordnance, Ranged) )
    , ( "PISTOL"                , (Basic   , Ranged) )
    , ( "POLEARM AXE"           , (Melee   , MeleeGroup) )
    , ( "POLEARM SPIKE"         , (Melee   , MeleeGroup) )
    , ( "RAILGUN"               , (Advanced, Ranged) )
    , ( "RIFLE"                 , (Infantry, Ranged) )
    , ( "ROCKET LAUNCHER"       , (Launcher, Ranged) )
    , ( "SATCHEL CHARGE"        , (Ordnance, Thrown) )
    , ( "SHOVEL"                , (Melee   , MeleeGroup) )
    , ( "SINGLE LOADING SHOTGUN", (Basic   , Ranged) )
    , ( "SMG"                   , (Infantry, Ranged) )
    , ( "SNIPER RIFLE"          , (Range   , Ranged) )
    , ( "SPRAY WEAPON"          , (Melee   , MeleeGroup) )
    , ( "TASER"                 , (Melee   , MeleeGroup) )
    , ( "TWO-HANDED SWORD"      , (Melee   , MeleeGroup) )
    ]

