module Domain.Convert.Foundry
  ( toFoundry
  ) where

import           Flipstone.Prelude
import           Data.Types
import           Domain.Helpers (tryParseInt)

import qualified Data.Bool as B
import qualified Data.Char as C
import           Data.Either.Extra (maybeToEither)
import qualified Data.List as L
import qualified Data.List.Extra as L
import qualified Data.List.NonEmpty as NE
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
mkFoundry (faction, _) rawData =
  fmap concat
    . for rawData
    $ \raw ->
      case raw of
        AbilityData     a -> fmap L.singleton . Right $ mkAbility     a
        ArmorData       a -> fmap L.singleton $ mkArmor       faction a
        EquipmentData   e -> fmap L.singleton $ mkEquipment   faction e
        MeleeData       m -> mkMeleeWeapons  faction m
        PermutationData p -> fmap L.singleton $ mkPermutation faction p
        RangedData      r -> mkRangedWeapons faction r

mkAbility :: RawAbility -> FoundryData
mkAbility raw =
  FoundryAbility
    $ Ability
        { abilityName        = mkName $ rawAbilityName raw
        , abilityPrereqs     = mkPrereqs $ rawAbilityPrereqs raw
        , abilityCost        = rawAbilityCost raw
        , abilitySummary     = mkDescription $ rawAbilitySummary raw
        , abilityDescription = mkDescription $ rawAbilityDescription raw
        , abilityType        = TrueAbility
        }

mkArmor :: Maybe Faction -> RawArmor -> Either T.Text FoundryData
mkArmor mbFaction raw = do
  faction <-
    flip maybeToEither mbFaction
      $ T.unwords
          [ "Cannot build Armor"
          , rawArmorName raw <> ":"
          , "no faction could be parsed."
          ]

  let weight =
        Weight
          { weightEach          = rawArmorWeight raw
          , weightSelfSupported = rawArmorSelfSupported raw
          }

      desc = rawArmorDescription raw
      notes =
        ArmorNotes
          { armorNotesDefault      = desc
          , armorNotesVariant      = Nothing
          , armorNotesPermutations = Nothing
          , armorNotesOther        = Nothing
          }

      toProtection fn = mkArmorAdjustment . basicItemAdjustment $ fn raw
      protection =
        Protection
          { protectionHead      = toProtection rawArmorHead
          , protectionChest     = toProtection rawArmorChest
          , protectionLeftArm   = toProtection rawArmorArms
          , protectionRightArm  = toProtection rawArmorArms
          , protectionLeftLeg   = toProtection rawArmorLegs
          , protectionRightLeg  = toProtection rawArmorLegs
          }

      shields =
        if rawArmorIntegrity raw > 0
           then
             let mkShieldValue fn = basicItemAdjustment $ fn raw
              in Shields
                   { shieldsHas       = True
                   , shieldsIntegrity = mkShieldValue rawArmorIntegrity
                   , shieldsRecharge  = mkShieldValue rawArmorRecharge
                   , shieldsDelay     = mkShieldValue rawArmorDelay
                   }

           else
             emptyShields

      stats =
        sumStatAdjustments
          . findArmorStatAdjustments raw
          . statAdjustmentsFromSpecialRules
          $ maybe T.empty T.toLower desc

  Right
    . FoundryArmor
    $ Armor
        { armorName        = mkName $ rawArmorName raw
        , armorVariant     = Nothing
        , armorFaction     = faction
        , armorPrice       = mkItemPrice $ rawArmorPrice raw
        , armorBreakpoints = mkBreakpoints 0
        , armorTrainings   = mkItemTrainings faction Nothing
        , armorWeight      = weight
        , armorStats       = stats
        , armorHardpoints  = emptyHardpoints
        , armorMaterial    = T.empty
        , armorNotes       = notes
        , armorProtection  = protection
        , armorShields     = shields
        , armorSize        = Normal
        }

mkEquipment :: Maybe Faction -> RawEquipment -> Either T.Text FoundryData
mkEquipment mbFaction raw = do
  faction <-
    flip maybeToEither mbFaction
      $ T.unwords
          [ "Cannot build Equipment"
          , rawEquipmentName raw <> ":"
          , "no faction could be parsed."
          ]

  let desc = rawEquipmentDescription raw
      weight =
        Weight
          { weightEach = rawEquipmentWeight raw
          -- No Item supports its own weight as far as the descriptions are
          -- concerned. This can be updated if/when a user points one out or one
          -- gets added to the game.
          , weightSelfSupported = False
          }

  Right
    . FoundryEquipment
    $ Equipment
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

mkMeleeWeapons :: Maybe Faction -> RawMeleeWeapon -> Either T.Text [FoundryData]
mkMeleeWeapons mbFaction raw =
  traverse (mkMelee mbFaction raw) . NE.toList $ rawMeleeBases raw

mkMelee :: Maybe Faction
        -> RawMeleeWeapon
        -> RawMeleeBase
        -> Either T.Text FoundryData
mkMelee mbFaction raw rawBase = do
  faction <-
    flip maybeToEither mbFaction
      $ T.unwords
          [ "Cannot build Melee Weapon "
          , rawMeleeName raw <> ":"
          , "no faction could be parsed."
          ]

  let weaponType = rawMeleeBaseType rawBase
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

      specialsTxt = rawMeleeBaseSpecialRules rawBase
      (tags, specials) =
        fromMaybe ([], T.empty)
          . L.unsnoc
          . fmap T.strip
          . T.split (== ']')
          $ T.filter (/= '[') specialsTxt

      (tagSpecials, weaponTags) =
        L.unzip $ mapMaybe (L.unsnoc . T.split (== ' ')) tags

      (unknownSpecials, specialRules) =
        buildSpecials $ T.split (== ',') specials <> fmap T.unwords tagSpecials

      description =
        mkDescription $ T.concat
          [ rawMeleeBaseDescription rawBase
          , "<br><br>"
          , T.intercalate ". " $ Set.toList unknownSpecials
          ]

  Right
    . FoundryWeapon
    $ Weapon
        { weaponName        = mkName $ rawMeleeBaseName rawBase
        , weaponFaction     = faction
        , weaponDescription = description
        , weaponPrice       = mkItemPrice $ rawMeleePrice raw
        , weaponBreakpoints = mkBreakpoints 0
        , weaponTrainings   = mkItemTrainings faction $ fst <$> weaponDetails
        , weaponWeight      = weight
        , weaponGroup       = fromMaybe MeleeGroup $ snd <$> weaponDetails
        , weaponTags        = buildWeaponTags weaponTags
                                $ rawMeleeBaseAttr rawBase
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
        , weaponAmmoList    = mkMeleeSTDAmmo rawBase specialRules
        , weaponSettings    = emptyWeaponSettings
        , weaponShields     = findShieldsIn specialsTxt

        -- At present, there are no ranged Weapon Items that modify their
        -- wielder's characteristics. This may change in the future, so the
        -- value will be kept hardcoded to `Nothing` here for now.
        , weaponCharacteristics = Nothing
        }

mkPermutation :: Maybe Faction -> RawPermutation -> Either T.Text FoundryData
mkPermutation mbFaction raw = do
  faction <-
    flip maybeToEither mbFaction
      $ T.unwords
          [ "Cannot build Permutation"
          , rawPermutationName raw <> ":"
          , "no faction could be parsed."
          ]

  let desc =
        T.intercalate "<br><br>"
          [ "Hardpoint Location: " <> rawPermutationLocation raw
          , rawPermutationDescription raw
          ]

  Right
    . FoundryEquipment
    $ Equipment
        { equipmentName        = mkName $ rawPermutationName raw
        , equipmentFaction     = faction
        , equipmentPrice       = mkItemPrice $ rawPermutationPrice raw
        , equipmentBreakpoints = mkBreakpoints 0
        , equipmentTrainings   = mkItemTrainings faction Nothing
        , equipmentWeight      = emptyWeight
        , equipmentDescription = mkDescription desc
        , equipmentShields     = Nothing

        -- At present, there are no Equipment Items that can modify an Actor's
        -- characteristics. This may change in the future, so the value will be
        -- kept hardcoded to `Nothing` here for now.
        , equipmentCharacteristics = Nothing
        }

mkRangedWeapons :: Maybe Faction
                -> RawRangedWeapon
                -> Either T.Text [FoundryData]
mkRangedWeapons mbFaction raw =
  traverse (mkRanged mbFaction raw) . NE.toList $ rawRangedBases raw

mkRanged :: Maybe Faction
         -> RawRangedWeapon
         -> RawRangedBase
         -> Either T.Text FoundryData
mkRanged mbFaction raw rawBase = do
  faction <-
    flip maybeToEither mbFaction
      $ T.unwords
          [ "Cannot build Ranged Weapon"
          , rawRangedName raw <> ":"
          , "no faction could be parsed."
          ]

  let (name, nickname) = mkNameAndNickname $ rawRangedName raw
      weaponType = rawRangedBaseType rawBase
      weaponDetails = Map.lookup (T.toUpper weaponType) weaponDetailsMap
      weight =
        Weight
          { weightEach = rawRangedWeight raw
          -- No Item supports its own weight as far as the descriptions are
          -- concerned. This can be updated if/when a user points one out or one
          -- gets added to the game.
          , weightSelfSupported = False
          }

      specialsTxt = rawRangedBaseSpecialRules rawBase
      (tags, specials) =
        fromMaybe ([], T.empty)
          . L.unsnoc
          . fmap T.strip
          . T.split (== ']')
          $ T.filter (/= '[') specialsTxt

      (tagSpecials, weaponTags) =
        L.unzip $ mapMaybe (L.unsnoc . T.split (== ' ')) tags

      (unknownSpecials, specialRules) =
        buildSpecials $ T.split (== ',') specials <> fmap T.unwords tagSpecials

      description =
        mkDescription $ T.concat
          [ rawRangedBaseDescription rawBase
          , "&#13;&#13;"
          , T.intercalate ". " $ Set.toList unknownSpecials
          ]

      reload =
        mkReload
          . maybe 0 (read . T.unpack)
          . listToMaybe
          . T.words
          $ rawRangedBaseReload rawBase

  Right
    . FoundryWeapon
    $ Weapon
        { weaponName        = name
        , weaponFaction     = faction
        , weaponDescription = description
        , weaponPrice       = mkItemPrice $ rawRangedPrice raw
        , weaponBreakpoints = mkBreakpoints 0
        , weaponTrainings   = mkItemTrainings faction $ fst <$> weaponDetails
        , weaponWeight      = weight
        , weaponGroup       = fromMaybe Ranged $ snd <$> weaponDetails
        , weaponTags        = buildWeaponTags weaponTags
                                $ rawRangedBaseAttr rawBase
        , weaponFireModes   = mkFireModeMap $ rawRangedBaseROF rawBase
        , weaponAttack      = emptyAttack
        , weaponReload      = reload
        , weaponNickname    = nickname
        , weaponType        = WeaponType weaponType
        , weaponMagCap      = mkMagazineCapacity $ rawRangedBaseMagazine rawBase
        , weaponAmmo        = mkAmmo ""
        , weaponAmmoGroup   = None
        , weaponScopeMag    = Just $ mkScopeMagnification 1
        , weaponCurrentAmmo = mkName "STD"
        , weaponAmmoList    = mkRangedSTDAmmo rawBase specialRules
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

buildSpecials :: [T.Text] -> (Set.Set T.Text, SpecialRules_Weapon)
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
          "linked"      : _ -> (unk, rules { linked           = extractInt txt })
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

   in L.foldl' updateSpecialRules (Set.empty, emptyWeaponSpecialRules) specials

findArmorStatAdjustments :: RawArmor -> StatAdjustments -> StatAdjustments
findArmorStatAdjustments raw startingStats =
  let setStatAdjustment adj stat =
        stat { itemAdjustment = itemAdjustment stat + adj }

      tryParseStatAdjustment stats txt =
        case T.words $ T.strip txt of
          adj : "STR" : [] ->
            stats { statAdjustmentsHas = True
                  , statAdjustmentsSTR =
                      setStatAdjustment (read $ T.unpack adj)
                        $ statAdjustmentsSTR stats
                  }

          adj : "MSTR" : [] ->
            stats { statAdjustmentsHas       = True
                  , statAdjustmentsMythicSTR =
                      setStatAdjustment (read $ T.unpack adj)
                        $ statAdjustmentsMythicSTR stats
                  }

          adj : "AGI" : [] ->
            stats { statAdjustmentsHas = True
                  , statAdjustmentsAGI =
                      setStatAdjustment (read $ T.unpack adj)
                        $ statAdjustmentsAGI stats
                  }

          adj : "MAGI" : [] ->
            stats { statAdjustmentsHas       = True
                  , statAdjustmentsMythicAGI =
                      setStatAdjustment (read $ T.unpack adj)
                        $ statAdjustmentsMythicAGI stats
                  }

          _ ->
            stats

   in L.foldl' tryParseStatAdjustment startingStats
        . T.split (== ',')
        $ rawArmorStats raw

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

mkMeleeSTDAmmo :: RawMeleeBase -> SpecialRules_Weapon -> AmmoList
mkMeleeSTDAmmo rawBase specials =
  let (diceQuantity, diceValue) = parseDiceValues $ rawMeleeBaseDamageRoll rawBase
      range = emptyWeaponRange { weaponRangeMelee = rawMeleeBaseRange rawBase }
   in mkAmmoList $
        Ammunition
          { ammunitionName         = mkName "STD"
          , ammunitionAttackBonus  = rawMeleeBaseHitMod rawBase
          , ammunitionDiceQuantity = diceQuantity
          , ammunitionDiceValue    = diceValue
          , ammunitionBaseDamage   = rawMeleeBaseDamageBase rawBase
          , ammunitionSTRDamage    = strengthMultiplierFromText
                                   $ rawMeleeBaseBaseAdd rawBase
          , ammunitionPiercing     = rawMeleeBasePierce rawBase
          , ammunitionSTRPiercing  = strengthMultiplierFromText
                                   $ rawMeleeBasePierceAdd rawBase
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

mkRangedSTDAmmo :: RawRangedBase -> SpecialRules_Weapon -> AmmoList
mkRangedSTDAmmo rawBase specials =
  let (diceQuantity, diceValue) =
        parseDiceValues $ rawRangedBaseDamageRoll rawBase

      range =
        case T.split (== '-') $ T.filter (/= 'm') $ rawRangedBaseRange rawBase of
          close : long : []
            | L.all (not . T.null) [ close, long ] ->
              emptyWeaponRange
                { weaponRangeClose = read $ T.unpack close
                , weaponRangeLong  = read $ T.unpack long
                }

          _ ->
            emptyWeaponRange

      finalSpecials =
        if T.isPrefixOf "SINGLE LOADING" . T.toUpper $ rawRangedBaseType rawBase
           then specials { singleLoading = Just () }
           else specials

   in mkAmmoList $
        Ammunition
          { ammunitionName         = mkName "STD"
          , ammunitionAttackBonus  = rawRangedBaseHitMod rawBase
          , ammunitionDiceQuantity = diceQuantity
          , ammunitionDiceValue    = diceValue
          , ammunitionBaseDamage   = rawRangedBaseDamageBase rawBase
          , ammunitionSTRDamage    = Nothing
          , ammunitionPiercing     = rawRangedBasePierce rawBase
          , ammunitionSTRPiercing  = Nothing
          , ammunitionTarget       = 0
          , ammunitionCurrentMag   = rawRangedBaseMagazine rawBase
          , ammunitionCritsOn      = 10
          , ammunitionRange        = range
          , ammunitionDescription  = mkDescription ""
          , ammunitionSpecials     = finalSpecials
          }

parseDiceValues :: T.Text -> (Int, Int)
parseDiceValues dmgRoll =
  case T.split (== 'D') dmgRoll of
    qty : val : _
      | L.all T.null [ qty, val ] -> (0, 10)
      | qty == "0.5", val == "10" -> (1, 5)
      | otherwise ->
        ( read $ T.unpack qty
        , read . T.unpack $ T.takeWhile C.isDigit val
        )

    _ ->
      (0, 10)

statAdjustmentsFromSpecialRules :: T.Text -> StatAdjustments
statAdjustmentsFromSpecialRules txt =
  emptyStatAdjustments
    { statAdjustmentsAGI =
        basicItemAdjustment
          $ sum [ B.bool 0 (negate 10) $ T.isInfixOf "bulky"  txt
                , B.bool 0 (negate  5) $ T.isInfixOf "uvh-ba" txt
                , B.bool 0 10 $ T.isInfixOf "mobility-boosting exo-lining" txt
                ]
    }

sumStatAdjustments :: StatAdjustments -> StatAdjustments
sumStatAdjustments stats =
  let sumStat stat =
        stat
          { itemAdjustmentTotal =
              sum [ itemAdjustment stat
                  , itemAdjustmentVariant stat
                  , itemAdjustmentOther stat
                  ]
          }

   in stats
        { statAdjustmentsSTR       = sumStat $ statAdjustmentsSTR stats
        , statAdjustmentsAGI       = sumStat $ statAdjustmentsAGI stats
        , statAdjustmentsMythicSTR = sumStat $ statAdjustmentsMythicSTR stats
        , statAdjustmentsMythicAGI = sumStat $ statAdjustmentsMythicAGI stats
        }

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
