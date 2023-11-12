module Domain.Convert.Foundry
  ( toFoundry
  ) where

import           Flipstone.Prelude
import           Data.Types
import           Domain.Helpers (tryParseInt)

import qualified Data.Bool as B
import qualified Data.Char as C
import           Data.Either.Extra (eitherToMaybe, maybeToEither)
import qualified Data.List as L
import qualified Data.List.Extra as L
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Traversable (for)
import           Data.Tuple (fst, snd)
import           Text.Read (read, readMaybe)
import           Text.Show (show)

toFoundry :: CompendiumMap [RawData]
          -> Either T.Text (CompendiumMap [FoundryData])
toFoundry = Map.traverseWithKey mkFoundry

mkFoundry :: CompendiumData -> [RawData] -> Either T.Text [FoundryData]
mkFoundry (faction, _) rawData =
  fmap concat
    . for rawData
    $ \raw ->
      case raw of
        AbilityData     a -> Right [ mkAbility TrueAbility a ]
        ArmorData       a -> fmap L.singleton $ mkArmor faction a
        BestiaryData    b -> fmap L.singleton $ mkBestiary b
        EquipmentData   e -> fmap L.singleton $ mkEquipment faction e
        FloodData       f -> fmap L.singleton $ mkFlood f
        MeleeData       m -> mkMeleeWeapons False m
        PermutationData p -> fmap L.singleton $ mkPermutation faction p
        RangedData      r -> mkRangedWeapons False faction r
        VehicleData     v -> fmap L.singleton $ mkVehicle faction v

mkAbility :: AbilityType -> RawAbility -> FoundryData
mkAbility aType raw =
  FoundryAbility
    $ Ability
        { abilityName        = mkName $ rawAbilityName raw
        , abilityPrereqs     = mkPrereqs $ rawAbilityPrereqs raw
        , abilityCost        = rawAbilityCost raw
        , abilitySummary     = mkDescription $ rawAbilitySummary raw
        , abilityDescription = mkDescription $ rawAbilityDescription raw
        , abilityType        = aType
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
          , weightIsEmbedded    = False
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
        , armorTrainings   = mkItemTrainings (Just faction) Nothing
        , armorWeight      = weight
        , armorStats       = stats
        , armorHardpoints  = emptyHardpoints
        , armorMaterial    = T.empty
        , armorNotes       = notes
        , armorProtection  = protection
        , armorShields     = shields
        , armorSize        = Normal
        }

mkBestiary :: RawBestiary -> Either T.Text FoundryData
mkBestiary raw = do
  size <- sizeFromText $ rawBestiarySize raw
  faction <- factionFromText $ rawBestiaryFaction raw

  (normalOnlyByExp, expDiff) <-
    difficultyTierValues
      (rawBestiaryName raw)
      "experience difficulty"
      (rawBestiaryExperience raw)

  (normalOnlyByLuck, luckDifficulty) <-
    difficultyTierValues
      (rawBestiaryName raw)
      "luck"
      (rawBestiaryLuck raw)

  normalOnly <-
    if normalOnlyByExp == normalOnlyByLuck
       then Right $ normalOnlyByExp || normalOnlyByLuck
       else
         Left
           $ T.unwords
               [ "Cannot build Bestiary Character"
               , rawBestiaryName raw <> ": mismatched difficulty tier count"
               , "between luck and experience payout values."
               ]

  let abilities = mkAbility RacialTrait <$> rawBestiaryAbilities raw
      name =
        mkName
          $ if T.isSuffixOf " - BR1" $ rawBestiaryName raw
               then T.dropEnd 6 $ rawBestiaryName raw
               else rawBestiaryName raw

      carryingCapacity =
        CarryingCapacity
          { carryingCapacityDblSTR = (== 2) $ rawBestiaryCarryStrMod raw
          , carryingCapacityDblTOU = (== 2) $ rawBestiaryCarryTouMod raw
          , carryingCapacityMod    =
              let carry =
                    sum [ rawBestiaryCarryStrMod raw * rawBestiarySTR raw
                        , rawBestiaryCarryTouMod raw * rawBestiaryTOU raw
                        , 10 * rawBestiaryMythicSTR raw
                        , 10 * rawBestiaryMythicTOU raw
                        ]

               in maybe 0 (subtract carry) $ rawBestiaryCarryCap raw
          }

      staticChars =
        Set.fromList . splitStaticValues $ rawBestiaryStaticStats raw

      mkChar char fn =
        mkCharacteristic (fn raw)
          . not
          $ Set.member char staticChars

      characteristics =
        Characteristics
          { characteristicsSTR = mkChar "STR" rawBestiarySTR
          , characteristicsTOU = mkChar "TOU" rawBestiaryTOU
          , characteristicsAGI = mkChar "AGI" rawBestiaryAGI
          , characteristicsWFR = mkChar "WFR" rawBestiaryWFR
          , characteristicsWFM = mkChar "WFM" rawBestiaryWFM
          , characteristicsINT = mkChar "INT" rawBestiaryINT
          , characteristicsPER = mkChar "PER" rawBestiaryPER
          , characteristicsCRG = mkChar "CRG" rawBestiaryCRG
          , characteristicsCHA = mkChar "CHA" rawBestiaryCHA
          , characteristicsLDR = mkChar "LDR" rawBestiaryLDR
          }

      difficulty =
        Difficulty
          { difficultyNormalOnly = normalOnly
          , difficultyAdvancesMythics =
              L.any ((==) "Mythic Advancements" . rawAbilityName)
                $ rawBestiaryAbilities raw
          }

      experiencePayout =
        ExperiencePayout
          { expBase       = 0
          , expDifficulty = mkExperienceDifficulty expDiff
          }

      movement =
        Movement
          { movementRunChargeBonus = rawBestiaryChargeRunMod raw
          , movementJumpMultiplier = rawBestiaryJumpMod raw
          , movementLeapBonus      = rawBestiaryLeapAdd raw
          , movementLeapMultiplier = rawBestiaryLeapMod raw
          }

      mythicCharacteristics =
        MythicCharacteristics
          { mythicSTR = rawBestiaryMythicSTR raw
          , mythicTOU = rawBestiaryMythicTOU raw
          , mythicAGI = rawBestiaryMythicAGI raw
          }

      shields =
        case ( rawBestiaryIntegrity raw
             , rawBestiaryDelay raw
             , rawBestiaryRecharge raw
             ) of
          (Just integrity, Just delay, Just recharge) ->
            mkCharacterShields integrity recharge delay

          _ ->
            emptyCharacterShields

      wounds =
        Wounds
          { woundsMod    = 0
          , woundsDblTOU =
              flip L.any [ "Jiralhanae", "Cavalier" ]
                . flip T.isInfixOf
                $ rawBestiaryName raw
          }

  Right
    . FoundryBestiary
    $ Bestiary
        { bestiaryName             = name
        , bestiaryArmor            = emptyCharacterArmor
        , bestiaryCarryingCapacity = carryingCapacity
        , bestiaryCharacteristics  = characteristics
        , bestiaryDifficulty       = difficulty
        , bestiaryExpPayout        = experiencePayout
        , bestiaryFaction          = faction
        , bestiaryLuck             = mkLuck luckDifficulty
        , bestiaryMovement         = movement
        , bestiaryMythics          = mythicCharacteristics
        , bestiaryNaturalArmor     = fromMaybe 0 $ rawBestiaryArmor raw
        , bestiaryNotes            = rawBestiaryDescription raw
        , bestiaryShields          = shields
        , bestiarySkills           = characterSkillList
        , bestiarySize             = size
        , bestiaryTrainings        = findTrainings faction $ rawBestiaryName raw
        , bestiaryWounds           = wounds
        , bestiaryItems            = abilities
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
          , weightIsEmbedded    = False
          }

  Right
    . FoundryEquipment
    $ Equipment
        { equipmentName        = mkName $ rawEquipmentName raw
        , equipmentFaction     = faction
        , equipmentPrice       = mkItemPrice $ rawEquipmentPrice raw
        , equipmentBreakpoints = mkBreakpoints 0
        , equipmentTrainings   = mkItemTrainings (Just faction) Nothing
        , equipmentWeight      = weight
        , equipmentDescription = mkDescription desc
        , equipmentShields     = findShieldsIn desc

        -- At present, there are no Equipment Items that can modify an Actor's
        -- characteristics. This may change in the future, so the value will be
        -- kept hardcoded to `Nothing` here for now.
        , equipmentCharacteristics = Nothing
        }

mkFlood :: RawFlood -> Either T.Text FoundryData
mkFlood raw = do
  size <- sizeFromText $ rawFloodSize raw

  let name =
        mkName
          . maybe (rawFloodName raw) snd
          . L.unsnoc
          . T.splitOn " - "
          $ rawFloodName raw

      characteristics =
        Characteristics_Flood
          { floodCharacteristicsSTR = rawFloodSTR raw
          , floodCharacteristicsTOU = rawFloodTOU raw
          , floodCharacteristicsAGI = rawFloodAGI raw
          , floodCharacteristicsWFR = rawFloodWFR raw
          , floodCharacteristicsWFM = rawFloodWFM raw
          , floodCharacteristicsINT = rawFloodINT raw
          , floodCharacteristicsPER = rawFloodPER raw
          }

      mythics =
        MythicCharacteristics_Flood
          { floodMythicSTR = rawFloodMythicSTR raw
          , floodMythicTOU = rawFloodMythicTOU raw
          , floodMythicAGI = rawFloodMythicAGI raw
          }

      swarm =
        mkSwarm 1
          . L.any ((==) "Swarm" . rawAbilityName)
          $ rawFloodAbilities raw

      abilities = mkAbility RacialTrait <$> rawFloodAbilities raw
      trainings =
        emptyTrainings
          { trainingsEquipment = Set.fromList [ minBound..maxBound ]
          , trainingsFaction   = Set.fromList factions
          }

      experience =
        ExperiencePayout
          { expBase       = rawFloodExperience raw
          , expDifficulty = emptyExperienceDifficulty
          }

  Right
    . FoundryFlood
    $ Flood
        { floodName                  = name
        , floodCharacteristics       = characteristics
        , floodMythicCharacteristics = mythics
        , floodWounds                = FloodWounds $ rawFloodWounds raw
        , floodSize                  = size
        , floodNotes                 = Nothing
        , floodContamination         = Contamination 0
        , floodSwarm                 = swarm
        , floodArmor                 = emptyCharacterArmor
        , floodShields               = emptyCharacterShields
        , floodSkills                = floodSkillList
        , floodTrainings             = trainings
        , floodExperience            = experience
        , floodItems                 = abilities
        }

mkMeleeWeapons :: Embedded -> RawMeleeWeapon -> Either T.Text [FoundryData]
mkMeleeWeapons embedded raw =
  traverse (mkMelee embedded raw) $ rawMeleeBases raw

mkMelee :: Embedded
        -> RawMeleeWeapon
        -> RawMeleeBase
        -> Either T.Text FoundryData
mkMelee embedded raw rawBase = do
  let mbFaction = eitherToMaybe . factionFromText $ rawMeleeFaction raw
      wType = rawMeleeBaseType rawBase
      weaponDetails = Map.lookup (T.toUpper $ T.strip wType) weaponDetailsMap
      fireModeMap = Map.singleton NoFireMode $ mkFireRate 0
      weight =
        Weight
          { weightEach       = rawMeleeWeight raw
          , weightIsEmbedded = embedded
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

      (tagSpecials, wTags) =
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
        , weaponFaction     = mbFaction
        , weaponDescription = description
        , weaponPrice       = mkItemPrice $ rawMeleePrice raw
        , weaponBreakpoints = mkBreakpoints 0
        , weaponTrainings   = mkItemTrainings mbFaction $ fst <$> weaponDetails
        , weaponWeight      = weight
        , weaponGroup       = MeleeGroup
        , weaponTags        = buildWeaponTags wTags
                                $ rawMeleeBaseAttr rawBase
        , weaponFireModes   = mkFireModes fireModeMap
        , weaponAttack      = emptyAttack
        , weaponReload      = mkReload 0
        , weaponNickname    = Nothing
        , weaponType        = WeaponType wType
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

      weight =
        Weight
          { weightEach       = rawPermutationWeight raw
          , weightIsEmbedded = False
          -- No Item supports its own weight as far as the descriptions are
          -- concerned. This can be updated if/when a user points one out or one
          -- gets added to the game.
          , weightSelfSupported = False
          }

  Right
    . FoundryEquipment
    $ Equipment
        { equipmentName        = mkName $ rawPermutationName raw
        , equipmentFaction     = faction
        , equipmentPrice       = mkItemPrice $ rawPermutationPrice raw
        , equipmentBreakpoints = mkBreakpoints 0
        , equipmentTrainings   = mkItemTrainings (Just faction) Nothing
        , equipmentWeight      = weight
        , equipmentDescription = mkDescription desc
        , equipmentShields     = Nothing

        -- At present, there are no Equipment Items that can modify an Actor's
        -- characteristics. This may change in the future, so the value will be
        -- kept hardcoded to `Nothing` here for now.
        , equipmentCharacteristics = Nothing
        }

mkRangedWeapons :: Embedded
                -> Maybe Faction
                -> RawRangedWeapon
                -> Either T.Text [FoundryData]
mkRangedWeapons embedded mbFaction raw =
  traverse (mkRanged mbFaction embedded raw)
    $ rawRangedBases raw

mkRanged :: Maybe Faction
         -> Bool
         -> RawRangedWeapon
         -> RawRangedBase
         -> Either T.Text FoundryData
mkRanged mbFaction isEmbedded raw rawBase = do
  let faction = eitherToMaybe . factionFromText $ rawRangedFaction raw
      (name, nickname) = mkNameAndNickname $ rawRangedBaseName rawBase
      wType = rawRangedBaseType rawBase
      weight =
        Weight
          { weightEach       = rawRangedWeight raw
          , weightIsEmbedded = isEmbedded
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

      (tagSpecials, wTags) =
        L.unzip $ mapMaybe (L.unsnoc . T.split (== ' ')) tags

      (unknownSpecials, specialRules) =
        buildSpecials
          . catMaybes
          . L.cons (findSpecialRuleLinked $ nameText name)
          . fmap Just
          $ T.split (== ',') specials <> fmap T.unwords tagSpecials

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

  weaponDetails <-
    case Map.lookup (T.toUpper $ T.strip wType) weaponDetailsMap of
      Just details ->
        Right $ Just details

      Nothing
        | wType == "Secondary Attack" ->
            Right Nothing

        | otherwise ->
            Left $ "Could not parse weapon type " <> wType

  Right
    . FoundryWeapon
    $ Weapon
        { weaponName        = name
        , weaponFaction     = mbFaction
        , weaponDescription = description
        , weaponPrice       = mkItemPrice $ rawRangedPrice raw
        , weaponBreakpoints = mkBreakpoints 0
        , weaponTrainings   = mkItemTrainings faction $ fst <$> weaponDetails
        , weaponWeight      = weight
        , weaponGroup       = maybe Ranged snd weaponDetails
        , weaponTags        = buildWeaponTags wTags
                                $ rawRangedBaseAttr rawBase
        , weaponFireModes   = mkFireModeMap $ rawRangedBaseROF rawBase
        , weaponAttack      = emptyAttack
        , weaponReload      = reload
        , weaponNickname    = nickname
        , weaponType        = WeaponType wType
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

mkVehicle :: Maybe Faction -> RawVehicle -> Either T.Text FoundryData
mkVehicle mbFaction raw = do
  let errMsg reason =
        T.unwords
          [ "Cannot build Vehicle"
          , rawVehicleName raw <> ":"
          , reason
          ]

      (specialsTxt, propulsionTxt) =
        case L.filter (T.any C.isAlphaNum) . T.split (== '.') $ rawVehicleAdditionalInfo raw of
          [ g1, g2 ] ->
            (g1, g2)

          [ g1 ]
            | Just True <- fmap (C.isDigit . fst) . T.uncons $ T.strip g1 ->
                (T.empty, g1)

            | otherwise ->
                (g1, T.empty)

          _ ->
            (T.empty, T.empty)

  meleeWeapons <-
    fmap concat
      . traverse (mkMeleeWeapons True)
      $ rawVehicleMeleeWeapons raw

  rangedWeapons <-
    fmap concat
      . traverse (mkRangedWeapons True mbFaction)
      $ rawVehicleRangedWeapons raw

  faction <- maybeToEither (errMsg "no faction could be parsed") mbFaction
  tonnes <-
    case T.words . T.toLower $ rawVehicleWeight raw of
      [ wt , unit ]
        | unit == "kg"     -> Right . (/ 1000) . read $ T.unpack wt
        | unit == "tonnes" -> Right . read $ T.unpack wt
      _ -> Left . errMsg $ "Cannot parse weight " <> rawVehicleWeight raw

  (characteristics, movement, propulsion, isWalker) <- do
    mapLeft errMsg $ getVehicleStatsByType raw specialsTxt propulsionTxt

  (operators, gunners) <- do
    case fmap (T.words . T.strip) . T.split (== ',') <$> rawVehicleCrew raw of
      Just [ [ o, opTxt ], [ g, grTxt ] ]
        | T.isInfixOf "Operator" opTxt
        , T.isInfixOf "Gunner" grTxt
        , Just ops <- readMaybe $ T.unpack o
        , ops > 0
        , Just grs <- readMaybe $ T.unpack g ->
            Right (ops, grs)

      Just [ o : restOp ]
        | T.isInfixOf "Operator" $ T.unwords restOp
        , Just ops <- readMaybe $ T.unpack o
        , ops > 0 ->
            Right (ops, 0)

      Nothing ->
        Right (0, 0)

      _ ->
        Left
          . errMsg
          $ "Cannot parse crew " <> fromMaybe T.empty (rawVehicleCrew raw)

  let (designation, variant) =
        case fmap T.strip . T.splitOn " - " $ rawVehicleName raw of
          [ d, v ] -> (d, Just v)
          [ d ]    -> (d, Nothing)
          _        -> (rawVehicleName raw, Nothing)

      mbSize = eitherToMaybe . sizeFromText $ rawVehicleSize raw
      dimensions =
        Dimensions
          { dimensionsLength = rawVehicleLength raw
          , dimensionsWidth  = rawVehicleWidth  raw
          , dimensionsHeight = rawVehicleHeight raw
          , dimensionsWeight = tonnes
          }

      breakpoints =
        Breakpoints_Vehicle
          { breakpointsWEP  = mkBreakpoints $ rawVehicleBreakpointsWEP  raw
          , breakpointsMOB  = mkBreakpoints $ rawVehicleBreakpointsMOB  raw
          , breakpointsENG  = mkBreakpoints $ rawVehicleBreakpointsENG  raw
          , breakpointsOP   = mkBreakpoints $ rawVehicleBreakpointsOP   raw
          , breakpointsHULL = mkBreakpoints $ rawVehicleBreakpointsHULL raw
          }

      armor =
        Armor_Vehicle
          { armorFront  = rawVehicleArmorFront  raw
          , armorBack   = rawVehicleArmorBack   raw
          , armorSide   = rawVehicleArmorSide   raw
          , armorTop    = rawVehicleArmorTop    raw
          , armorBottom = rawVehicleArmorBottom raw
          }

      shields =
        case ( rawVehicleShieldIntegrity raw
             , rawVehicleShieldDelay     raw
             , rawVehicleShieldRecharge  raw
             ) of
          (Just integrity, Just delay, Just recharge) ->
            mkCharacterShields integrity recharge delay

          _ ->
            emptyCharacterShields

      complement = T.count "[C]" $ rawVehicleDescription raw
      crewNotes =
        T.intercalate ", "
          . catMaybes
          $ [ rawVehicleCrew raw
            , rawVehicleComplement raw
            ]

      (unknownSpecials, specialRules) =
        buildVehicleSpecials raw specialsTxt isWalker

      isAutomated =
        L.and [ propulsionType propulsion == Stationary
              , vehCharacteristicsWFR characteristics > 0
              ]

      notes =
        mkDescription
          . T.intercalate "<br><br>"
          . L.filter (not . T.null)
          $ [ propulsionTxt
            , T.intercalate ", " $ Set.toList unknownSpecials
            , rawVehicleDescription raw
            ]

  Right
    . FoundryVehicle
    $ Vehicle
        { vehicleName            = mkName $ rawVehicleName raw
        , vehicleDesignation     = mkName designation -- Name
        , vehicleVariant         = variant
        , vehicleFaction         = faction
        , vehiclePrice           = rawVehiclePrice raw
        , vehicleExperience      = rawVehicleExperience raw
        , vehicleSize            = mbSize
        , vehicleDimensions      = dimensions
        , vehicleCharacteristics = characteristics
        , vehicleMovement        = movement
        , vehicleBreakpoints     = breakpoints
        , vehicleArmor           = armor
        , vehicleShields         = shields
        , vehicleSizePoints      = rawVehicleSizePoints raw
        , vehicleWeaponPoints    = rawVehicleWeaponPoints raw
        , vehicleCrew            = mkCrew operators gunners complement crewNotes
        , vehicleSpecialRules    = specialRules
        , vehicleAutomated       = isAutomated
        , vehiclePropulsion      = propulsion
        , vehicleNotes           = notes
        , vehicleWeapons         = meleeWeapons <> rangedWeapons
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
          "emp"         : _ -> (unk, rules { emp              = Just $ extract txt })
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

buildVehicleSpecials :: RawVehicle
                     -> T.Text
                     -> Bool
                     -> (Set.Set T.Text, SpecialRules_Vehicle)
buildVehicleSpecials raw specials isWalker =
  let boolToMaybe txt cond = B.bool Nothing (Just txt) cond
      extract = tryParseInt . T.takeWhile (/= ')') . T.drop 1 . T.dropWhile (/= '(')
      updateSpecialRules (unk, rules) txt =
        case T.strip . T.toLower <$> T.words txt of
          "all-terrain" : _ -> (unk, rules { allTerrain      = Just ()     })
          "antigrav"    : _ -> (unk, rules { antiGrav        = Just ()     })
          "autoloader"  : _ -> (unk, rules { autoloader      = Just ()     })
          "boost"       : _ -> (unk, rules { boost           = extract txt })
          "continuous"  : _ -> (unk, rules { continuousTrack = Just ()     })
          "heavy"       : _ -> (unk, rules { heavyPlating    = Just ()     })
          "neural"      : _ -> (unk, rules { neuralInterface = Just ()     })
          "open-top"    : _ -> (unk, rules { openTop         = Just ()     })
          "slipspace"   : _ -> (unk, rules { slipspace       = Just ()     })
          "walker"      : _ -> (unk, rules { walkerStomp     = Just ()     })
          _                 -> (Set.insert txt unk, rules)

   in L.foldl' updateSpecialRules (Set.empty, emptyVehicleSpecialRules)
        . catMaybes
        . L.cons
            ( boolToMaybe "autoloader"
                . not
                . L.null
                $ rawVehicleRangedWeapons raw
            )
        . L.cons
            ( boolToMaybe "slipspace"
                . T.isInfixOf "slipspace"
                . T.toLower
                $ rawVehicleDescription raw
            )
        . L.cons (boolToMaybe "walker" isWalker)
        . fmap (Just . T.strip)
        $ T.split (== ',') specials

getVehicleStatsByType :: RawVehicle
                      -> T.Text
                      -> T.Text
                      -> Either T.Text ( Characteristics_Vehicle
                                       , Movement_Vehicle
                                       , Propulsion
                                       , Bool
                                       )
getVehicleStatsByType raw specialsTxt propulsionTxt =
  let mbWalkerStats = do
        str  <- rawVehicleSTR raw
        mStr <- rawVehicleMythicSTR raw
        agi  <- rawVehicleAGI raw
        mAgi <- rawVehicleMythicAGI raw
        Just (str, mStr, agi, mAgi)

      mbMovement = do
        acc <- rawVehicleAccelerate raw
        brk <- rawVehicleBrake raw
        spd <- rawVehicleTopSpeed raw
        Just (acc, brk, spd, rawVehicleManeuver raw)

   in case ( getTurretStats raw <$> rawVehicleTurretStats raw
           , getWalkerStats specialsTxt propulsionTxt <$> mbWalkerStats
           , getVehicleStats specialsTxt propulsionTxt <$> mbMovement
           ) of
        (Just turret, _, _)  -> turret
        (_, Just walker, _)  -> walker
        (_, _, Just vehicle) -> vehicle
        _ -> Left $ "Couldn't build stats for " <> rawVehicleName raw

getTurretStats :: RawVehicle
               -> T.Text
               -> Either T.Text ( Characteristics_Vehicle
                                , Movement_Vehicle
                                , Propulsion
                                , Bool
                                )
getTurretStats raw turretStats = do
  (wfr, int, per) <-
    case traverse (readMaybe . T.unpack) $ T.split (== ';') turretStats of
      Just [ wfr, int, per ] -> Right (wfr, int, per)
      _ -> Left $ "Could not parse turret stats: " <> rawVehicleName raw

  Right
    ( emptyVehicleCharacteristics
        { vehCharacteristicsWFR = wfr
        , vehCharacteristicsINT = int
        , vehCharacteristicsPER = per
        }
    , emptyVehicleMovement
    , Propulsion
        { propulsionType  = Stationary
        , propulsionCount = 0
        }
    , False
    )

getVehicleStats :: T.Text
                -> T.Text
                -> (Int, Int, Int, Int)
                -> Either T.Text ( Characteristics_Vehicle
                                 , Movement_Vehicle
                                 , Propulsion
                                 , Bool
                                 )
getVehicleStats specialsTxt propulsionTxt (acc, brk, spd, man) = do
  propulsion <- findVehiclePropulsion specialsTxt propulsionTxt

  let pType = propulsionType propulsion
      count = propulsionCount propulsion

  if hasPermissiblePropulsionCountForType count pType
     then
       Right
         ( emptyVehicleCharacteristics
         , emptyVehicleMovement
             { movementAccelerate = acc
             , movementBrake      = brk
             , movementSpeed      = spd
             , movementManeuver   = man
             }
         , propulsion
         , False
         )

     else
       Left
         $ T.unwords
             [ "Inappropriate propulsion count"
             , T.pack $ show count
             , "for type"
             , propulsionTypeToText pType
             ]

getWalkerStats :: T.Text
               -> T.Text
               -> (Int, Int, Int, Int)
               -> Either T.Text ( Characteristics_Vehicle
                                , Movement_Vehicle
                                , Propulsion
                                , Bool
                                )
getWalkerStats specialsTxt propulsionTxt (str, mStr, agi, mAgi) = do
  propulsion <- do
    unchecked <- findVehiclePropulsion specialsTxt propulsionTxt
    case (propulsionType unchecked, propulsionCount unchecked) of
      (Legs, 3) -> Right $ unchecked { propulsionCount = 2 }
      _ -> Right unchecked

  let count = propulsionCount propulsion
      pType = propulsionType propulsion

  if hasPermissiblePropulsionCountForType count pType
     then
       Right
         ( emptyVehicleCharacteristics
             { vehCharacteristicsSTR       = str
             , vehCharacteristicsMythicSTR = mStr
             , vehCharacteristicsAGI       = agi
             , vehCharacteristicsMythicAGI = mAgi
             }
         , emptyVehicleMovement
             { movementWalkerJump = div mStr 2
             , movementWalkerLeap = mStr
             }
         , propulsion
         , True
         )

     else
       Left
         $ T.unwords
             [ "Inappropriate propulsion count"
             , T.pack $ show count
             , "for type"
             , propulsionTypeToText pType
             ]

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

findSpecialRuleLinked :: T.Text -> Maybe T.Text
findSpecialRuleLinked txt =
  case T.words . T.takeWhile (/= ')') . T.drop 1 $ T.dropWhile (/= '(') txt of
    [ val, "Linked" ] ->
      Just $ "Linked (" <> val <> ")"

    _ ->
      Nothing

findTrainings :: Faction -> T.Text -> Trainings
findTrainings Forerunner _name =
  emptyTrainings
    { trainingsEquipment = allEquipmentTrainings
    , trainingsFaction   = Set.singleton Forerunner
    }

findTrainings faction name =
  let is = flip T.isInfixOf name
   in case (is "Spartan", is "Civilian") of
        (True, _) ->
          emptyTrainings
            { trainingsEquipment = allEquipmentTrainings
            , trainingsFaction   = Set.fromList factions
            }

        (_, True) ->
          emptyTrainings
            { trainingsFaction = Set.singleton faction }

        _ ->
          emptyTrainings
            { trainingsEquipment = Set.fromList [ Basic, Infantry ]
            , trainingsFaction   = Set.singleton faction
            }

findVehiclePropulsion :: T.Text -> T.Text -> Either T.Text Propulsion
findVehiclePropulsion specialsTxt =
  (=<<) (combinePropulsions . catMaybes)
    . traverse (propulsionFromText specialsTxt . T.strip)
    . T.split (== ',')

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

difficultyTierValues :: T.Text
                     -> T.Text
                     -> T.Text
                     -> Either T.Text DifficultyTierValuesWithSingleLevelFlag
difficultyTierValues name field values =
  case fmap (readMaybe . T.unpack) $ splitStaticValues values of
    [ Just easy, Just normal, Just heroic, Just legendary, Just nemesis ] ->
      Right (False, (easy, normal, heroic, legendary, nemesis))

    [ Just normal ] ->
      Right (True, (normal, normal, normal, normal, normal))

    _ ->
      Left
        $ T.unwords
            [ "Cannot build"
            , name <> ": "
            , "must have either 1 or 5 difficulty values exactly for"
            , field <> "."
            ]

splitStaticValues :: T.Text -> [T.Text]
splitStaticValues = T.split (== ';')

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
    , ( "BEAM WEAPON"           , (Advanced, Ranged) )
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
    , ( "GMG"                   , (Heavy   , Ranged) )
    , ( "GRENADE"               , (Infantry, Thrown) )
    , ( "GRENADE LAUNCHER"      , (Launcher, Ranged) )
    , ( "HAMMER"                , (Melee   , MeleeGroup) )
    , ( "HEAVY MACHINE GUN"     , (Heavy   , Ranged) )
    , ( "HMG"                   , (Heavy   , Ranged) )
    , ( "KNIFE"                 , (Basic   , MeleeGroup) )
    , ( "LANDMINE"              , (Ordnance, Thrown) )
    , ( "LIGHT MACHINE GUN"     , (Heavy   , Ranged) )
    , ( "LMG"                   , (Heavy   , Ranged) )
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
    , ( "SHOTGUN"               , (Basic   , Ranged) )
    , ( "SHOVEL"                , (Melee   , MeleeGroup) )
    , ( "SINGLE LOADING SHOTGUN", (Basic   , Ranged) )
    , ( "SINGLE USE"            , (Infantry, Thrown) )
    , ( "SMG"                   , (Infantry, Ranged) )
    , ( "SNIPER RIFLE"          , (Range   , Ranged) )
    , ( "SPRAY WEAPON"          , (Melee   , MeleeGroup) )
    , ( "TASER"                 , (Melee   , MeleeGroup) )
    , ( "TWO-HANDED SWORD"      , (Melee   , MeleeGroup) )
    ]
