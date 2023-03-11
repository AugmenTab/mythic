/** @module calculations */

import { interpretDiceRollModifiers } from "./dice.js";
import { makeUIError, makeUIWarning } from "./common.js";

const MELEE_REACH_SIZE_BONUS = {
  "mini": 0,
  "small": 1,
  "normal": 1,
  "large": 2,
  "huge": 3,
  "hulking": 4,
  "giant": 5,
  "immense": 6,
  "massive": 8,
  "great": 10,
  "monumental": 20,
  "colossal": 50,
  "vast": 100
};

const PHYSICAL_SKILLS = new Set([
  "athletics",
  "evasion",
  "pilotGround",
  "pilotAir",
  "pilotSpace",
  "stunting"
]);

/**
 * Calculates armor protection, shield, and characteristics values.
 *
 * @param {ItemData} armorData - The armor's ItemData.
 */
export function calculateArmorValues(armorData) {
  armorData.price.total = armorData.price.base + armorData.price.mods;
  new Array(
    Object.entries(armorData.characteristics),
    Object.entries(armorData.protection),
    Object.entries(armorData.shields),
  ).flat().filter(v => v[0] !== "has").map(calculateItemValues);
}

/**
 * Calculates equipment shield values.
 *
 * @param {ItemData} equipmentData - The equipment's ItemData.
 */
export function calculateEquipmentValues(equipmentData) {
  new Array(
    Object.entries(equipmentData.characteristics),
    Object.entries(equipmentData.shields)
  ).flat().filter(v => v[0] !== "has").map(calculateItemValues);
}

/**
 * Calculates weapon values.
 *
 * @param {ItemData} weaponData - The weapon's ItemData.
 */
export function calculateWeaponValues(weaponData) {
  weaponData.price.total = weaponData.price.base + weaponData.price.mods;
  weaponData.scopeMinimum = getScopeMinimumRange(weaponData.scopeMagnification);

  // TODO: Remove this once special ammo is implemented.
  weaponData.currentAmmo = "STD";

  new Array(
    Object.entries(weaponData.characteristics),
    Object.entries(weaponData.shields)
  ).flat().filter(v => v[0] !== "has").map(calculateItemValues);
}

/**
 * Get the characteristic modifier for a given characteristic score.
 *
 * @param {number} score - The characteristic score.
 * @returns {number} The characteristic modifier.
 */
export function getCharacteristicModifier(score) {
  return score < 0 ? 0 : Math.floor(score / 10);
}

/**
 * Process current magazine and ammo pool values for a weapon upon reloading.
 *
 * @param {ItemData} weaponData - The Weapon Item data.
 */
export function handleReloadMagCount(weaponData) {
  const currentAmmo = weaponData.currentAmmo;
  const ammo = weaponData.ammoList[currentAmmo];
  const magCurrent = weaponData.ammoList[currentAmmo].currentMag;
  const magCapacity = weaponData.magazineCapacity;
  const tracking = game.settings.get("mythic", "ammoTracking");
  const isSingleLoading = ammo.special.singleLoading.has;

  if (magCurrent === magCapacity) {
    makeUIWarning("mythic.chat.error.noNeedToReload");
    return weaponData;
  }

  if (tracking === "selfManaged") {
    ammo.currentMag = magCapacity
    return weaponData;
  }

  if (tracking === "magazines" && !isSingleLoading) {
    return calculateReloadMagFed(weaponData, currentAmmo);
  }

  if (tracking === "ammoPool" || isSingleLoading) {
    return calculateReloadAmmoPool(weaponData, {
      magCapacity: magCapacity,
      magCurrent: magCurrent,
      isSingleLoading: isSingleLoading
    });
  }

  return weaponData;
}

/**
 * Prepares all base Actor data for a Bestiary Enemy Actor type.
 *
 * @param {Actor} actor - The Bestiary Enemy Actor data.
 */
export function prepareBestiaryBase(actor) {
  // Calculate Ability Pool
  calculateAbilityPool(actor.system);

  // Calculate Luck
  calculateLuck(actor);

  // Calculate Support Points
  calculateSupportPoints(actor.system);

  // Calculate Experience Payout
  calculateExperiencePayout(actor);

  // Fix Talent Dependencies
  fixTalentDependencies(actor.system);

  // Calculate Weight
  calculateInventoryWeight(actor);

  // Reset characteristic penalties
  resetCharacteristicPenalties(actor.system);
}

/**
 * Prepares all derived Actor data for a Bestiary Enemy Actor type.
 *
 * @param {Actor} actor - The Bestiary Enemy Actor data.
 */
export function prepareBestiaryDerived(actor) {
  // Set Up Armor
  applyArmorStatsToCharacter(actor);
  applyItemCharacteristicsToCharacter(actor);

  // Calculate Mythic Characteristics
  calculateMythicCharacteristics(actor);

  // Calculate Characteristics
  const feltFatigue = calculateFeltFatigue(actor.system.fatigue);
  calculateCharacteristics(actor, feltFatigue);

  // Calculate Perceptive Range
  calculatePerceptiveRange(actor.system);

  // Calculate Education Limit
  calculateEducations(actor);

  // Calculate DR
  calculateDamageResistance(actor.system);

  // Calculate Wounds
  calculateWoundsBestiary(actor.system);

  // Calculate Max Fatigue
  calculateMaxFatigue(actor.system);

  // Calculate Movement Distances
  calculateMovementDistancesWithEncumbrance(actor.system);

  // Calculate Initiative
  calculateInitiative(actor.system, feltFatigue);

  // Calculate Skill Test Target Numbers
  calculateSkillTargets(actor.system);

  // Calculate Education Test Target Numbers
  calculateEducationTargets(actor);

  // Calculate Weapon Attacks
  calculateWeaponSummaryAttackData(actor);
}

/**
 * Prepares all embedded entity data for a Bestiary Enemy or Named Character
 * Actor type.
 *
 * @param {Actor} actor - The prepared Actor data.
 */
export function prepareCharacterEmbedded(actor) {
  // Prepare Armors
  Object.values(actor.items.filter(a => a.type === "armor")).forEach(armor =>
    calculateArmorValues(armor.system)
  );
}

/**
 * Prepares all base Actor data for a Flood Actor type.
 *
 * @param {Actor} actor - The Flood Actor data.
 */
export function prepareFloodBase(actor) {
  // Swarm
  calculateSwarm(actor.system);

  // Experience Payout (must be unique for multiplier)
  calculateExperiencePayout(actor);

  // Wounds
  calculateWoundsFlood(actor.system);

  // Fix Talent Dependencies
  fixTalentDependencies(actor.system);

  // Calculate Weight
  calculateInventoryWeight(actor);
}

/**
 * Prepares all derived Actor Data for a Flood Actor type.
 *
 * @param {Actor} actor - The Flood Actor data.
 */
export function prepareFloodDerived(actor) {
  // Set Up Armor
  applyArmorStatsToCharacter(actor);
  applyItemCharacteristicsToCharacter(actor);

  // Calculate Characteristics
  calculateCharacteristicsFlood(actor.system);

  // Calculate Perceptive Range
  calculatePerceptiveRange(actor.system);

  // Calculate Mythic Characteristics
  calculateMythicCharacteristicsFlood(actor.system);

  // Calculate DR
  calculateDamageResistanceFlood(actor);

  // Calculate Movement Distances
  calculateMovementDistancesBase(actor.system);

  // Calculate Initiative
  calculateInitiativeFlood(actor.system);

  // Calculate Skill Test Target Numbers
  calculateSkillTargets(actor.system);

  // Calculate Weapon Attacks
  calculateWeaponSummaryAttackData(actor);
}

/**
 * Prepares all base Actor data for a Named Character Actor type.
 *
 * @param {Actor} actor - The Named Character Actor data.
 */
export function prepareNamedCharacterBase(actor) {
  // Calculate Ability Pool
  calculateAbilityPool(actor.system);

  // Calculate Experience
  calculateExperience(actor.system);

  // Calculate Luck
  calculateLuck(actor);

  // Calculate Support Points
  calculateSupportPoints(actor.system);

  // Fix Talent Dependencies
  fixTalentDependencies(actor.system);

  // Calculate Weight
  calculateInventoryWeight(actor);

  // Reset characteristic penalties
  resetCharacteristicPenalties(actor.system);
}

/**
 * Prepares all derived Actor data for a Named Character Actor type.
 *
 * @param {Actor} actor - The Named Character Actor data.
 */
export function prepareNamedCharacterDerived(actor) {
  // Set Up Armor
  applyArmorStatsToCharacter(actor);
  applyItemCharacteristicsToCharacter(actor);

  // Calculate Mythic Characteristics
  calculateMythicCharacteristics(actor);

  // Calculate Characteristics
  const feltFatigue = calculateFeltFatigue(actor.system.fatigue);
  calculateCharacteristics(actor, feltFatigue);

  // Calculate Perceptive Range
  calculatePerceptiveRange(actor.system);

  // Calculate Education Limit
  calculateEducations(actor);

  // Calculate DR
  calculateDamageResistance(actor.system);

  // Calculate Wounds
  calculateWoundsNamedCharacter(actor.system);

  // Calculate Max Fatigue
  calculateMaxFatigue(actor.system);

  // Calculate Movement Distances
  calculateMovementDistancesWithEncumbrance(actor.system);

  // Calculate Initiative
  calculateInitiative(actor.system, feltFatigue);

  // Calculate Skill Test Target Numbers
  calculateSkillTargets(actor.system);

  // Calculate Education Test Target Numbers
  calculateEducationTargets(actor);

  // Calculate Weapon Attacks
  calculateWeaponSummaryAttackData(actor);
}

/**
 * Prepares all base Actor data for a Vehicle Actor type.
 *
 * @param {Actor} actor - The Vehicle Actor data.
 */
export function prepareVehicleBase(actor) {
  // TODO
}

/**
 * Prepares all embedded entities for a Vehicle Actor type.
 *
 * @param {Actor} actor - The Vehicle Actor data.
 */
export function prepareVehicleEmbedded(actor) {
  // TODO
}

/**
 * Prepares all derived Actor data for a Vehicle Actor type.
 *
 * @param {Actor} actor - The Vehicle Actor data.
 */
export function prepareVehicleDerived(actor) {
  // TODO
}

/**
 * Updates a list of experience purchases with new indexes.
 *
 * @param {Array.<object>} purchases - List of experience purchases.
 * @returns {Array.<object>} The list of experience records updated with new
 * indexes.
 */
export function setupExperiencePurchases(purchases) {
  for (let i = 0; i < purchases.length; i++) {
    purchases[i].index = i;
  }
  return purchases;
}

/**
 * Filter a list of Items by their type, then sort based on a given parameter.
 *
 * @param {Array.<number>} items - An array of Item objects.
 * @param {string} filterParam - The Item type to filter by.
 * @param {string} sortParam - The Item key sort parameter.
 * @returns {Array.<number>} The filtered and sorted array of Item objects.
 */
export function sortAndFilterItems(items, filterParam, sortParam = "name") {
  let f = items.filter(item => item.type === filterParam);
  if (sortParam === "name") {
    return f.sort((a, b) => a.name < b.name ? -1 : (a.name > b.name ? 1 : 0));
  } else if (sortParam === "nickname") {
    return f.sort((a, b) =>
      a.system.nickname < b.system.nickname
        ? -1 : (a.system.nickname > b.system.nickname ? 1 : 0)
    );
  }
}

function applyArmorStatsToCharacter(actor) {
  const armor = actor.items.filter(a => a.type === "armor" && a.system.weight.equipped)[0];
  const naturalArmor = actor.system.naturalArmor < 0 ? 0 : actor.system.naturalArmor;

  if (!armor) {
    Object.values(actor.system.armor).forEach(loc => loc.protection = naturalArmor);
    return;
  }

  Object.entries(armor.system.protection).forEach(([ key, value ]) =>
    actor.system.armor[key].protection =
      value.total === 0
        ? naturalArmor
        : value.total + Math.floor(naturalArmor / 2)
  );

  if (armor.system.shields.has) {
    actor.system.shields.max = armor.system.shields.integrity.total;
    actor.system.shields.recharge = armor.system.shields.recharge.total;
    actor.system.shields.delay = armor.system.shields.delay.total;
  } else {
    emptyArmorShields(actor.system);
  }
}

function applyItemCharacteristicsToCharacter(actor) {
  emptyArmorCharacteristics(actor.system);

  const items = actor.items.filter(i => i.system.weight.equipped && i.system.characteristics.has);
  Object.values(items).forEach (item => {
    actor.system.characteristics.str.equipment       += item.system.characteristics.str.total;
    actor.system.characteristics.agi.equipment       += item.system.characteristics.agi.total;
    actor.system.mythicCharacteristics.str.equipment += item.system.characteristics.mythicStr.total;
    actor.system.mythicCharacteristics.agi.equipment += item.system.characteristics.mythicAgi.total;
  });
}

function calculateAbilityPool(actorData) {
  actorData.characteristics.extra.poolTotal = (
      actorData.characteristics.str.abilityPool
    + actorData.characteristics.tou.abilityPool
    + actorData.characteristics.agi.abilityPool
    + actorData.characteristics.wfr.abilityPool
    + actorData.characteristics.wfm.abilityPool
    + actorData.characteristics.int.abilityPool
    + actorData.characteristics.per.abilityPool
    + actorData.characteristics.crg.abilityPool
    + actorData.characteristics.cha.abilityPool
    + actorData.characteristics.ldr.abilityPool
  );
}

function calculateCarryWeight(actor) {
  const tou = actor.system.characteristics.tou.total;
  const str = (
      actor.system.characteristics.str.total
    + (actor.system.carryingCapacity.imposing ? 10 : 0)
  );

  const touBase = tou * (actor.system.carryingCapacity.doubleTou ? 2 : 1);
  const liftPullTou = touBase * (actor.system.carryingCapacity.strongBack ? 2 : 1);
  const carryBase = (
      (str * (actor.system.carryingCapacity.doubleStr ? 2 : 1)) // STR
    + (actor.system.mythicCharacteristics.str.total * 10) // Mythic STR
    + (actor.system.mythicCharacteristics.tou.total * 10) // Mythic TOU
    + actor.system.carryingCapacity.mod // Modifier
  );

  actor.system.carryingCapacity.carry = carryBase + touBase;
  actor.system.carryingCapacity.lift = 2 * (carryBase + liftPullTou);
  actor.system.carryingCapacity.push = 4 * (carryBase + liftPullTou);

  if (actor.type !== "Flood") calculateInventoryBars(actor.system);
  calculateHearingPenaltyByWeight(actor);
}

function calculateCharacteristics(actor, feltFatigue) {
  // Process STR and TOU first.
  Object.entries(actor.system.characteristics).splice(0, 2).forEach(stat => {
    calculateCharacteristic(actor, feltFatigue, stat)
  });

  // Calculate carry weight.
  calculateCarryWeight(actor);

  // Calculate encumbrance.
  calculateEncumbrance(actor.system);

  // Process remaining stats.
  Object.entries(actor.system.characteristics).splice(2, 8).forEach(stat => {
    calculateCharacteristic(actor, feltFatigue, stat);
  });
}

function calculateCharacteristic(actor, feltFatigue, [ key, value ]) {
  if (actor.type === "Bestiary Character") {
    const diff = parseInt(actor.system.difficulty.tier);
    if (    isNaN(diff)
         || actor.system.difficulty.normalOnly
         || !actor.system.characteristics[key].advances
       ) {
      value.difficulty = 0;
    } else if (diff === 4) {
      value.difficulty = 25;
    } else {
      value.difficulty = diff * 5;
    }
  }

  const total = (
    value.soldierType + value.abilityPool + value.equipment +
    value.background + value.difficulty + value.other +
    (parseInt(value.advancements) * 5) + value.medical
  );

  value.total = Math.floor(total >= 0 ? total : 0);
  const fatiguePenalty = 5 * (feltFatigue < 0 ? 0 : feltFatigue);
  const roll = value.total - fatiguePenalty - value.penalty;
  value.roll = Math.floor(roll > 0 ? roll : 0);
}

function calculateCharacteristicsFlood(actorData) {
  Object.entries(actorData.characteristics).forEach(([ key, value ]) => {
    const calc = value.base + value.equipment + value.medical + value.other;
    const total = Math.floor(calc >= 0 ? calc : 0);
    value.total = total;
    value.roll = total;
  });
}

function calculateDamageResistance(actorData) {
  const touMod = getCharacteristicModifier(actorData.characteristics.tou.total);
  const touSoak = touMod + actorData.mythicCharacteristics.tou.total;
  actorData.characteristics.extra.touDR = touSoak;

  Object.values(actorData.armor).forEach(val => {
    val.resistance = val.protection + touSoak;
  });
}

function calculateDamageResistanceFlood(actor) {
  const touDamageResistance = (
      getCharacteristicModifier(actor.system.characteristics.tou.total)
    + actor.system.mythicCharacteristics.tou.total
  );

  const isWearing = Array.from(actor.items.values()).some(i => {
    return i.type === "armor" && i.system.weight.equipped;
  });

  Object.values(actor.system.armor).forEach(val => {
    if (isWearing) {
      const newProtection = Math.floor(val.protection / 2);
      val.protection = newProtection;
      const soak = touDamageResistance + newProtection;
      val.resistance = soak > 0 ? soak : 0;
    } else {
      val.resistance = touDamageResistance > 0 ? touDamageResistance : 0;
    }
  });
}

function calculateEducations(actor) {
  if (actor.system.educations.unlimited) return;

  actor.system.educations.value = actor.items.filter(item =>
    item.type === "education" && item.system.roll.training !== "none"
  ).length;

  actor.system.educations.max = actor.system.educations.mod + (
      actor.system.educations.intMultiplier
    * getCharacteristicModifier(actor.system.characteristics.int.total)
  );

  actor.system.educations.style =
    actor.system.educations.value > actor.system.educations.max
      ? "color:red" : "";
}

function calculateEducationTargets(actor) {
  let educations = actor.items.filter(item => item.type === "education");
  Object.values(educations).forEach(e => {
    const base = e.system.roll.skill === "int"
      ? actor.system.characteristics.int.roll
      : actor.system.skills[e.system.roll.skill].roll;
    const training = e.system.roll.training !== "none"
      ? parseInt(e.system.roll.training.replace("plus", ""))
      : 0;
    e.system.roll.roll = base + training + e.system.roll.mods;
  });
}

function calculateEncumbrance(actorData) {
  const method = game.settings.get("mythic", "encumbrance");
  if (method === "off") return;

  const str = actorData.characteristics.str.total;
  const felt = actorData.carryingCapacity.felt;
  const carry = actorData.carryingCapacity.carry;
  const overCarry = felt - carry;

  actorData.carryingCapacity.overencumbered = felt > carry;

  let penalty = actorData.characteristics.agi.penalty;
  if (method === "standard") {
    penalty = (
      str > 0
        ? 10 * Math.floor((overCarry > 0 ? overCarry : 0) / str)
        : 1000 // Some arbitrarily high number to ensure AGI is set to 0.
    );
  } else if (method === "simplified") {
    const strMod = (
        actorData.mythicCharacteristics.str.total
      + getCharacteristicModifier(str)
    );

    penalty = (
      strMod > 0
        ? 10 * Math.ceil((overCarry > 0 ? overCarry : 0) / strMod)
        : 1000 // Some arbitrarily high number to ensure AGI is set to 0.
    );
  }

  actorData.characteristics.agi.penalty = Math.floor(
    penalty / (actorData.carryingCapacity.strongman ? 2 : 1)
  );
}

function calculateExperience(actorData) {
  const totalExp = actorData.experience.total;
  const spent = actorData.experience.purchases.reduce(
    (t, a) => t + (isNaN(a.price) ? 0 : a.price), 0
  );
  const current = totalExp - spent;

  actorData.experience.spent = spent;
  actorData.experience.current = totalExp - spent;
  actorData.experience.color = current < 0 ? "red" : "black";

  if (totalExp >= 64000) {
    actorData.experience.tier = 8;
  } else if (totalExp >= 32000) {
    actorData.experience.tier = 7;
  } else if (totalExp >= 16000 ) {
    actorData.experience.tier = 6;
  } else if (totalExp >= 8000) {
    actorData.experience.tier = 5;
  } else if (totalExp >= 4000) {
    actorData.experience.tier = 4;
  } else if (totalExp >= 2000) {
    actorData.experience.tier = 3;
  } else if (totalExp >= 1000) {
    actorData.experience.tier = 2;
  } else if (totalExp >= 500) {
    actorData.experience.tier = 1;
  } else {
    actorData.experience.tier = 0;
  }
}

function calculateExperiencePayout(actor) {
  let base = 0;
  let diffMult = 1;
  if (actor.type === "Flood") {
    base = actor.system.experiencePayout.base;
    diffMult = actor.system.swarm.total;
  } else {
    const tier = getDifficultyFromTier(
      actor.system.difficulty.normalOnly ? "1" : actor.system.difficulty.tier
    );
    actor.system.experiencePayout.tier = tier;
    base = actor.system.experiencePayout.difficulty[tier];
  }

  actor.system.experiencePayout.base = base;
  actor.system.experiencePayout.diffMultiplier = diffMult;
  actor.system.experiencePayout.total = (
    (base * diffMult) + actor.system.experiencePayout.kit
  );
}

function calculateFeltFatigue(f) {
  const current = f.value + (f.encumbrance ? 1 : 0);
  return isNaN(f.enduring) ? current : current - (2 * f.enduring);
}

function calculateGripPenaltyThrown(grip) {
  if (grip === "solid") return 0;
  if (grip === "slight") return 1;
  if ([ "partial", "sloppy" ].includes(grip)) return 2;

  // This should never happen since the grip value is chosen from a select menu,
  // but if it does, we should produce an error.
  makeUIError("mythic.chat.error.unrecognizedGrip");
  return 0;
}

function calculateHearingPenaltyByWeight(actor) {
  const current = actor.type === "Flood"
                ? actor.system.carryingCapacity.total
                : actor.system.carryingCapacity.felt;

  actor.system.carryingCapacity.hearingPenalty =
    current > (actor.system.carryingCapacity.carry / 2);
}

function calculateInitiative(actorData, feltFatigue) {
  const init = actorData.initiative;
  const stats = actorData.characteristics;
  const mythicAgi = actorData.mythicCharacteristics.agi.total;

  let mods = [];
  const dice = actorData.initiative.fastFoot ? "2d10kh" : "1d10";
  mods.push(
    [ getCharacteristicModifier(stats.agi.total)
    , init.battleMind    ? getCharacteristicModifier(stats.int.total) : 0
    , init.grandEntrance ? getCharacteristicModifier(stats.ldr.total) : 0
    ].sort().reverse()[0]
  );

  if (mythicAgi > 0) {
    const bonus = Math.floor(mythicAgi / 2);
    mods.push(bonus > 1 ? bonus : 1);
  };

  mods.push(-5 * (feltFatigue < 0 ? 0 : feltFatigue));

  actorData.initiative.mods = mods.reduce((acc, x) => acc + x, 0);
  actorData.initiative.formula = `${dice}+${mods.join("+")}`;
}

function calculateInitiativeFlood(actorData) {
  const agiMod = getCharacteristicModifier(actorData.characteristics.agi.total);
  const mythicAgi = actorData.mythicCharacteristics.agi.total;
  let bonus = 0;
  if (mythicAgi > 0) {
    bonus = Math.floor(mythicAgi / 2);
    bonus += (bonus > 0 ? 0 : 1);
  }
  const mods = agiMod + bonus;
  actorData.initiative.mods = `${mods > 0 ? "+" : ""}${mods}`;
  actorData.initiative.formula = `1D10 + ${agiMod} + ${bonus}`;
}

function calculateInventoryBars(actorData) {
  const percent = 100 * (
    actorData.carryingCapacity.felt / actorData.carryingCapacity.carry
  );
  if (isNaN(percent)) {
    actorData.carryingCapacity.bar.bgBar = "tranparent";
    actorData.carryingCapacity.bar.bgFill = "transparent";
    actorData.carryingCapacity.bar.width = "0%";
    actorData.carryingCapacity.bar.left = "0.3em";
    actorData.carryingCapacity.bar.tier = "carry";
  } else if (percent >= 400) {
    const adjusted = percent - 400;
    actorData.carryingCapacity.bar.bgBar = "darkred";
    actorData.carryingCapacity.bar.bgFill = "darkred";
    actorData.carryingCapacity.bar.width = "100%";
    actorData.carryingCapacity.bar.left = adjusted <= 4 ? "0.3em" : "0";
    actorData.carryingCapacity.bar.tier = "push";
  } else if (percent > 200) {
    const adjusted = (percent - 200) / 2;
    actorData.carryingCapacity.bar.bgBar = "#fb8c00";
    actorData.carryingCapacity.bar.bgFill = "darkred";
    actorData.carryingCapacity.bar.width = `${adjusted.toFixed(1)}%`;
    actorData.carryingCapacity.bar.left = adjusted <= 4 ? "0.3em" : "0";
    actorData.carryingCapacity.bar.tier = "push";
  } else if (percent > 100) {
    const adjusted = percent - 100;
    actorData.carryingCapacity.bar.bgBar = "rgba(0, 0, 0, 0.5)";
    actorData.carryingCapacity.bar.bgFill = "#fb8c00";
    actorData.carryingCapacity.bar.width = `${adjusted.toFixed(1)}%`;
    actorData.carryingCapacity.bar.left = adjusted <= 4 ? "0.3em" : "0";
    actorData.carryingCapacity.bar.tier = "lift";
  } else {
    actorData.carryingCapacity.bar.bgBar = "transparent";
    actorData.carryingCapacity.bar.bgFill = "rgba(0, 0, 0, 0.5)";
    actorData.carryingCapacity.bar.width = `${percent.toFixed(1)}%`;
    actorData.carryingCapacity.bar.left = percent <= 4 ? "0.3em" : "0";
    actorData.carryingCapacity.bar.tier = "carry";
  }
}

function calculateItemValues(item) {
  const canBeNegative = ["str", "agi", "mythicStr", "mythicAgi"];
  const total =
    (item[1].armor || item[1].item || 0) + item[1].variant + item[1].other;

  if (item < 0 && !canBeNegative.includes(item[0])) {
    item[1].total = 0;
  } else {
    item[1].total = total;
  }
}

function calculateItemWeight(item) {
  if (!item.system.weight.carried) {
    item.system.weight.felt = 0;
    item.system.weight.total = 0;
    return { felt: 0, total: 0 };
  }

  const total = item.system.weight.each * (
    (item.type === "weapon" && item.system.group === "thrown")
      ? item.system.ammoList[item.system.currentAmmo].currentMag
      : item.system.weight.quantity
  );

  if (item.system.weight.equipped && item.system.weight.selfSupported) {
    return { felt: 0, total: total };
  }

  if (item.type === "armor" && item.system.weight.equipped) {
    return { felt: total / 4, total: total };
  }

  return { felt: total, total: total };
}

function calculateInventoryWeight(actor) {
  let felt = 0, total = 0;

  actor.items.filter(
    item => ["armor", "equipment", "weapon"].includes(item.type)
  ).forEach(item => {
    const weight = calculateItemWeight(item);

    if (!item.system.weight.carried) item.system.weight.equipped = false;
    item.system.weight.felt = weight.felt;
    item.system.weight.total = weight.total;

    felt += weight.felt;
    total += weight.total;
  });

  actor.system.carryingCapacity.felt = felt > 0 ? felt : 0;
  actor.system.carryingCapacity.total = total > 0 ? total : 0;
  actor.system.carryingCapacity.character = total + actor.system.weight;
}

function calculateLuck(actor) {
  let difficulty = 0;
  if (actor.type === "Bestiary Character") {
    actor.system.luck.tier = getDifficultyFromTier(
      actor.system.difficulty.normalOnly ? "1" : actor.system.difficulty.tier
    );
    difficulty = actor.system.luck.difficulty[actor.system.luck.tier];
  }

  const max = (
      actor.system.luck.starting
    + actor.system.luck.advancements
    + actor.system.luck.other
    + difficulty
    - actor.system.luck.burnt
  );
  actor.system.luck.max = max > 0 ? max : 0;
}

function calculateMaxFatigue(actorData) {
  actorData.fatigue.max =
    2 * getCharacteristicModifier(actorData.characteristics.tou.total);
}

function calculateMovementDistancesWithEncumbrance(actorData) {
  const felt = actorData.carryingCapacity.felt;
  const carry = actorData.carryingCapacity.carry;
  const push = actorData.carryingCapacity.push;
  const agi  = actorData.characteristics.agi.roll;

  switch (game.settings.get("mythic", "encumbrance")) {
    case "standard":
      if (agi === 0 || felt >= push || (felt / carry) >= 1.5) {
        actorData.fatigue.encumbrance = true;
        calculateMovementDistancesEmpty(actorData);
        return;
      }

      calculateMovementDistancesBase(actorData);
      if (actorData.carryingCapacity.overencumbered) {
        actorData.movement.charge = 0;
        actorData.movement.run = 0;
      }
      return;

    case "simplified":
      if (agi === 0 || felt > actorData.carryingCapacity.lift) {
        actorData.fatigue.encumbrance = true;
        calculateMovementDistancesEmpty(actorData);
        return;
      }

      calculateMovementDistancesBase(actorData);
      if (actorData.carryingCapacity.overencumbered) {
        actorData.movement.charge = 0;
        actorData.movement.run = 0;
      }
      return;

    default: calculateMovementDistancesBase(actorData);
  }
}

function calculateMovementDistancesBase(actorData) {
  const strMod = getCharacteristicModifier(actorData.characteristics.str.total);
  const agiMod = getCharacteristicModifier(actorData.characteristics.agi.roll);
  const base = agiMod + actorData.mythicCharacteristics.agi.total;
  const chargeRunBonus =
    isNaN(actorData.movement.agiBonusRunCharge)
      ? 0 : actorData.movement.agiBonusRunCharge;

  if (base <= 0) {
    actorData.movement.half = 0.5;
    actorData.movement.full = 1;
    actorData.movement.charge = 2;
    actorData.movement.run = 3;
  } else {
    actorData.movement.half = base;
    actorData.movement.full = base * 2;
    actorData.movement.charge = (
      ((base + chargeRunBonus) * 3) + (actorData.movement.rush ? agiMod : 0)
    );
    actorData.movement.run = 6 * (base + chargeRunBonus);
    actorData.movement.sprint = actorData.movement.blur ? base * 8 : "--";
  }

  const strLeap = Math.floor(strMod / 2);
  const agiLeap = Math.floor(agiMod / 2) + actorData.movement.leapAgiBonus;
  actorData.movement.jump = (strMod * actorData.movement.jumpMultiplier) / 4;
  actorData.movement.leap = (
    (strLeap > agiLeap ? strLeap : agiLeap) * actorData.movement.leapMultiplier
  );
}

function calculateMovementDistancesEmpty(actorData) {
  actorData.movement.half = 0;
  actorData.movement.full = 0;
  actorData.movement.charge = 0;
  actorData.movement.run = 0;
  actorData.movement.sprint = actorData.movement.blur ? 0 : "--";
  actorData.movement.jump = 0;
  actorData.movement.leap = 0;
}

function calculateMythicCharacteristics(actor) {
  if (actor.type === "Bestiary Character") {
    calculateMythicDifficulty(actor.system);
  } else {
    actor.system.mythicCharacteristics.str.difficulty = 0;
    actor.system.mythicCharacteristics.tou.difficulty = 0;
    actor.system.mythicCharacteristics.agi.difficulty = 0;
  }

  Object.entries(actor.system.mythicCharacteristics)
    .splice(0, 3) // Strip off the last "notes" object in this array.
    .forEach(([ key, value ]) => {
      const total = value.soldierType
                  + value.equipment
                  + value.other
                  + parseInt(value.advancements)
                  + value.difficulty;
      value.total = total >= 0 ? total : 0;
    });
}

function calculateMythicCharacteristicsFlood(actorData) {
  Object.entries(actorData.mythicCharacteristics)
    .splice(0, 3) // Strip off the last "notes" object in this array.
    .forEach(([ key, value ]) => {
      const total = value.base + value.equipment + value.other;
      value.total = total >= 0 ? total : 0;
    });
}

function calculateMythicDifficulty(actorData) {
  function applyDifficulty(diff) {
    actorData.mythicCharacteristics.str.difficulty = diff;
    actorData.mythicCharacteristics.tou.difficulty = diff;
    actorData.mythicCharacteristics.agi.difficulty = diff;
  }

  if (!actorData.difficulty.advancesMythics) return;
  if (actorData.difficulty.normalOnly) return applyDifficulty(0);

  const difficulty = parseInt(actorData.difficulty.tier);
  if (difficulty === 4) return applyDifficulty(2);
  if (difficulty >=  1) return applyDifficulty(1);
  return applyDifficulty(0);
}

function calculatePerceptiveRange(actorData) {
  const base = actorData.characteristics.per.total * (
    actorData.perceptiveRange.vigil ? 10 : 5
  );
  actorData.perceptiveRange.base = base;
  actorData.perceptiveRange.total = actorData.perceptiveRange.mod + base;
}

function calculateSkillTargetEncumbrancePenalty(actorData) {
  const percentOfCarry = (
    actorData.carryingCapacity.felt / actorData.carryingCapacity.carry
  );

  if (percentOfCarry >= 1.25) return 80;
  if (percentOfCarry > 1) return 40;
  return 0;
}

function calculateSkillTargets(actorData) {
  Object.entries(actorData.skills).forEach(([ key, value ]) => {
    if (key === "notes") return;

    let target = value.mods;
    const stats = actorData.characteristics;

    if (stats[value.characteristic.toLowerCase()]) {
      target += stats[value.characteristic.toLowerCase()].roll;
    }

    const tier = value.training.tier;
    if (tier === "none") {
      target -= (20 * value.training.penalty);
    } else if (tier === "plus10") {
      target += 10;
    } else if (tier === "plus20") {
      target += 20;
    }

    if ( (key === "techHuman" && !actorData.trainings.faction.unsc)
      || (key === "techCovenant" && !actorData.trainings.faction.covenant)
      || (key === "techForerunner" && !actorData.trainings.faction.forerunner)
    ) {
      target -= actorData.trainings.alienTech ? 10 : 20;
    } else if ( key === "evasion"
             && value.characteristic === "WFM"
             && actorData.trainings.weapons.hth
              ) {
      target += 5;
    } else if (key === "intimidation") {
      target += actorData.carryingCapacity.imposing ? 5 : 0;
    }


    const method = game.settings.get("mythic", "encumbrance");
    const encumbered = actorData.carryingCapacity.overencumbered;
    if (method === "standard" && encumbered && PHYSICAL_SKILLS.has(key)) {
      target -= calculateSkillTargetEncumbrancePenalty(actorData);
    }

    value.roll = target > 0 ? target : 0;
  });
}

function calculateSupportPoints(actorData) {
  actorData.supportPoints.max = (
    actorData.supportPoints.rank + actorData.supportPoints.other
  );
}

function calculateSwarm(actorData) {
  if (actorData.swarm.willSwarm) {
    const method = game.settings.get("mythic", "swarmVersion");
    if (method === "contamination") {
      const c = game.settings.get("mythic", "contaminationLevel");
      actorData.swarm.base = 2 * c;
    } else if (method === "difficulty") {
      const d = game.settings.get("mythic", "floodDifficulty");
      actorData.swarm.base = 10 * d;
    } else if (method === "manual") {
      actorData.swarm.base = 1;
    }
    actorData.swarm.total = actorData.swarm.base + actorData.swarm.mod;
  } else {
    actorData.swarm.total = 1;
  }
}

function calculateWeaponAttacksMelee(actorData, weaponData) {
  const wfm = getCharacteristicModifier(actorData.characteristics.wfm.total);
  const extra = weaponData.attack.extraMelee;
  const half = Math.min(4, Math.max(1, Math.floor(wfm / 2)));

  const macMod = (
    actorData.trainings.weapons.hth && actorData.trainings.weapons.mac
  ) ? 1 : 0;

  weaponData.attack.fireMode = "melee";
  weaponData.attack.half = half + extra;
  weaponData.attack.full = extra + Math.min(8, (2 * half) + macMod);
}

function calculateWeaponAttacksRanged(weaponData) {
  const currentAmmo = weaponData.currentAmmo;
  const a = weaponData.attack.fireMode.split("-");
  const mode = a[0], attacks = parseInt(a[1]);
  if (["auto", "sustained"].includes(mode)) {
    const half = Math.floor(attacks / 2);
    const mag = weaponData.ammoList[currentAmmo].currentMag;
    weaponData.attack.half = mag >= half ? half : mag;
    weaponData.attack.full = mag >= attacks ? attacks : mag;
  } else if (["burst", "pump", "semi"].includes(mode)) {
    const full = attacks * 2;
    const mag = weaponData.ammoList[currentAmmo].currentMag;
    weaponData.attack.half = mag >= attacks ? attacks : mag;
    weaponData.attack.full = mag >= full ? full : mag;
  } else if (mode === "charge") {
    weaponData.attack.half = 1;
  } else if (mode === "drawback") {
    weaponData.attack.half = 1;
    weaponData.attack.full = 1;
  } else if (mode === "flintlock") {
    weaponData.attack.full = 1;
  }
}

function calculateWeaponRangeMelee(actorData, weaponData) {
  const currentAmmo = weaponData.currentAmmo;
  weaponData.ammoList[currentAmmo].range.melee = (
      weaponData.ammoList[currentAmmo].range.close
    + MELEE_REACH_SIZE_BONUS[actorData.size]
  );
}

function calculateWeaponRangeThrown(actorData, weaponData) {
  const currentAmmo = weaponData.currentAmmo;
  const bonus = weaponData.ammoList[currentAmmo].range.thrownBonus;
  const base = (
      getCharacteristicModifier(actorData.characteristics.str.total)
    + actorData.mythicCharacteristics.str.total
  );

  let mult = 20;
  const grip = weaponData.ammoList[currentAmmo].range.grip;
  mult -= calculateWeightPenaltyThrown(base, weaponData.weight.each);
  mult -= calculateGripPenaltyThrown(grip);

  weaponData.ammoList[currentAmmo].range.thrownMax =
    bonus + Math.floor(base * 20);

  weaponData.ammoList[currentAmmo].range.thrown = Math.floor(
    ((base * mult) + bonus) / (grip === "sloppy" ? 2 : 1)
  );
}

function calculateWeaponReloadStandard(actorData, weaponData) {
  let base = weaponData.reload.base;
  if (actorData.trainings.weapons.rapidReload) base = Math.ceil(base / 2);
  const agiMod = getCharacteristicModifier(actorData.characteristics.agi.total);
  const wfrMod = getCharacteristicModifier(actorData.characteristics.wfr.total);
  const final = base - Math.floor(agiMod / 2) - Math.floor(wfrMod / 2);
  weaponData.reload.total = final > 0 ? final : 1;
}

function calculateWeaponReloadSingleLoading(actorData, weaponData) {
  const agiMod = getCharacteristicModifier(actorData.characteristics.agi.total);
  const wfrMod = getCharacteristicModifier(actorData.characteristics.wfr.total);
  const rrBonus = actorData.trainings.weapons.rapidReload ? 1 : 0;
  const final = 1 + Math.floor(agiMod / 2) + Math.floor(wfrMod / 2) + rrBonus;
  weaponData.reload.total = final > 3 ? 3 : final;
}

function calculateWeaponTarget(actorData, weaponData) {
  const currentAmmo = weaponData.currentAmmo;
  const group = weaponData.group;
  const mode = weaponData.attack.fireMode.split("-")[0];
  const stat = group === "ranged"
             ? actorData.characteristics.wfr.roll
             : actorData.characteristics.wfm.roll;

  let mod = (
    stat + weaponData.attack.attackBonus +
    weaponData.ammoList[currentAmmo].attackBonus
  );
  if (!actorData.trainings.faction[weaponData.trainings.faction]) {
    mod -= actorData.trainings.alienTech ? 10 : 20;
  }
  if (!actorData.trainings.equipment[weaponData.trainings.equipment]) {
    mod -= 10;
  }
  if (["burst", "semi", "sustained"].includes(mode)) {
    mod += 10;
  }
  if (group === "melee") {
    const hasHTH = actorData.trainings.weapons.hth;
    if (hasHTH) mod += 5;
    if (hasHTH && actorData.trainings.weapons.mac) mod += 5;
  }
  weaponData.ammoList[currentAmmo].target = mod > 0 ? mod : 0;
}

function calculateWeaponSummaryAttackData(actor) {
  const weapons = actor.items.filter(item => item.type === "weapon");
  Object.values(weapons).forEach(weapon => {
    const currentAmmo = weapon.system.currentAmmo;
    if (weapon.system.group === "thrown") {
      calculateWeaponRangeThrown(actor.system, weapon.system);
      weapon.system.attack.half = 1;
      weapon.system.attack.full = 1;
      weapon.system.attack.fireMode = "thrown";
    } else if (weapon.system.group === "melee") {
      calculateWeaponRangeMelee(actor.system, weapon.system);
      calculateWeaponAttacksMelee(actor.system, weapon.system);
    } else if (weapon.system.group === "ranged") {
      calculateWeaponAttacksRanged(weapon.system);
      if (weapon.system.ammoList[currentAmmo].special.singleLoading.has) {
        calculateWeaponReloadSingleLoading(actor.system, weapon.system);
      } else calculateWeaponReloadStandard(actor.system, weapon.system);
    }
    calculateWeaponTarget(actor.system, weapon.system);
  });
}

function calculateWeightPenaltyThrown(mod, weight) {
  if (mod >= 19) return Math.floor(weight / 20);
  if (mod >= 13) return Math.floor(weight / 10);
  if (mod >= 10) return Math.floor(weight / 5);
  if (mod >=  7) return Math.floor(weight / 2);
  if (mod >=  5) return Math.floor(weight / 1);
  if (mod >=  3) return Math.floor(weight / 1) * 2;
  if (mod >=  1) return Math.floor(weight / 0.5) * 2;
  return 0;
}

function calculateWoundsBestiary(actorData) {
  const touMod = getCharacteristicModifier(actorData.characteristics.tou.total);
  const mythicTou = actorData.mythicCharacteristics.tou.total;
  const doubleTou = actorData.wounds.doubleTou ? 2 : 1;
  const diffTier = actorData.difficulty.normalOnly
                 ? 1 : parseInt(actorData.difficulty.tier);

  actorData.wounds.max = (
      (36 + (diffTier * 4))
    + (2 * doubleTou * (touMod + mythicTou))
    + actorData.wounds.other
    + (actorData.wounds.aiDegen * -5)
  );
}

function calculateWoundsFlood(actorData) {
  actorData.wounds.max = (
    actorData.wounds.mod + (actorData.wounds.base * actorData.swarm.total)
  );;
}

function calculateWoundsNamedCharacter(actorData) {
  const touMod = getCharacteristicModifier(actorData.characteristics.tou.total);
  const doubleTou = actorData.wounds.doubleTou ? 2 : 1;
  const mythicTou = actorData.mythicCharacteristics.tou.total;
  actorData.wounds.max = (
      (40 + (2 * doubleTou * (touMod + mythicTou)))
    + actorData.wounds.other
    + (actorData.wounds.aiDegen * -5)
    + (parseInt(actorData.wounds.advancements) * 4)
  );
}

function calculateReloadAmmoPool(weaponData, extraData) {
  const currentAmmo = weaponData.currentAmmo;
  const pool = weaponData.ammoList[currentAmmo].ammoTracking.pool;
  const missingRounds = extraData.magCapacity - extraData.magCurrent;
  const roundsToReload = extraData.isSingleLoading
                       ? weaponData.reload.total
                       : missingRounds;

  if (pool === 0) {
    makeUIError("mythic.chat.error.ammoPoolEmpty");
    return weaponData;
  }

  const reload = Math.min(missingRounds, roundsToReload);
  if (pool >= reload) {
    const newPool = pool - reload;
    if (newPool === 0) {
      makeUIWarning("mythic.chat.error.reloadEmptiesPool");
    } else if (newPool <= extraData.magCapacity) {
      makeUIWarning("mythic.chat.error.poolDownToOneMag");
    }
    weaponData.ammoList[currentAmmo].ammoTracking.pool = newPool;
    weaponData.ammoList[currentAmmo].currentMag =
      extraData.magCurrent + reload;
  } else if (reload > pool) {
    makeUIWarning("mythic.chat.error.reloadEmptiesPool");
    weaponData.ammoList[currentAmmo].ammoTracking.pool = 0;
    weaponData.ammoList[currentAmmo].currentMag =
      extraData.magCurrent + pool;
  }
  return weaponData;
}

function calculateReloadMagFed(weaponData, currentAmmo) {
  const magsCarried = weaponData.ammoList[currentAmmo].ammoTracking.mags;
  if (magsCarried > 0) {
    const newMagsCarried = magsCarried - 1;
    if (newMagsCarried === 0) makeUIWarning("mythic.chat.error.lastMag");
    weaponData.ammoList[currentAmmo].currentMag = weaponData.magazineCapacity;
    weaponData.ammoList[currentAmmo].ammoTracking.mags = newMagsCarried;
  } else {
    makeUIError("mythic.chat.error.outOfMags");
  }
  return weaponData;
}

function emptyArmorCharacteristics(actorData) {
  actorData.characteristics.str.equipment = 0;
  actorData.characteristics.agi.equipment = 0;
  actorData.mythicCharacteristics.str.equipment = 0;
  actorData.mythicCharacteristics.agi.equipment = 0;
}

function emptyArmorShields(actorData) {
  actorData.shields.max = 0;
  actorData.shields.recharge = 0;
  actorData.shields.delay = 0;
}

function fixTalentDependencies(actorData) {
  if (!actorData.trainings.weapons.hth) actorData.trainings.weapons.mac = false;
}

function getDifficultyFromTier(tier) {
  const tierMap = {
    "0": "easy",
    "1": "normal",
    "2": "heroic",
    "3": "legendary",
    "4": "nemesis",
  };
  return tierMap[tier];
}

function getScopeMinimumRange(magnification) {
  const mag = Math.floor(magnification);
  if (mag >= 25) return 10  * mag;
  if (mag >= 21) return 170 + (20 * (mag - 21));
  if (mag >= 10) return 10  * (mag - 5);
  if (mag >=  3) return  5  * (mag - 1);
  if (mag >=  2) return  7;
  return 0;
}

function resetCharacteristicPenalties(actorData) {
  Object.entries(actorData.characteristics)
    .splice(0, 10).forEach(([ _, value ]) => value.penalty = 0);
}
