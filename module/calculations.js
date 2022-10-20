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

/**
 * Calculates armor protection, shield, and characteristics values.
 *
 * @param {ItemData} armorData - The armor's ItemData.
 */
export function calculateArmorValues(armorData, type) {
  const canBeNegative = ["str", "agi", "mythicStr", "mythicAgi"];
  new Array(
    Object.entries(armorData.protection),
    Object.entries(armorData.shields),
    Object.entries(armorData.characteristics),
  ).flat()
   .filter(v => v[0] !== "has")
   .map(x => {
      const total = x[1].armor + x[1].variant + x[1].other;
      if (type === "Flood") {
        const floodTotal = Math.floor(total / 2);
        x[1].total = floodTotal < 0 && !canBeNegative.includes(x[0]) ? 0 : floodTotal;
      } else if (x < 0 && !canBeNegative.includes(x[0])) {
        x[1].total = 0;
      } else {
        x[1].total = total;
      }
   });
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
 * Calculates weapon values.
 *
 * @param {ItemData} weaponData - The weapon's ItemData.
 */
export function calculateWeaponValues(weaponData) {
  weaponData.price.total = weaponData.price.base + weaponData.price.mods;

  // TODO: Remove this once special ammo is implemented.
  weaponData.currentAmmo = "STD";
}

/**
 * Process current magazine and ammo pool values for a weapon upon reloading.
 *
 * @param {ItemData} weaponData - The Weapon Item data.
 */
export function handleReloadMagCount(weaponData) {
  const currentAmmo = weaponData.currentAmmo;
  const magCurrent = weaponData.ammoList[currentAmmo].currentMag;
  const magCapacity = weaponData.magazineCapacity;
  const tracking = game.settings.get("mythic", "ammoTracking");
  const isSingleLoading = weaponData.special.singleLoading.has;

  if (magCurrent === magCapacity) {
    makeUIWarning("mythic.chat.error.noNeedToReload");
    return weaponData;
  }

  if (tracking === "selfManaged") {
    weaponData.ammoList[currentAmmo].currentMag = magCapacity
    return weaponData;
  }

  if (tracking === "magazines" && !isSingleLoading) {
    return calculateReloadMagFed(weaponData, currentAmmo);
  }

  if (tracking === "ammoPool" || isSingleLoading) {
    return calculateReloadAmmoPool(weaponData, {
      currentAmmo: currentAmmo,
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
 * @param {ActorData} actorData - The Bestiary Enemy Actor data.
 */
export function prepareBestiaryBase(actorData) {
  // Calculate Ability Pool
  calculateAbilityPool(actorData);

  // Calculate Luck
  calculateLuck(actorData);

  // Calculate Support Points
  calculateSupportPoints(actorData);

  // Calculate Experience Payout
  calculateExperiencePayout(actorData);

  // Fix Talent Dependencies
  if (!actorData.data.trainings.weapons.hth) actorData.data.trainings.weapons.mac = false;

  // Calculate Weight
  calculateInventoryWeight(actorData);
}

/**
 * Prepares all derived Actor data for a Bestiary Enemy Actor type.
 *
 * @param {ActorData} actorData - The Bestiary Enemy Actor data.
 */
export function prepareBestiaryDerived(actorData) {
  // Set Up Armor
  applyArmorStatsToCharacter(actorData);

  // Calculate Characteristics
  const feltFatigue = calculateFeltFatigue(actorData);
  calculateCharacteristics(actorData, feltFatigue);

  // Calculate Mythic Characteristics
  calculateMythicCharacteristics(actorData);

  // Calculate Carry Weight
  calculateCarryWeight(actorData);

  // Calculate Encumbrance
  calculateEncumbrance(actorData);

  // Calculate DR
  calculateDamageResistance(actorData);

  // Calculate Wounds
  calculateWoundsBestiary(actorData);

  // Calculate Max Fatigue
  calculateMaxFatigue(actorData);

  // Calculate Movement Distances
  calculateMovementDistances(actorData);

  // Calculate Initiative
  calculateInitiative(actorData, feltFatigue);

  // Calculate Skill Test Target Numbers
  calculateSkillTargets(actorData);

  // Calculate Education Test Target Numbers
  calculateEducationTargets(actorData);

  // Calculate Weapon Attacks
  calculateWeaponSummaryAttackData(actorData);
}

/**
 * Prepares all embedded entity data for a Bestiary Enemy or Named Character
 * Actor type.
 *
 * @param {ActorData} actorData - The prepared ActorData.
 */
export function prepareCharacterEmbedded(actorData) {
  // Prepare Armors
  let armors = actorData.items.filter(a => a.type === "armor");
  for (let armor of Object.values(armors)) {
    calculateArmorValues(armor.data.data, actorData.type);
  }
}

/**
 * Prepares all base Actor data for a Flood Actor type.
 *
 * @param {ActorData} actorData - The Flood Actor data.
 */
export function prepareFloodBase(actorData) {
  // Swarm
  calculateSwarm(actorData);

  // Experience Payout (must be unique for multiplier)
  calculateExperiencePayout(actorData);

  // Wounds
  calculateWoundsFlood(actorData);

  // Fix Talent Dependencies
  if (!actorData.data.trainings.weapons.hth) actorData.data.trainings.weapons.mac = false;

  // Calculate Weight
  calculateInventoryWeight(actorData);
}

/**
 * Prepares all derived Actor Data for a Flood Actor type.
 *
 * @param {ActorData} actorData - The Flood Actor data.
 */
export function prepareFloodDerived(actorData) {
  // Set Up Armor
  applyArmorStatsToCharacter(actorData);

  // Calculate Characteristics
  calculateCharacteristicsFlood(actorData);

  // Calculate Mythic Characteristics
  calculateMythicCharacteristicsFlood(actorData);

  // Calculate DR
  calculateDamageResistanceFlood(actorData);

  // Calculate Movement Distances
  calculateMovementDistances(actorData);

  // Calculate Initiative
  calculateInitiativeFlood(actorData);

  // Calculate Skill Test Target Numbers
  calculateSkillTargets(actorData);

  // Calculate Weapon Attacks
  calculateWeaponSummaryAttackData(actorData);
}

/**
 * Prepares all base Actor data for a Named Character Actor type.
 *
 * @param {ActorData} actorData - The Named Character Actor data.
 */
export function prepareNamedCharacterBase(actorData) {
  // Calculate Ability Pool
  calculateAbilityPool(actorData);

  // Calculate Experience
  calculateExperience(actorData);

  // Calculate Luck
  calculateLuck(actorData);

  // Calculate Support Points
  calculateSupportPoints(actorData);

  // Fix Talent Dependencies
  if (!actorData.data.trainings.weapons.hth) actorData.data.trainings.weapons.mac = false;

  // Calculate Weight
  calculateInventoryWeight(actorData);
}

/**
 * Prepares all derived Actor data for a Named Character Actor type.
 *
 * @param {ActorData} actorData - The Named Character Actor data.
 */
export function prepareNamedCharacterDerived(actorData) {
  // Set Up Armor
  applyArmorStatsToCharacter(actorData);

  // Calculate Characteristics
  const feltFatigue = calculateFeltFatigue(actorData);
  calculateCharacteristics(actorData, feltFatigue);

  // Calculate Mythic Characteristics
  calculateMythicCharacteristics(actorData);

  // Calculate Carry Weight
  calculateCarryWeight(actorData);

  // Calculate Encumbrance
  calculateEncumbrance(actorData);

  // Calculate DR
  calculateDamageResistance(actorData);

  // Calculate Wounds
  calculateWoundsNamedCharacter(actorData);

  // Calculate Max Fatigue
  calculateMaxFatigue(actorData);

  // Calculate Movement Distances
  calculateMovementDistances(actorData);

  // Calculate Initiative
  calculateInitiative(actorData, feltFatigue);

  // Calculate Skill Test Target Numbers
  calculateSkillTargets(actorData);

  // Calculate Education Test Target Numbers
  calculateEducationTargets(actorData);

  // Calculate Weapon Attacks
  calculateWeaponSummaryAttackData(actorData);
}

/**
 * Prepares all base Actor data for a Vehicle Actor type.
 *
 * @param {ActorData} actorData - The Vehicle Actor data.
 */
export function prepareVehicleBase(actorData) {
  // TODO
}

/**
 * Prepares all embedded entities for a Vehicle Actor type.
 *
 * @param {ActorData} actorData - The Vehicle Actor data.
 */
export function prepareVehicleEmbedded(actorData) {
  // TODO
}

/**
 * Prepares all derived Actor data for a Vehicle Actor type.
 *
 * @param {ActorData} actorData - The Vehicle Actor data.
 */
export function prepareVehicleDerived(actorData) {
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
  let f = items.filter(function(item) { return item.type === filterParam });
  if (sortParam === "name") {
    return f.sort((a, b) => a.name < b.name ? -1 : (a.name > b.name ? 1 : 0));
  } else if (sortParam === "nickname") {
    return f.sort((a, b) => (
      a.data.nickname < b.data.nickname ? -1 : (a.data.nickname > b.data.nickname ? 1 : 0))
    );
  }
}

function applyArmorStatsToCharacter(actorData) {
  const armor = actorData.items.filter(a => a.type === "armor" && a.data.data.weight.equipped)[0];
  if (armor) {
    for (let [key, value] of Object.entries(armor.data.data.protection)) {
      actorData.data.armor[key].protection = value.total;
    }
    actorData.data.shields.max = armor.data.data.shields.integrity.total;
    actorData.data.shields.recharge = armor.data.data.shields.recharge.total;
    actorData.data.shields.delay = armor.data.data.shields.delay.total;
    actorData.data.characteristics.str.equipment = armor.data.data.characteristics.str.total;
    actorData.data.characteristics.agi.equipment = armor.data.data.characteristics.agi.total;
    actorData.data.mythicCharacteristics.str.equipment = armor.data.data.characteristics.mythicStr.total;
    actorData.data.mythicCharacteristics.agi.equipment = armor.data.data.characteristics.mythicAgi.total;
  };
}

function calculateAbilityPool(actorData) {
  actorData.data.characteristics.extra.poolTotal = (
    actorData.data.characteristics.str.abilityPool +
    actorData.data.characteristics.tou.abilityPool +
    actorData.data.characteristics.agi.abilityPool +
    actorData.data.characteristics.wfr.abilityPool +
    actorData.data.characteristics.wfm.abilityPool +
    actorData.data.characteristics.int.abilityPool +
    actorData.data.characteristics.per.abilityPool +
    actorData.data.characteristics.cr.abilityPool +
    actorData.data.characteristics.ch.abilityPool +
    actorData.data.characteristics.ld.abilityPool
  );
}

function calculateCarryWeight(actorData) {
  const tou = actorData.data.characteristics.tou.total;
  const str = (
      actorData.data.characteristics.str.total
    + (actorData.data.carryingCapacity.imposing ? 5 : 0)
  );

  const touBase = tou * (actorData.data.carryingCapacity.doubleTou ? 2 : 1);
  const liftPullTou = touBase * (actorData.data.carryingCapacity.strongBack ? 2 : 1);
  const carryBase = (
      (str * (actorData.data.carryingCapacity.doubleStr ? 2 : 1)) // STR
    + (actorData.data.mythicCharacteristics.str.total * 10) // Mythic STR
    + (actorData.data.mythicCharacteristics.tou.total * 10) // Mythic TOU
    + actorData.data.carryingCapacity.mod // Modifier
  );

  actorData.data.carryingCapacity.carry = carryBase + touBase;
  actorData.data.carryingCapacity.lift = 2 * (carryBase + liftPullTou);
  actorData.data.carryingCapacity.push = 4 * (carryBase + liftPullTou);

  if (actorData.type !== "Flood") calculateInventoryBars(actorData);
}

function calculateCharacteristics(actorData, feltFatigue) {
  Object.entries(actorData.data.characteristics)
    .splice(0, 10) // Strip off the last "extra" object in this array.
    .forEach(([ key, value ]) => {
      if (actorData.type === "Bestiary Character") {
        const diff = parseInt(actorData.data.difficulty.tier);
        if (isNaN(diff) || actorData.data.difficulty.normalOnly) {
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
        (parseInt(value.advancements) * 5)
      );

      value.total = Math.floor(total >= 0 ? total : 0);
      const roll = value.total + (-5 * (feltFatigue < 0 ? 0 : feltFatigue));
      value.roll = Math.floor(roll > 0 ? roll : 0);
  });
}

function calculateCharacteristicsFlood(actorData) {
  for (const [key, value] of Object.entries(actorData.data.characteristics)) {
    const calc = value.base + value.equipment + value.medical + value.other;
    const total = Math.floor(calc >= 0 ? calc : 0);
    value.total = total;
    value.roll = total;
  }
}

function calculateDamageResistance(actorData) {
  const touMod = getCharacteristicModifier(actorData.data.characteristics.tou.total);
  const touSoak = touMod + actorData.data.mythicCharacteristics.tou.total;
  actorData.data.characteristics.extra.touDR = touSoak;
  for (let val of Object.values(actorData.data.armor)) {
    val.resistance = val.protection + touSoak;
  }
}

function calculateDamageResistanceFlood(actorData) {
  const isWearing = Array.from(actorData.items.values()).some(i => {
    return i.type === "armor" && i.data.data.weight.equipped;
  });
  for (let val of Object.values(actorData.data.armor)) {
    if (isWearing) {
      const soak = val.protection + 6;
      val.resistance = soak > 0 ? soak : 0;
    } else {
      const tou = actorData.data.characteristics.tou.total;
      const touMod = getCharacteristicModifier(tou);
      const touSoak = touMod + actorData.data.mythicCharacteristics.tou.total;
      val.resistance = touSoak > 0 ? touSoak : 0;
    }
  }
}

function calculateEducationTargets(actorData) {
  let educations = actorData.items.filter(function(item) { return item.type === "education" });
  for (let e of Object.values(educations)) {
    const base = e.data.data.roll.skill === "int"
      ? actorData.data.characteristics.int.roll
      : actorData.data.skills[e.data.data.roll.skill].roll;
    const training = e.data.data.roll.training !== "none"
      ? parseInt(e.data.data.roll.training.replace("plus", ""))
      : 0;
    e.data.data.roll.roll = base + training + e.data.data.roll.mods;
  }
}

function calculateEncumbrance(actorData) {
  // TODO
}

function calculateExperience(actorData) {
  const totalExp = actorData.data.experience.total;
  const spent = actorData.data.experience.purchases.reduce(
    (t, a) => t + (isNaN(a.price) ? 0 : a.price), 0
  );
  const current = totalExp - spent;
  actorData.data.experience.spent = spent;
  actorData.data.experience.current = totalExp - spent;
  actorData.data.experience.color = current < 0 ? "red" : "black";
  if (totalExp >= 32000) {
    actorData.data.experience.tier = 7;
  } else if (totalExp >= 16000 ) {
    actorData.data.experience.tier = 6;
  } else if (totalExp >= 8000) {
    actorData.data.experience.tier = 5;
  } else if (totalExp >= 4000) {
    actorData.data.experience.tier = 4;
  } else if (totalExp >= 2000) {
    actorData.data.experience.tier = 3;
  } else if (totalExp >= 1000) {
    actorData.data.experience.tier = 2;
  } else if (totalExp >= 500) {
    actorData.data.experience.tier = 1;
  } else {
    actorData.data.experience.tier = 0;
  }
}

function calculateExperiencePayout(actorData) {
  let diffMult = 1;
  if (actorData.type === "Flood") {
    diffMult = actorData.data.swarm.total;
  } else if (!actorData.data.difficulty.normalOnly) {
    diffMult = parseInt(actorData.data.difficulty.tier) + 1;
  }
  actorData.data.experiencePayout.diffMultiplier = diffMult;
  actorData.data.experiencePayout.total = (
    (actorData.data.experiencePayout.base * diffMult) + actorData.data.experiencePayout.kit
  );
}

function calculateFeltFatigue(actorData) {
  const f = actorData.data.fatigue;
  const resist = parseInt(f.enduring);
  return isNaN(resist) ? f.value : f.value - (2 * resist);
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

function calculateInitiative(actorData, feltFatigue) {
  const agiMod = getCharacteristicModifier(actorData.data.characteristics.agi.total);
  const intMod = getCharacteristicModifier(actorData.data.characteristics.int.total);
  const mythicAgi = actorData.data.mythicCharacteristics.agi.total;
  const battlemind = actorData.data.initiative.battleMind;
  let formula = [];
  formula.push(actorData.data.initiative.fastFoot ? "2d10kh" : "1d10");
  formula.push((battlemind ? intMod : agiMod).toString());
  if (mythicAgi > 0) {
    const bonus = Math.floor(mythicAgi / 2);
    formula.push(bonus > 1 ? bonus : 1);
  };
  formula.push(-5 * (feltFatigue < 0 ? 0 : feltFatigue));
  const mods = interpretDiceRollModifiers(formula.slice(1).join("+"));
  actorData.data.initiative.mods = (mods > 0 ? "+" : "") + mods.toString();
  actorData.data.initiative.formula = formula.join("+");
}

function calculateInitiativeFlood(actorData) {
  const agiMod = getCharacteristicModifier(actorData.data.characteristics.agi.total);
  const mythicAgi = actorData.data.mythicCharacteristics.agi.total;
  let bonus = 0;
  if (mythicAgi > 0) {
    bonus = Math.floor(mythicAgi / 2);
    bonus += (bonus > 0 ? 0 : 1);
  }
  const mods = agiMod + bonus;
  actorData.data.initiative.mods = `${mods > 0 ? "+" : ""}${mods}`;
  actorData.data.initiative.formula = `1D10 + ${agiMod} + ${bonus}`;
}

function calculateInventoryBars(actorData) {
  const percent = 100 * (
    actorData.data.carryingCapacity.felt / actorData.data.carryingCapacity.carry
  );
  if (isNaN(percent)) {
    actorData.data.carryingCapacity.bar.bgBar = "tranparent";
    actorData.data.carryingCapacity.bar.bgFill = "transparent";
    actorData.data.carryingCapacity.bar.width = "0%";
    actorData.data.carryingCapacity.bar.left = "0.3em";
    actorData.data.carryingCapacity.bar.tier = "carry";
  } else if (percent >= 400) {
    const adjusted = percent - 400;
    actorData.data.carryingCapacity.bar.bgBar = "darkred";
    actorData.data.carryingCapacity.bar.bgFill = "darkred";
    actorData.data.carryingCapacity.bar.width = "100%";
    actorData.data.carryingCapacity.bar.left = adjusted <= 4 ? "0.3em" : "0";
    actorData.data.carryingCapacity.bar.tier = "push";
  } else if (percent > 200) {
    const adjusted = (percent - 200) / 2;
    actorData.data.carryingCapacity.bar.bgBar = "#fb8c00";
    actorData.data.carryingCapacity.bar.bgFill = "darkred";
    actorData.data.carryingCapacity.bar.width = `${adjusted.toFixed(1)}%`;
    actorData.data.carryingCapacity.bar.left = adjusted <= 4 ? "0.3em" : "0";
    actorData.data.carryingCapacity.bar.tier = "push";
  } else if (percent > 100) {
    const adjusted = percent - 100;
    actorData.data.carryingCapacity.bar.bgBar = "rgba(0, 0, 0, 0.5)";
    actorData.data.carryingCapacity.bar.bgFill = "#fb8c00";
    actorData.data.carryingCapacity.bar.width = `${adjusted.toFixed(1)}%`;
    actorData.data.carryingCapacity.bar.left = adjusted <= 4 ? "0.3em" : "0";
    actorData.data.carryingCapacity.bar.tier = "lift";
  } else {
    actorData.data.carryingCapacity.bar.bgBar = "transparent";
    actorData.data.carryingCapacity.bar.bgFill = "rgba(0, 0, 0, 0.5)";
    actorData.data.carryingCapacity.bar.width = `${percent.toFixed(1)}%`;
    actorData.data.carryingCapacity.bar.left = percent <= 4 ? "0.3em" : "0";
    actorData.data.carryingCapacity.bar.tier = "carry";
  }
}

function calculateItemWeight(item) {
  let felt = 0, total = 0;

  if (item.data.data.weight.carried) {
    const weight = item.data.data.weight.each * (
      (item.type === "weapon" && item.data.data.group === "thrown")
        ? item.data.data.ammoList[item.data.data.currentAmmo].currentMag
        : item.data.data.weight.quantity
    );

    total += weight;
    item.data.data.weight.total = weight;

    if (!item.data.data.weight.selfSupported) {
      if (item.type === "armor" && item.data.data.weight.equipped) {
        const quarter = weight / 4;
        felt += quarter;
        item.data.data.weight.felt = quarter;
      } else {
        felt += weight;
        item.data.data.weight.felt = weight;
      }
    } else {
      item.data.data.weight.felt = 0
    }
  } else {
    item.data.data.weight.felt = 0;
    item.data.data.weight.total = 0;
  }
  return {felt: felt, total: total};
}

function calculateInventoryWeight(actorData) {
  let felt = 0, total = 0;

  actorData.items.filter(
    item => ["armor", "equipment", "weapon"].includes(item.type)
  ).forEach(item => {
    if (!item.data.data.weight.carried) item.data.data.weight.equipped = false;
    const weight = calculateItemWeight(item);
    felt += weight.felt;
    total += weight.total;
  });

  actorData.data.carryingCapacity.total = total > 0 ? total : 0;
  actorData.data.carryingCapacity.character = total + actorData.data.weight;

  if (actorData.type === "Flood") {
    actorData.data.carryingCapacity.hearing = Math.floor((total > 0 ? total : 0) / 10);
  } else {
    actorData.data.carryingCapacity.felt = felt > 0 ? felt : 0;
    actorData.data.carryingCapacity.hearing = Math.floor((felt > 0 ? felt : 0) / 10);
  }
}

function calculateLuck(actorData) {
  if (actorData.type === "Bestiary Character") {
    const difficulty = parseInt(actorData.data.difficulty.tier);
    if (actorData.data.difficulty.normalOnly) {
      actorData.data.luck.difficulty = 0;
    } else if (difficulty === 4) {
      actorData.data.luck.difficulty = 3;
    } else if (difficulty === 3) {
      actorData.data.luck.difficulty = 1;
    } else {
      actorData.data.luck.difficulty = 0;
    }
  } else {
    actorData.data.luck.difficulty = 0;
  }

  const max = (
    actorData.data.luck.starting + actorData.data.luck.advancements +
    actorData.data.luck.other + actorData.data.luck.difficulty - actorData.data.luck.burnt
  );
  actorData.data.luck.max = max > 0 ? max : 0;
}

function calculateMaxFatigue(actorData) {
  actorData.data.fatigue.max =
    2 * getCharacteristicModifier(actorData.data.characteristics.tou.total);
}

function calculateMovementDistances(actorData) {
  const strMod = getCharacteristicModifier(actorData.data.characteristics.str.total);
  const agiMod = getCharacteristicModifier(actorData.data.characteristics.agi.total);
  const base = agiMod + actorData.data.mythicCharacteristics.agi.total;
  const chargeRunBonus =
    isNaN(actorData.data.movement.agiBonusRunCharge)
      ? 0 : actorData.data.movement.agiBonusRunCharge;

  if (base <= 0) {
    actorData.data.movement.half = 0.5;
    actorData.data.movement.full = 1;
    actorData.data.movement.charge = 2;
    actorData.data.movement.run = 3;
  } else {
    actorData.data.movement.half = base;
    actorData.data.movement.full = base * 2;
    actorData.data.movement.charge = (
      ((base + chargeRunBonus) * 3) + (actorData.data.movement.rush ? agiMod : 0)
    );
    actorData.data.movement.run = 6 * (base + chargeRunBonus);
    actorData.data.movement.sprint = actorData.data.movement.blur ? base * 8 : "--";
  }
  const strLeap = Math.floor(strMod / 2);
  const agiLeap = Math.floor(agiMod / 2) + actorData.data.movement.leapAgiBonus;
  actorData.data.movement.jump = (strMod * actorData.data.movement.jumpMultiplier) / 4;
  actorData.data.movement.leap = (
    (strLeap > agiLeap ? strLeap : agiLeap) * actorData.data.movement.leapMultiplier
  );
}

function calculateMythicCharacteristics(actorData) {
  if (actorData.type === "Bestiary Character") {
    calculateMythicDifficulty(actorData);
  } else {
    actorData.data.mythicCharacteristics.str.difficulty = 0;
    actorData.data.mythicCharacteristics.tou.difficulty = 0;
    actorData.data.mythicCharacteristics.agi.difficulty = 0;
  }

  Object.entries(actorData.data.mythicCharacteristics)
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
  for (const [key, value] of Object.entries(actorData.data.mythicCharacteristics)) {
    const total = value.base + value.equipment + value.other;
    value.total = total >= 0 ? total : 0;
  }
}

function calculateMythicDifficulty(actorData) {
  if (actorData.data.difficulty.advancesMythics) {
    const difficulty = parseInt(actorData.data.difficulty.tier);
    if (difficulty === 4) {
      actorData.data.mythicCharacteristics.str.difficulty = 2;
      actorData.data.mythicCharacteristics.tou.difficulty = 1;
      actorData.data.mythicCharacteristics.agi.difficulty = 1;
    } else if (difficulty === 3) {
      actorData.data.mythicCharacteristics.str.difficulty = 1;
      actorData.data.mythicCharacteristics.tou.difficulty = 1;
      actorData.data.mythicCharacteristics.agi.difficulty = 1;
    } else if (difficulty === 2) {
      actorData.data.mythicCharacteristics.str.difficulty = 1;
      actorData.data.mythicCharacteristics.tou.difficulty = 1;
      actorData.data.mythicCharacteristics.agi.difficulty = 0;
    } else if (difficulty === 1) {
      actorData.data.mythicCharacteristics.str.difficulty = 1;
      actorData.data.mythicCharacteristics.tou.difficulty = 0;
      actorData.data.mythicCharacteristics.agi.difficulty = 0;
    } else {
      actorData.data.mythicCharacteristics.str.difficulty = 0;
      actorData.data.mythicCharacteristics.tou.difficulty = 0;
      actorData.data.mythicCharacteristics.agi.difficulty = 0;
    }
  }
}

function calculateSkillTargets(actorData) {
  for (const [key, value] of Object.entries(actorData.data.skills)) {
    if (key != "notes") {
      let target = value.mods;
      const stats = actorData.data.characteristics;
      target += stats[value.characteristic.toLowerCase()].roll;
      const tier = value.training.tier;

      if (tier === "none") {
        target -= (20 * value.training.penalty);
      } else if (tier === "plus10") {
        target += 10;
      } else if (tier === "plus20") {
        target += 20;
      }

      if ( (key === "techHuman" && !actorData.data.trainings.faction.unsc)
        || (key === "techCovenant" && !actorData.data.trainings.faction.covenant)
        || (key === "techForerunner" && !actorData.data.trainings.faction.forerunner)
      ) {
        target -= actorData.data.trainings.alienTech ? 10 : 20;
      } else if ( key === "evasion"
               && value.characteristic === "WFM"
               && actorData.data.trainings.weapons.hth
                ) {
        target += 5;
      }

      value.roll = target > 0 ? target : 0;
    }
  }
}

function calculateSupportPoints(actorData) {
  actorData.data.supportPoints.max = (
    actorData.data.supportPoints.rank + actorData.data.supportPoints.other
  );
}

function calculateSwarm(actorData) {
  if (actorData.data.swarm.willSwarm) {
    const method = game.settings.get("mythic", "swarmVersion");
    if (method === "contamination") {
      const c = game.settings.get("mythic", "contaminationLevel");
      actorData.data.swarm.base = 2 * c;
    } else if (method === "difficulty") {
      const d = game.settings.get("mythic", "floodDifficulty");
      actorData.data.swarm.base = 10 * d;
    } else if (method === "manual") {
      actorData.data.swarm.base = 1;
    }
    actorData.data.swarm.total = actorData.data.swarm.base + actorData.data.swarm.mod;
  } else {
    actorData.data.swarm.total = 1;
  }
}

function calculateWeaponAttacksMelee(actorData, weapon) {
  const mkBase = n => Math.floor(
    (n + getCharacteristicModifier(actorData.data.characteristics.wfm.total)) / 2
  );

  const macMod = (
       actorData.data.trainings.weapons.hth
    && actorData.data.trainings.weapons.mac
  ) ? 1 : 0;

  weapon.data.data.attack.half = Math.min(4, Math.max(1, mkBase(0)));
  weapon.data.data.attack.full = Math.min(8, 2 * mkBase(macMod));
  weapon.data.data.attack.fireMode = "melee";
}

function calculateWeaponAttacksRanged(weapon) {
  const currentAmmo = weapon.data.data.currentAmmo;
  const a = weapon.data.data.attack.fireMode.split("-");
  const mode = a[0], attacks = parseInt(a[1]);
  if (["auto", "sustained"].includes(mode)) {
    const half = Math.floor(attacks / 2);
    const mag = weapon.data.data.ammoList[currentAmmo].currentMag;
    weapon.data.data.attack.half = mag >= half ? half : mag;
    weapon.data.data.attack.full = mag >= attacks ? attacks : mag;
  } else if (["burst", "pump", "semi"].includes(mode)) {
    const full = attacks * 2;
    const mag = weapon.data.data.ammoList[currentAmmo].currentMag;
    weapon.data.data.attack.half = mag >= attacks ? attacks : mag;
    weapon.data.data.attack.full = mag >= full ? full : mag;
  } else if (mode === "charge") {
    weapon.data.data.attack.half = 1;
  } else if (mode === "drawback") {
    weapon.data.data.attack.half = 1;
    weapon.data.data.attack.full = 1;
  } else if (mode === "flintlock") {
    weapon.data.data.attack.full = 1;
  }
}

function calculateWeaponRangeMelee(actorData, weapon) {
  const currentAmmo = weapon.data.data.currentAmmo;
  weapon.data.data.ammoList[currentAmmo].range.melee = (
      weapon.data.data.ammoList[currentAmmo].range.close
    + MELEE_REACH_SIZE_BONUS[actorData.data.size]
  );
}

function calculateWeaponRangeThrown(actorData, weapon) {
  const currentAmmo = weapon.data.data.currentAmmo;
  const base = (
      getCharacteristicModifier(actorData.data.characteristics.str.total)
    + actorData.data.mythicCharacteristics.str.total
  );

  let mult = 20;
  const grip = weapon.data.data.ammoList[currentAmmo].range.grip;
  mult -= calculateWeightPenaltyThrown(base, weapon.data.data.weight.each);
  mult -= calculateGripPenaltyThrown(grip);

  weapon.data.data.ammoList[currentAmmo].range.thrownMax = Math.floor(base * 20);
  weapon.data.data.ammoList[currentAmmo].range.thrown = Math.floor(
    (base * mult) / (grip === "sloppy" ? 2 : 1)
  );
}

function calculateWeaponReloadStandard(actorData, weapon) {
  let base = weapon.data.data.reload.base;
  if (actorData.data.trainings.weapons.rapidReload) base = Math.ceil(base / 2);
  const agiMod = getCharacteristicModifier(actorData.data.characteristics.agi.total);
  const wfrMod = getCharacteristicModifier(actorData.data.characteristics.wfr.total);
  const final = base - Math.floor(agiMod / 2) - Math.floor(wfrMod / 2);
  weapon.data.data.reload.total = final > 0 ? final : 1;
}

function calculateWeaponReloadSingleLoading(actorData, weapon) {
  const agiMod = getCharacteristicModifier(actorData.data.characteristics.agi.total);
  const wfrMod = getCharacteristicModifier(actorData.data.characteristics.wfr.total);
  const rrBonus = actorData.data.trainings.weapons.rapidReload ? 1 : 0;
  const final = 1 + Math.floor(agiMod / 2) + Math.floor(wfrMod / 2) + rrBonus;
  weapon.data.data.reload.total = final > 3 ? 3 : final;
}

function calculateWeaponTarget(actorData, weapon) {
  const currentAmmo = weapon.data.data.currentAmmo;
  const group = weapon.data.data.group;
  const mode = weapon.data.data.attack.fireMode.split("-")[0];
  const stat = group === "ranged"
    ? actorData.data.characteristics.wfr.roll
    : actorData.data.characteristics.wfm.roll;
  let mod = (
    stat + weapon.data.data.attack.attackBonus +
    weapon.data.data.ammoList[currentAmmo].attackBonus
  );
  if (!actorData.data.trainings.faction[weapon.data.data.trainings.faction]) {
    mod -= actorData.data.trainings.alienTech ? 10 : 20;
  }
  if (!actorData.data.trainings.equipment[weapon.data.data.trainings.equipment]) {
    mod -= 10;
  }
  if (["burst", "semi", "sustained"].includes(mode)) {
    mod += 10;
  }
  if (group === "melee") {
    const hasHTH = actorData.data.trainings.weapons.hth;
    if (hasHTH) mod += 5;
    if (hasHTH && actorData.data.trainings.weapons.mac) mod += 5;
  }
  weapon.data.data.ammoList[currentAmmo].target = mod > 0 ? mod : 0;
}

function calculateWeaponSummaryAttackData(actorData) {
  let weapons = actorData.items.filter(function(item) { return item.type === "weapon" });
  for (let weapon of Object.values(weapons)) {
    if (weapon.data.data.group === "thrown") {
      calculateWeaponRangeThrown(actorData, weapon);
      weapon.data.data.attack.half = 1;
      weapon.data.data.attack.full = 1;
      weapon.data.data.attack.fireMode = "thrown";
    } else if (weapon.data.data.group === "melee") {
      calculateWeaponRangeMelee(actorData, weapon);
      calculateWeaponAttacksMelee(actorData, weapon);
    } else if (weapon.data.data.group === "ranged") {
      calculateWeaponAttacksRanged(weapon);
      if (weapon.data.data.special.singleLoading.has) {
        calculateWeaponReloadSingleLoading(actorData, weapon);
      } else calculateWeaponReloadStandard(actorData, weapon);
    }
    calculateWeaponTarget(actorData, weapon);
  }
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

function calculateWoundsBestiary (actorData) {
  const touMod = getCharacteristicModifier(actorData.data.characteristics.tou.total);
  const mythicTou = actorData.data.mythicCharacteristics.tou.total;
  const doubleTou = actorData.data.wounds.doubleTou ? 2 : 1;
  const diffTier = parseInt(actorData.data.difficulty.tier);

  const wounds = 1 + (diffTier / 10);
  const addition = 36 + (diffTier * 4);
  actorData.data.wounds.max = (
    actorData.data.wounds.other + (actorData.data.wounds.aiDegen * -5) +
    (addition + Math.floor(2 * ((doubleTou * touMod) + mythicTou) * wounds))
  );
}

function calculateWoundsFlood(actorData) {
  const w = actorData.data.wounds.base * actorData.data.swarm.total;
  actorData.data.wounds.max = w + actorData.data.wounds.mod
}

function calculateWoundsNamedCharacter(actorData) {
  const touMod = getCharacteristicModifier(actorData.data.characteristics.tou.total);
  const doubleTou = actorData.data.wounds.doubleTou ? 2 : 1;
  const mythicTou = actorData.data.mythicCharacteristics.tou.total;
  actorData.data.wounds.max = 40 + ((2 * ((doubleTou * touMod) + mythicTou)) +
    actorData.data.wounds.other + (actorData.data.wounds.aiDegen * -5) +
    (parseInt(actorData.data.wounds.advancements) * 4)
  );
}

function calculateReloadAmmoPool(weaponData, extraData) {
  const pool = weaponData.ammoList[extraData.currentAmmo].ammoTracking.pool;
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
    weaponData.ammoList[extraData.currentAmmo].ammoTracking.pool = newPool;
    weaponData.ammoList[extraData.currentAmmo].currentMag =
      extraData.magCurrent + reload;
  } else if (reload > pool) {
    makeUIWarning("mythic.chat.error.reloadEmptiesPool");
    weaponData.ammoList[extraData.currentAmmo].ammoTracking.pool = 0;
    weaponData.ammoList[extraData.currentAmmo].currentMag =
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
