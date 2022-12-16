/** @module dice */

import { getCharacteristicModifier } from "./calculations.js";
import * as Chat from "./chat.js";
import { localize, makeUIError } from "./common.js";
import { determineHitLocation } from "./location.js";

const FORMULA = "D100";
const CHARACTERISTICS = {
  "str": "Strength",
  "tou": "Toughness",
  "agi": "Agility",
  "wfr": "Warfare Range",
  "wfm": "Warfare Melee",
  "int": "Intellect",
  "per": "Perception",
  "cr": "Courage",
  "ch": "Charisma",
  "ld": "Leadership"
};

const SIZE_DAMAGE_BONUS = {
  "mini": -2,
  "small": 0,
  "normal": 0,
  "large": 2,
  "huge": 3,
  "hulking": 4,
  "giant": 5,
  "immense": 6,
  "massive": 8,
  "great": 10,
  "monumental": 15,
  "colossal": 20,
  "vast": 30
};

/**
 * Evaluates a string of simple addition and subtraction expressions.
 *
 * @param {string} str - The string representation of the expression.
 * @returns {number} The number value of the evaluated expression.
 */
export function interpretDiceRollModifiers(str) {
  let re = /\s*[+\-]?\s*\d+\s*/g;
  let mods = str.trim().match(re) || [0];
  let num = 0;
  for (let mod of mods) num += parseInt(mod);
  if (isNaN(num)) {
    makeUIError("mythic.chat.error.parseFailure");
    throw new Error(msg);
  } else return num;
}

/**
 * Roll attacks from an Actor sheet.
 *
 * @async
 * @param {Element} element - The HTML element the listener originated from.
 * @param {Actor} actor - The Actor that fired the listener.
 */
export async function rollAttacks(element, actor, weapon) {
  const attackOptions = await getAttackRollOptions();
  if (attackOptions.cancelled) return;

  const atkMod = interpretDiceRollModifiers(attackOptions.circumstance.attack);
  const dmgMod = interpretDiceRollModifiers(attackOptions.circumstance.damage);
  const distanceFromTarget = parseFloat(attackOptions.distanceFromTarget);

  if ([ atkMod, dmgMod, distanceFromTarget ].some(isNaN)) {
    makeUIError("mythic.chat.error.nan");
    return;
  }

  const currentAmmo = weapon.system.currentAmmo;
  const data = {
    circDmg: dmgMod,
    distanceFromTarget: distanceFromTarget,
    isZeroG: attackOptions.isZeroG,
    isVehicle: attackOptions.targetVehicle,
    target: weapon.system.ammoList[currentAmmo].target + atkMod,
    type: element.value
  };

  await getAttackAndDamageOutcomes(actor, weapon, data);
  return weapon.system.ammoList[currentAmmo].currentMag - parseInt(element.innerHTML);
}

/**
 * Roll a number of Evasion tests from an Actor sheet.
 *
 * @async
 * @param {Element} element - The HTMl element the listener originated from.
 * @param {Actor} actor - The Actor that fired the listener.
 */
export async function rollEvasionBatch(element, actor) {
  const options = await getEvadeOptions(actor.system.skills.evasion.characteristic);
  let mod = 0;
  if (options.cancelled) {
    return;
  } else mod = interpretDiceRollModifiers(options.circumstance);
  if (isNaN(mod) || isNaN(options.penalty) || isNaN(options.times)) {
    makeUIError("mythic.chat.error.nan");
    options.cancelled = true;
  }
  if (!options.cancelled) await rollEvasions(parseInt(element.value) + mod, options, actor);
}

/**
 * Roll tests from an Actor sheet.
 *
 * @async
 * @param {Element} element - The HTML element from which the listener
 * originated.
 * @param {Actor} actor - The Actor that fired the listener.
 */
export async function rollTest(element, actor) {
  const type = element.classList[0];
  const test = type === "initiative"
    ? capitalize(type)
    : (CHARACTERISTICS[element.name] != undefined
        ? CHARACTERISTICS[element.name]
        : element.name
    );
  const target = parseInt(element.value);
  const testOptions = await getTestOptions(test);
  let mod = 0;
  if (!testOptions.cancelled) mod = interpretDiceRollModifiers(testOptions.circumstance);
  if (isNaN(mod)) {
    makeUIError("mythic.chat.error.nan");
    testOptions.cancelled = true;
  }
  if (!testOptions.cancelled) {
    if (type === "initiative") {
      await rollInitiative(element, mod, actor);
    } else {
      await rollBasicTest(target + mod, test, type, actor);
    }
  }
}

function capitalize(str) {
  return str[0].toUpperCase() + str.slice(1);
}

function determineRollOutcome(roll, target) {
  let outcome = { color: "black", critical: false };
  const d = ((target > 0 ? target : 0) - roll) / 10;
  outcome.degrees = parseFloat(Math.abs(d).toFixed(1));
  if (roll >= game.settings.get("mythic", "criticalFailureThreshold")) {
    outcome.critical = true;
    outcome.outcome = "failure";
    outcome.color = "red";
  } else if (roll === 1) {
    outcome.critical = true;
    outcome.outcome = "success";
    outcome.color = "green";
  } else {
    outcome.outcome = d >= 0 ? "success" : "failure";
  }
  return outcome;
}

async function getAttackAndDamageOutcomes(actor, weapon, data) {
  const fireMode = weapon.system.attack.fireMode.split("-")[0];
  let result = {
    actorId: actor.id,
    name: weapon.system.nickname || weapon.data.name,
    img: weapon.img,
    wfm: actor.system.characteristics.wfm.total,
    weaponData: weapon.system,
    attacks: [],
    hits: 0,
    type: data.type,
    target: data.target > 0 ? data.target : 0,
    template: "attack",
    flavor: getAttackFlavor(weapon.system.group, data.type, fireMode)
  };
  let attacks = 1, damagesPerAttack = 1;
  if (data.type !== "single" && fireMode === "sustained") {
    damagesPerAttack = weapon.system.attack[data.type];
  } else if (data.type !== "single" && fireMode === "burst") {
    attacks = data.type === "full" ? 2 : 1;
    damagesPerAttack = weapon.system.attack.half;
  } else if (data.type !== "single") {
    attacks = weapon.system.attack[data.type];
  }
  for (let i = 1; i <= attacks; i++) {
    const attackData = {
      target: data.target,
      attackNumber: i,
      damages: damagesPerAttack,
      distanceFromTarget: data.distanceFromTarget,
      isZeroG: data.isZeroG,
      vehicle: data.vehicle,
      circDmg: data.circDmg,
    };

    const attack = await rollAttackAndDamage(actor, weapon, attackData);
    result.hits += attack.outcome === "success" ? 1 : 0;
    result.attacks.push(attack);
  }
  result.hits *= damagesPerAttack;
  if (result.hits > 0) {
    result.specials = getSpecialRuleValues(result.hits, weapon.system.special);
  }
  await Chat.postChatMessage(result, actor);
}

function getAttackFlavor(group, type, fireMode) {
  let message = `${capitalize(group)} ${capitalize(type)}
  ${localize("mythic.chat.attack.title")}`;
  if (group === "ranged") {
    message += " - ";
    message += localize(`mythic.weaponSheet.fireMode.${fireMode}`);
  }
  return message;
}

async function getAttackRollOptions() {
  const template = "systems/mythic/templates/chat/attack-dialog.hbs";
  const html = await renderTemplate(template, {});
  return new Promise(resolve => {
    const data = {
      title: game.i18n.format("mythic.chat.attack.title"),
      content: html,
      buttons: {
        roll: {
          label: localize("mythic.chat.actions.roll"),
          callback: html => resolve(_processAttackOptions(html[0].querySelector("form")))
        },
        cancel: {
          label: localize("mythic.chat.actions.cancel"),
          callback: html => resolve({cancelled: true})
        }
      },
      default: "roll",
      close: () => resolve({cancelled: true})
    };
    new Dialog(data, null).render(true);
  });
}

async function getEvadeOptions(stat) {
  const i18nPath = stat === "AGI" ? "evade" : "parry";
  const template = "systems/mythic/templates/chat/evade-dialog.hbs";
  const html = await renderTemplate(template, {});
  return new Promise(resolve => {
    const data = {
      title: game.i18n.format(`mythic.chat.${i18nPath}`),
      content: html,
      buttons: {
        roll: {
          label: localize("mythic.chat.actions.roll"),
          callback: html => resolve(_processEvadeOptions(html[0].querySelector("form")))
        },
        cancel: {
          label: localize("mythic.chat.actions.cancel"),
          callback: html => resolve({cancelled: true})
        }
      },
      default: "roll",
      close: () => resolve({cancelled: true})
    };
    new Dialog(data, null).render(true);
  });
}

function getSpecialRuleValues(hits, specialRules) {
  // TODO: Don't forget this when it's time for special ammo. Check both weapon
  //       and ammo for special rules.
  let specials = {};
  if (specialRules.cryo.has) {
    const formula = specialRules.cryo.value.toLowerCase().split("d");
    specials.cryo = (
      `${hits * parseInt(formula[0])}` +
      (formula.length === 2 ? `D${formula[1]}` : "")
    );
  }
  if (specialRules.flame.has) {
    const formula = specialRules.flame.value.toLowerCase().split("d");
    specials.flame = (
      `${hits * parseInt(formula[0])}` +
      (formula.length === 2 ? `D${formula[1]}` : "")
    );
  }
  if (specialRules.needle.has) {
    const threshold = specialRules.needle.value;
    specials.needle = `${threshold * Math.floor(hits / threshold)}D10`;
  }
  return specials;
}

async function getTestOptions(test) {
  const template = "systems/mythic/templates/chat/test-dialog.hbs";
  const html = await renderTemplate(template, {});
  const title = CHARACTERISTICS[test] != undefined
              ? CHARACTERISTICS[test]
              : test;
  return new Promise(resolve => {
    const data = {
      title: `${game.i18n.format("mythic.chat.test")}: ${title}`,
      content: html,
      buttons: {
        roll: {
          label: localize("mythic.chat.actions.roll"),
          callback: html => resolve(_processTestOptions(html[0].querySelector("form")))
        },
        cancel: {
          label: localize("mythic.chat.actions.cancel"),
          callback: html => resolve({cancelled: true})
        }
      },
      default: "roll",
      close: () => resolve({cancelled: true})
    };
    new Dialog(data, null).render(true);
  });
}

function reverseDigits(roll) {
  let digits = String(roll).split("");
  if (digits.length === 1) {
    digits.push("0");
  } else digits.reverse();
  return parseInt(digits.join(""));
}

async function rollAttackAndDamage(actor, weapon, data) {
  const currentAmmo = weapon.system.currentAmmo;
  const roll = await new Roll(FORMULA).roll({ async: true });
  const outcome = determineRollOutcome(roll.total, data.target);

  let attack = {
    attackNumber: data.attackNumber,
    damages: data.damages,
    roll: roll.total,
    ...outcome
  };

  if (    attack.outcome === "success"
       || weapon.system.special.blast.has
       || weapon.system.special.kill.has
  ) {
    attack.location = await determineHitLocation(reverseDigits(roll.total), data.vehicle);
    const ammo = weapon.system.ammoList[currentAmmo];
    let damage = `${ammo.diceQuantity}D${ammo.diceValue}`;
    let min = weapon.system.special.diceMinimum.has
            ? weapon.system.special.diceMinimum.value
            : 0;

    if (roll.total === 1 && min < 5) min = 5;
    if (min > 0) damage += `min${min}`;
    const critType = game.settings.get("mythic", "criticalHitResult");

    if (critType !== "special") {
      damage += `${critType}>=${ammo.critsOn}`;
    }

    let base = weapon.system.ammoList[currentAmmo].baseDamage;
    let pierce = weapon.system.ammoList[currentAmmo].piercing;

    if (weapon.system.group === "melee") {
      const str = (
        actor.system.mythicCharacteristics.str.total +
        getCharacteristicModifier(actor.system.characteristics.str.total)
      );
      base += Math.floor(str * weapon.system.ammoList[currentAmmo].strDamage);
      pierce += Math.floor(str * weapon.system.ammoList[currentAmmo].strPiercing);
      if (actor.system.trainings.weapons.unarmedCombatant) {
        const wfm = getCharacteristicModifier(actor.system.characteristics.wfm.total);
        pierce += Math.floor(wfm / 2);
      }
    }

    const sizeBonus = weapon.system.group === "melee"
                    ? SIZE_DAMAGE_BONUS[actor.system.size] : 0;
    attack.damageRoll = `${damage} + ${base} + ${sizeBonus} + ${data.circDmg}`;
    attack.piercing = pierce;

    if (weapon.system.special.blast.has || weapon.system.special.kill.has) {
      const scatterData = {
        distance: data.distanceFromTarget,
        degrees: outcome.degrees,
        isZeroG: data.isZeroG,
        vehicle: data.vehicle
      };

      attack.scatter = await scatterAttack(scatterData, weapon);
      attack.apply = true;
    }
  }
  return attack;
}

async function rollBasicTest(target, test, type, actor) {
  const roll = await new Roll(FORMULA).roll({ async: true });
  const outcome = determineRollOutcome(roll.total, target);
  let result = {
    type: type,
    test: test,
    roll: roll.total,
    target: target > 0 ? target : 0,
    critical: false,
    outcome: "",
    template: "test",
    flavor: `${localize("mythic.chat.test")}: ${test}`,
    ...outcome
  };
  await Chat.postChatMessage(result, actor);
}

async function rollEvasions(baseTarget, options, actor) {
  const stat = actor.system.skills.evasion.characteristic;
  const i18n = "mythic.skillNames." + (stat === "AGI" ? "evasion" : "parry");

  let result = {
    evasions: [],
    flavor: `${localize("mythic.chat.test")}: ${localize(i18n)} (${stat})`,
    type: "test",
    template: "evade"
  };

  for (let i = 0; i < options.times; i++) {
    const roll = await new Roll(FORMULA).roll({ async: true });
    let target = baseTarget - (i * options.penalty);
    let outcome = determineRollOutcome(roll.total, target);
    result.evasions.push({
      evasionNumber: i + 1,
      roll: roll.total,
      target: target > 0 ? target : 0,
      ...outcome
    });
  }
  await Chat.postChatMessage(result, actor);
}

async function rollInitiative(element, mod, actor) {
  const dataset = element.dataset;
  if (dataset.roll) {
    const circumstance = ` + ${mod}`;
    const roll = await new Roll(dataset.roll + circumstance, actor.system, { async: true });
    const result = await roll.roll({ async: true });
    result.toMessage({
      speaker: ChatMessage.getSpeaker({ actor: actor }),
      flavor: localize("mythic.characterWeaponSummary.initiative")
    });
  }
}

async function scatterAttack(data, weapon) {
  const currentAmmo = weapon.system.currentAmmo;
  const range = weapon.system.attack.fireMode === "thrown"
              ? weapon.system.ammoList[currentAmmo].range.thrown
              : weapon.system.ammoList[currentAmmo].range.long;

  let msg = "";
  for (let i = 1; i <= (data.isZeroG ? 2 : 1); i++) {
    const direction = await new Roll("1D10").roll({ async: true });
    if (i === 2) msg += " | ";

    let mod = Math.floor(data.degrees) + Math.floor(data.distance / 100);
    if (mod > (range * 3)) {
      mod *= 4;
    } else if (mod > (range * 2)) {
      mod *= 2;
    }
    const dice = await new Roll(`${mod > 1 ? mod : 1}D10`).roll({ async: true });
    msg += `${Chat.getScatterArrow(direction.total)} ${dice.total} m`;
  }
  return msg;
}

function _processAttackOptions(form) {
  return {
    circumstance: {
      attack: form.attackBonus.value,
      damage: form.damageBonus.value
    },
    distanceFromTarget: form.distance.value,
    isZeroG: form.isZeroG.checked,
    targetVehicle: form.targetVehicle.checked
  };
}

function _processEvadeOptions(form) {
  return {
    circumstance: form.circumstance.value,
    penalty: parseInt(form.penalty.value),
    times: parseInt(form.times.value)
  }
}

function _processTestOptions(form) {
  return {
    circumstance: form.circumstance.value
  };
}
