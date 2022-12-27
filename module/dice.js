/** @module dice */

import { getCharacteristicModifier } from "./calculations.js";
import * as Chat from "./chat.js";
import { localize, makeUIError, makeUIWarning } from "./common.js";
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
  const mods = str.trim().match(/\s*[+\-]?\s*\d+\s*/g);
  const num = mods.reduce((acc, mod) => acc + parseInt(mod), 0);

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

  if ([ atkMod, dmgMod ].some(isNaN)) {
    makeUIError("mythic.chat.error.nan");
    return;
  }

  if (isNaN(distanceFromTarget) || distanceFromTarget < 0) {
    makeUIWarning("mythic.chat.error.distanceFromTargetDefaulted");
  }

  const currentAmmo = weapon.system.currentAmmo;
  const rangeEffects =
    calculateRangeEffects(actor, weapon, distanceFromTarget);

  if (rangeEffects.error) {
    makeUIError(rangeEffects.error);
    return;
  }

  const target = (
      weapon.system.ammoList[currentAmmo].target
    + atkMod
    + rangeEffects.target
    + calculatePerceptiveRangePenalties(actor, weapon, distanceFromTarget)
  );

  const data = {
    circDmg: dmgMod,
    rangeDamage: rangeEffects.damage,
    distanceFromTarget: distanceFromTarget,
    isZeroG: attackOptions.isZeroG,
    isVehicle: attackOptions.targetVehicle,
    target: target,
    type: element.value,
    pierce: rangeEffects.pierce
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

function calculatePerceptiveRangePenalties(actor, weapon, distanceFromTarget) {
  if (!game.settings.get("mythic", "rangeEffects")) return 0;

  const pRange = actor.system.perceptiveRange.total * (
    isNaN(weapon.system.scopeMagnification) ? 1 : weapon.system.scopeMagnification
  );

  if (pRange >= distanceFromTarget) return 0;
  return 10 * Math.floor((pRange - distanceFromTarget) / 50);
}

function calculateRangeEffects(actorData, weapon, distanceFromTarget) {
  const noChanges = { target: 0, pierce: "full", damage: "0" };

  if (!game.settings.get("mythic", "rangeEffects")) return noChanges;

  const ammo = weapon.system.ammoList[weapon.system.currentAmmo];
  const spread = ammo.special.spread.has;
  switch(weapon.system.group) {
    case "melee":
      if (1 >= distanceFromTarget) {
        return { target: 10, pierce: "full", damage: "0" };
      }

      if (ammo.range.melee >= distanceFromTarget) return noChanges;
      break;

    case "thrown":
      if (ammo.range.thrown >= distanceFromTarget) return noChanges;
      break;

    case "ranged":
      if (ammo.special.longBarrel.has && 3 >= distanceFromTarget) {
        return {
          target: actorData.trainings.weapons.quickscope ? -5 : -10,
          pierce: "full",
          damage: spread ? "2D10" : "0"
        };
      }

      if (ammo.special.longBarrel.has && 10 >= distanceFromTarget) {
        return {
          target: actorData.trainings.weapons.quickscope ? -5 : -10,
          pierce: "full",
          damage: spread ? "1D10" : "0"
        };
      }

      if (ammo.special.longBarrel.has && ammo.range.close >= distanceFromTarget) {
        return { target: 0, pierce: "full", damage: spread ? "1D10" : "0" };
      }


      if (ammo.range.close >= 3 && 3 >= distanceFromTarget) {
        return { target: 20, pierce: "full", damage: spread ? "2D10" : "0" };
      }

      if (ammo.range.close >= distanceFromTarget) {
        return { target: 5, pierce: "full", damage: spread ? "1D10" : "0" };
      }

      if (ammo.range.long >= distanceFromTarget) return noChanges;

      if ((2 * ammo.range.long) >= distanceFromTarget) {
        return { target: -40, pierce: "half", damage: spread ? "-1D10" : "0" };
      }

      if ((3 * ammo.range.long) >= distanceFromTarget) {
        return { target: -80, pierce: "none", damage: spread ? "-2D10" : "0" };
      }
      break;
  }
  return { error: "mythic.chat.error.targetOutOfRange" };
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
    name: weapon.system.nickname || weapon.name,
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
      rangeDamage: data.rangeDamage,
      pierce: data.pierce
    };

    const attack = await rollAttackAndDamage(actor, weapon, attackData);
    result.hits += attack.outcome === "success" ? 1 : 0;
    result.attacks.push(attack);
  }
  result.hits *= damagesPerAttack;
  if (result.hits > 0) {
    result.specials = getSpecialRuleValues(result.hits, weapon.system);
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

function getSpecialRuleValues(hits, weaponData) {
  const specialRules = weaponData.ammoList[weaponData.currentAmmo].special;

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
  const ammo = weapon.system.ammoList[weapon.system.currentAmmo];
  const roll = await new Roll(FORMULA).roll({ async: true });
  const outcome = determineRollOutcome(roll.total, data.target);

  let attack = {
    attackNumber: data.attackNumber,
    damages: data.damages,
    roll: roll.total,
    ...outcome
  };

  if (attack.outcome === "success" || ammo.special.blast.has || ammo.special.kill.has) {
    attack.location = await determineHitLocation(reverseDigits(roll.total), data.vehicle);
    const critType = game.settings.get("mythic", "criticalHitResult");

    let damage =
      [ `${ammo.diceQuantity}D${ammo.diceValue}`, data.rangeDamage ].map(pool => {
        if (!pool.toLowerCase().includes("d")) return pool;

        let min = ammo.special.diceMinimum.has
                ? ammo.special.diceMinimum.value
                : 0;

        if (roll.total === 1 && min < 5) min = 5;
        if (min > 0) pool += `min${min}`;

        if (critType !== "special") {
          pool += `${critType}>=${ammo.critsOn}`;
        }

        return pool;
      }).reduce((acc, formula) => {
        return acc === "" ? formula : `${acc}+${formula}`;
      });

    let base = ammo.baseDamage;
    let pierce = ammo.piercing;

    if (weapon.system.group === "melee") {
      const str = (
        actor.system.mythicCharacteristics.str.total +
        getCharacteristicModifier(actor.system.characteristics.str.total)
      );
      base += Math.floor(str * ammo.strDamage);
      pierce += Math.floor(str * ammo.strPiercing);
      if (actor.system.trainings.weapons.unarmedCombatant) {
        const wfm = getCharacteristicModifier(actor.system.characteristics.wfm.total);
        pierce += Math.floor(wfm / 2);
      }
    }

    const sizeBonus = weapon.system.group === "melee"
                    ? SIZE_DAMAGE_BONUS[actor.system.size] : 0;

    const formula = `${damage} + ${base} + ${sizeBonus} + ${data.circDmg}`;
    const dmgResult = await rollDamage(formula, ammo.critsOn);
    attack = { ...attack, ...dmgResult };

    switch(data.pierce) {
      case "full":
        attack.piercing = pierce;
        break;

      case "half":
        attack.piercing = Math.floor(pierce / 2);
        break;

      case "none":
        attack.piercing = 0;
        break;
    }

    if (ammo.special.blast.has || ammo.special.kill.has) {
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

async function rollDamage(formula, critsOn) {
  const roll = await new Roll(formula).roll({ async: true });
  const inline = roll.toAnchor();
  const doesSpecialDamage = (
       (game.settings.get("mythic", "criticalHitResult") === "special")
    && roll.dice.some(die => die.results.some(res => res.result >= critsOn))
  );

  inline.className = "inline-roll inline-result";
  return { damageRoll: inline.outerHTML, doesSpecialDamage: doesSpecialDamage };
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
