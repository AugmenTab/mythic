/** @module dice */

import * as Chat from "./chat.js";
import * as Common from "./common.js";
import { determineHitLocation } from "./location.js";
import * as Vehicle from "./vehicle.js";

const FORMULA = "D100";
const CHARACTERISTICS = {
  "str": "Strength",
  "tou": "Toughness",
  "agi": "Agility",
  "wfr": "Warfare Range",
  "wfm": "Warfare Melee",
  "int": "Intellect",
  "per": "Perception",
  "crg": "Courage",
  "cha": "Charisma",
  "ldr": "Leadership"
};

const SIZE_DAMAGE_BONUS = {
  "mini":       -3,
  "small":      -1,
  "normal":      0,
  "large":       2,
  "huge":        3,
  "hulking":     4,
  "giant":       5,
  "immense":     6,
  "massive":     8,
  "great":      10,
  "monumental": 15,
  "colossal":   20,
  "vast":       30
};

/**
 * Evaluates a string of simple addition and subtraction expressions. This is
 * capable of handling dice as part of the calculation.
 *
 * @param {string} str - The string representation of the expression.
 * @returns {number} The number value of the evaluated expression.
 */
export function interpretDiceRollModifiers(str) {
  const numsAndDice = Common.partitionArray(
    (x => x.includes("d")),
    str.split(/(?=[+-])/g).map(x => x.trim().toLowerCase().replace("+", ""))
  );

  const nums = numsAndDice.no.reduce((acc, mod) => acc + parseInt(mod), 0);
  if (isNaN(nums)) {
    Common.makeUIError("mythic.chat.error.parseFailure");
    return { flat: 0, dice: [] };
  }

  return { flat: nums, dice: numsAndDice.yes };
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

  const isVehicle = actor.type === "Vehicle";
  const isManned = isVehicle && !actor.system.automated;
  const owner =
    isVehicle && isManned
      ? Vehicle.getRoleOwner(weapon.system.owner)
      : actor;

  const str =
    isVehicle
      ? ( actor.system.characteristics.mythicStr
        + Common.getCharacteristicModifier(actor.system.characteristics.str)
        )
      : ( owner.system.mythicCharacteristics.str.total
        + Common.getCharacteristicModifier(owner.system.characteristics.str.total)
        );

  const dmgMods = interpretDiceRollModifiers(attackOptions.circumstance.damage);
  const atkMods = interpretDiceRollModifiers(attackOptions.circumstance.attack);

  let atkModRoll = 0;
  if (atkMods.dice.length > 0) {
    const atkRoll = await new Roll(atkMods.dice.join("+")).roll({ async: true });
    atkModRoll = atkRoll.total;
  }

  if ([ atkMods.flat, dmgMods.flat ].some(isNaN)) {
    Common.makeUIError("mythic.chat.error.nan");
    return;
  }

  const distanceFromTarget = parseFloat(attackOptions.distanceFromTarget);
  if (isNaN(distanceFromTarget) || distanceFromTarget < 0) {
    Common.makeUIWarning("mythic.chat.error.distanceFromTargetDefaulted");
  }

  const currentAmmo = weapon.system.currentAmmo;
  const rangeEffects =
    calculateRangeEffects(owner, weapon, distanceFromTarget);

  if (rangeEffects.error) {
    Common.makeUIError(rangeEffects.error);
    return;
  }

  const pRange =
    (isVehicle && actor.system.propulsion.type === "none" && !isManned)
      ? actor.system.characteristics.per * 20
      : owner.system.perceptiveRange.total * (
          isNaN(weapon.system.scopeMagnification)
            ? 1
            : weapon.system.scopeMagnification
        );

  const target = (
      weapon.system.ammoList[currentAmmo].target
    + atkMods.flat
    + atkModRoll
    + rangeEffects.target
    + calculatePerceptiveRangePenalties(weapon, pRange, distanceFromTarget)
  );

  if (isNaN(target)) {
    Common.makeUIError("mythic.chat.error.nan");
    return;
  }

  const data = {
    circDmg: dmgMods,
    rangeDamage: rangeEffects.damage,
    distanceFromTarget: distanceFromTarget,
    isZeroG: attackOptions.isZeroG,
    isVehicle: attackOptions.targetVehicle,
    target: target,
    type: element.value,
    pierce: rangeEffects.pierce
  };

  const wfm = isVehicle ? 0 : actor.system.characteristics.wfm.total;
  await getAttackAndDamageOutcomes(actor, str, wfm, weapon, data);
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
  if (options.cancelled) return;

  const mods = interpretDiceRollModifiers(options.circumstance);
  let mod = mods.flat;
  if (mods.dice.length > 0) {
    const roll = await new Roll(mods.dice.join("+")).roll({ async: true });
    mod += roll.total;
  }

  if (isNaN(mod) || isNaN(options.penalty) || isNaN(options.times)) {
    Common.makeUIError("mythic.chat.error.nan");
    return;
  }

  await rollEvasions(parseInt(element.value) + mod, options, actor);
}

/**
 * Roll a special Vehicle attack from a Vehicle Actor sheet.
 *
 * @async
 * @param {Actor} veh - The Actor Vehicle that fired the listener.
 * @param {string} atkType - The type of special Vehicle attack being made.
 */
export async function rollVehicleAttack(veh, atkType) {
  function getDetails() {
    switch(atkType) {
      case "doom":     return Vehicle.getDoomDetails(veh);
      case "splatter": return Vehicle.getWreckDetails(veh, atkType);
      case "trample":  return Vehicle.getTrampleDetails(veh);
      case "wreck":    return Vehicle.getWreckDetails(veh, atkType);
    }
  }

  const details = getDetails();
  const dmgRoll = await rollDamage(details.formula, 10);

  Chat.postChatMessage({
    flavor: Common.localize(`mythic.chat.vehicle.${atkType}`),
    template: "vehicle-attack",
    atkType: atkType,
    dmgRoll: dmgRoll.damageRoll,
    doesSpecialDamage: dmgRoll.doesSpecialDamage,
    evasionPenalty: details.evasionPenalty,
    pierce: details.pierce,
    specials: details.specials,
  }, veh);
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
  const localizationPath = `mythic.characteristics.${element.name}`;
  const test =
    type === "initiative"
      ? capitalize(type)
      : (CHARACTERISTICS[element.name] != undefined
          ? CHARACTERISTICS[element.name]
          : element.name
        );

  const testOptions = await getTestOptions(test);
  if (testOptions.cancelled) return;

  const mods = interpretDiceRollModifiers(testOptions.circumstance);
  let mod = mods.flat;
  if (mods.dice.length > 0) {
    const roll = await new Roll(mods.dice.join("+")).roll({ async: true });
    mod += roll.total;
  }

  if (type === "initiative") {
    await rollInitiative(element, mod, actor);
  } else {
    const target = parseInt(element.value);
    await rollBasicTest(target + mod, test, type, actor);
  }
}

function calculatePerceptiveRangePenalties(weapon, pRange, distanceFromTarget) {
  if (!game.settings.get("mythic", "rangeEffects")) return 0;
  if (pRange >= distanceFromTarget) return 0;
  return 10 * Math.floor((pRange - distanceFromTarget) / 50);
}

function calculateRangeEffects(actor, weapon, distanceFromTarget) {
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

async function getAttackAndDamageOutcomes(actor, str, wfm, weapon, data) {
  const currentAmmo = weapon.system.ammoList[weapon.system.currentAmmo];
  const fireMode = weapon.system.attack.fireMode.split("-")[0];
  let result = {
    actorId: actor.id,
    name: weapon.system.nickname || weapon.name,
    img: weapon.img,
    wfm: wfm,
    weaponData: weapon.system,
    attacks: [],
    hits: 0,
    type: data.type,
    target: data.target > 0 ? data.target : 0,
    template: "attack-chat",
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

  damagesPerAttack *=
    currentAmmo.special.linked.has ? currentAmmo.special.linked.value : 1;

  for (let i = 1; i <= attacks; i++) {
    const attackData = {
      target: data.target,
      attackNumber: i,
      damages: damagesPerAttack,
      distanceFromTarget: data.distanceFromTarget,
      isZeroG: data.isZeroG,
      vehicle: data.isVehicle,
      circDmg: data.circDmg,
      rangeDamage: data.rangeDamage,
      pierce: data.pierce
    };

    const attack = await rollAttackAndDamage(actor, str, weapon, attackData);
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
  ${Common.localize("mythic.chat.attack.title")}`;
  if (group === "ranged") {
    message += " - ";
    message += Common.localize(`mythic.weaponSheet.fireMode.${fireMode}`);
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
          label: Common.localize("mythic.chat.actions.roll"),
          callback: html => resolve(_processAttackOptions(html[0].querySelector("form")))
        },
        cancel: {
          label: Common.localize("mythic.chat.actions.cancel"),
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
          label: Common.localize("mythic.chat.actions.roll"),
          callback: html => resolve(_processEvadeOptions(html[0].querySelector("form")))
        },
        cancel: {
          label: Common.localize("mythic.chat.actions.cancel"),
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
  if (specialRules.electrified.has) {
    specials.electrified = specialRules.electrified.value;
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
          label: Common.localize("mythic.chat.actions.roll"),
          callback: html => resolve(_processTestOptions(html[0].querySelector("form")))
        },
        cancel: {
          label: Common.localize("mythic.chat.actions.cancel"),
          callback: html => resolve({cancelled: true})
        }
      },
      default: "roll",
      close: () => resolve({cancelled: true})
    };
    new Dialog(data, null).render(true);
  });
}

function renderChatRoll(roll, classes = []) {
  const inline = roll.toAnchor();
  inline.className = [ "inline-roll", "inline-result", ...classes ].join(" ");
  return inline.outerHTML;
}

function reverseDigits(roll) {
  let digits = String(roll).split("");
  if (digits.length === 1) {
    digits.push("0");
  } else digits.reverse();
  return parseInt(digits.join(""));
}

async function rollAttackAndDamage(actor, str, weapon, data) {
  const ammo = weapon.system.ammoList[weapon.system.currentAmmo];
  const roll = await new Roll(FORMULA).roll({ async: true });
  const outcome = determineRollOutcome(roll.total, data.target);
  const scatters = (
       weapon.system.attack.fireMode === "thrown"
    || ammo.special.blast.has
    || ammo.special.kill.has
  );

  let attack = {
    attackNumber: data.attackNumber,
    damages: data.damages,
    roll: renderChatRoll(roll),
    ...outcome
  };

  if (attack.outcome === "success" || scatters) {
    attack.location = await determineHitLocation(reverseDigits(roll.total), data.vehicle);
    const critType = game.settings.get("mythic", "criticalHitResult");

    let damage =
      [ `${ammo.diceQuantity}D${ammo.diceValue}`, data.rangeDamage ].map(pool => {
        if (!pool.toLowerCase().includes("d")) return pool;

        if (ammo.special.diceMinimum.has) {
          pool += `min${ammo.special.diceMinimum.value}`;
        }

        if (ammo.special.hardlight.has) {
          pool += `x>=${ammo.critsOn}`;
        } else if (critType !== "special") {
          pool += `${critType}>=${ammo.critsOn}`;
        }

        return pool;
      }).reduce((acc, formula) => {
        return acc === "" ? formula : `${acc}+${formula}`;
      });

    let base = ammo.baseDamage;
    let pierce = ammo.piercing;

    if (weapon.system.group === "melee") {
      base += Math.floor(str * ammo.strDamage);
      pierce += Math.floor(str * ammo.strPiercing);

      if (actor.system.trainings.weapons.unarmedCombatant) {
        const wfm =
          Common.getCharacteristicModifier(actor.system.characteristics.wfm.total);

        pierce += Math.floor(wfm / 2);
      }
    }

    const sizeBonus = weapon.system.group === "melee"
                    ? SIZE_DAMAGE_BONUS[actor.system.size] : 0;

    let formula = `${damage} + ${base} + ${sizeBonus} + ${data.circDmg.flat}`;
    data.circDmg.dice.forEach(die => formula += ` + ${die}`);

    const dmgResults = [];
    for (let i = 0; i < data.damages; i++) {
      dmgResults.push(await rollDamage(formula, ammo.critsOn));
    }

    attack = { ...attack, dmgResults };

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

    if (scatters) {
      attack.scatter = await scatterAttack(weapon, {
        distance: data.distanceFromTarget,
        degrees: outcome.degrees,
        isZeroG: data.isZeroG,
        vehicle: data.vehicle
      });
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
    roll: renderChatRoll(roll),
    target: target > 0 ? target : 0,
    critical: false,
    outcome: "",
    template: "test-chat",
    flavor: `${Common.localize("mythic.chat.test")}: ${test}`,
    ...outcome
  };
  await Chat.postChatMessage(result, actor);
}

async function rollDamage(formula, critsOn) {
  const roll = await new Roll(formula).roll({ async: true });
  return {
    damageRoll: renderChatRoll(roll),
    doesSpecialDamage: (
         (game.settings.get("mythic", "criticalHitResult") === "special")
      && roll.dice.some(die => die.results.some(res => res.result >= critsOn))
    )
  };
}

async function rollEvasions(baseTarget, options, actor) {
  const stat = actor.system.skills.evasion.characteristic;
  const i18n = "mythic.skillNames." + (stat === "AGI" ? "evasion" : "parry");

  let result = {
    evasions: [],
    flavor: `${Common.localize("mythic.chat.test")}: ${Common.localize(i18n)} (${stat})`,
    type: "test",
    template: "evade-chat"
  };

  for (let i = 0; i < options.times; i++) {
    const roll = await new Roll(FORMULA).roll({ async: true });
    const target = baseTarget - (i * options.penalty);
    const outcome = determineRollOutcome(roll.total, target);
    result.evasions.push({
      evasionNumber: i + 1,
      roll: renderChatRoll(roll),
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
      flavor: Common.localize("mythic.characterWeaponSummary.initiative")
    });
  }
}

async function scatterAttack(weapon, data) {
  const currentAmmo = weapon.system.currentAmmo;
  const thrownRange = weapon.system.ammoList[currentAmmo].range.thrown;
  const range = weapon.system.attack.fireMode === "thrown"
              ? thrownRange
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
    const distance = weapon.system.attack.fireMode === "thrown"
                   ? Math.min(Math.floor(dice.total / 2), thrownRange)
                   : dice.total;

    msg += `${Chat.getScatterArrow(direction.total)} ${distance} m`;
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
