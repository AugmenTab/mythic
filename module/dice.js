/** @module dice */

import { calculateCharacteristicModifier } from "./calculations.js";
import { determineHitLocation } from "./location.js";

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
const FORMULA = "D100";

/**
 * Evaluates a string of simple addition and subtraction expressions.
 * @param {string} str - The string representation of the expression.
 * @returns {number} The number value of the evaluated expression.
 */
export function interpretDiceRollModifiers(str) {
  let re = /\s*[+\-]?\s*\d+\s*/g;
  let mods = str.trim().match(re);
  let num = 0;
  for (let mod of mods) num += parseInt(mod);
  if (isNaN(num)) {
    const msg = game.i18n.localize("mythic.chat.error.parseFailure");
    ui.notifications.error(msg);
    throw new Error(msg);
  } else return num;
}

/**
 * Roll attacks from an Actor sheet.
 * @param {Element} element - The HTML element the listener originated from.
 * @param {Actor} actor - The Actor that fired the listener.
 */
export async function rollAttacks(element, actor, weapon) {
  const attackOptions = await getAttackRollOptions();
  let mod = 0;
  if (!attackOptions.cancelled) mod = interpretDiceRollModifiers(attackOptions.circumstance);
  if (isNaN(mod)) {
    ui.notifications.error(game.i18n.localize("mythic.chat.error.nan"));
    attackOptions.cancelled = true;
  }
  if (!attackOptions.cancelled) {
    const target = weapon.data.data.attack.target + mod;
    const type = element.value;
    const isVehicle = attackOptions.targetVehicle;
    await getAttackAndDamageOutcomes(actor, weapon, target, type, isVehicle);
    return weapon.data.data.magazine.current - parseInt(element.innerHTML);
  }
  return;
}

export async function rollEvasionBatch(element, actor) {
  const options = await getEvadeOptions();
  let mod = 0;
  if (options.cancelled) {
    return;
  } else mod = interpretDiceRollModifiers(options.circumstance);
  if (isNaN(mod) || isNaN(options.penalty) || isNaN(options.times)) {
    ui.notifications.error(game.i18n.localize("mythic.chat.error.nan"));
    options.cancelled = true;
  }
  if (!options.cancelled) await rollEvasions(parseInt(element.value) + mod, options, actor);
}

/**
 * Roll tests from an Actor sheet.
 * @param {Element} element - The HTML element the listener originated from.
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
    ui.notifications.error(game.i18n.localize("mythic.chat.error.nan"));
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

async function buildChatMessageContent(data) {
  const template = `systems/mythic/templates/chat/${data.template}-chat.hbs`;
  return await renderTemplate(template, data);
}

function capitalize(str) {
  return str[0].toUpperCase() + str.slice(1);
}

function determineRollOutcome(roll, target) {
  let outcome = { color: "black", critical: false };
  const d = ((target > 0 ? target : 0) - roll) / 10;
  outcome.degrees = Math.abs(d).toFixed(1);
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

async function getAttackAndDamageOutcomes(actor, weapon, target, type, vehicle) {
  const fireMode = weapon.data.data.attack.fireMode.split("-")[0];
  let result = {
    name: weapon.data.data.nickname,
    img: weapon.img,
    wfm: actor.data.data.characteristics.wfm.total,
    weaponData: weapon.data.data,
    attacks: [],
    type: type,
    target: target > 0 ? target : 0,
    template: "attack",
    flavor: getAttackFlavor(weapon.data.data.group, type, fireMode)
  };
  let attacks = 1, damagesPerAttack = 1;
  if (type !== "single" && fireMode === "sustained") {
    damagesPerAttack = weapon.data.data.attack[type];
  } else if (type !== "single" && fireMode === "burst") {
    attacks = type === "full" ? 2 : 1;
    damagesPerAttack = weapon.data.data.attack.half;
  } else if (type !== "single") {
    attacks = weapon.data.data.attack[type];
  }
  for (let i = 1; i <= attacks; i++) {
    const attack = await rollAttackAndDamage(actor, weapon, target, i, damagesPerAttack, vehicle);
    result.attacks.push(attack);
  }
  await postChatMessage(result, actor);
}

function getAttackFlavor(group, type, fireMode) {
  let message = `${capitalize(group)} ${capitalize(type)}
  ${game.i18n.localize("mythic.chat.attack.title")}`;
  if (group === "ranged") {
    message += " - ";
    message += game.i18n.localize(`mythic.weaponSheet.fireMode.${fireMode}`);
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
          label: game.i18n.localize("mythic.chat.actions.roll"),
          callback: html => resolve(_processAttackOptions(html[0].querySelector("form")))
        },
        cancel: {
          label: game.i18n.localize("mythic.chat.actions.cancel"),
          callback: html => resolve({cancelled: true})
        }
      },
      default: "roll",
      close: () => resolve({cancelled: true})
    };
    new Dialog(data, null).render(true);
  });
}

async function getEvadeOptions() {
  const template = "systems/mythic/templates/chat/evade-dialog.hbs";
  const html = await renderTemplate(template, {});
  return new Promise(resolve => {
    const data = {
      title: game.i18n.format("mythic.chat.evade"),
      content: html,
      buttons: {
        roll: {
          label: game.i18n.localize("mythic.chat.actions.roll"),
          callback: html => resolve(_processEvadeOptions(html[0].querySelector("form")))
        },
        cancel: {
          label: game.i18n.localize("mythic.chat.actions.cancel"),
          callback: html => resolve({cancelled: true})
        }
      },
      default: "roll",
      close: () => resolve({cancelled: true})
    };
    new Dialog(data, null).render(true);
  });
}

async function getTestOptions(test) {
  const template = "systems/mythic/templates/chat/test-dialog.hbs";
  const html = await renderTemplate(template, {});
  return new Promise(resolve => {
    const data = {
      title: `${CHARACTERISTICS[test] != undefined 
        ? CHARACTERISTICS[test] 
        : test} ${game.i18n.format("mythic.chat.test.title")}`,
      content: html,
      buttons: {
        roll: {
          label: game.i18n.localize("mythic.chat.actions.roll"),
          callback: html => resolve(_processTestOptions(html[0].querySelector("form")))
        },
        cancel: {
          label: game.i18n.localize("mythic.chat.actions.cancel"),
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

async function rollAttackAndDamage(actor, weapon, target, attackNumber, damages, veh) {
  const roll = await new Roll(FORMULA).roll({ async: true });
  const outcome = determineRollOutcome(roll.total, target);
  let attack = {
    attackNumber: attackNumber,
    damages: damages,
    roll: roll.total,
    ...outcome
  };
  if (attack.outcome === "success"
    || weapon.data.data.special.blast.has 
    || weapon.data.data.special.kill.has
  ) {
    attack.location = await determineHitLocation(reverseDigits(roll.total), veh);
    let damage = weapon.data.data.attack.damageRoll;
    let min = weapon.data.data.special.diceMinimum.has
      ? weapon.data.data.special.diceMinimum.value
      : 0;
    if (roll.total === 1 && min < 5) min = 5;
    if (min > 0) damage += `min${min}`;
    const critType = game.settings.get("mythic", "criticalHitResult");
    if (critType !== "special") {
      const explode = weapon.data.data.special.cauterize.has ? weapon.data.data.special.cauterize.value : 10;
      damage += `${critType}>=${explode}`;
    }

    let base = weapon.data.data.attack.baseDamage;
    let pierce = weapon.data.data.attack.piercing;
    if (weapon.data.data.group === "melee") {
      const str = (
        actor.data.data.mythicCharacteristics.str.total +
        calculateCharacteristicModifier(actor.data.data.characteristics.str.total)
      );
      base += Math.floor(str * weapon.data.data.attack.strDamage);
      pierce += Math.floor(str * weapon.data.data.attack.strPiercing);
      if (actor.data.data.trainings.weapons.unarmedCombatant) {
        const wfm = calculateCharacteristicModifier(actor.data.data.characteristics.wfm.total);
        pierce += Math.floor(wfm / 2);
      }
    }
    attack.damageRoll = `${damage} + ${base}`;
    attack.piercing = pierce;
    if (weapon.data.data.special.blast.has || weapon.data.data.special.kill.has) {
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
    flavor: `${test} ${game.i18n.localize("mythic.chat.test.title")}`,
    ...outcome
  };
  await postChatMessage(result, actor);
}

async function rollEvasions(baseTarget, options, actor) {
  let result = {
    evasions: [],
    flavor: `${game.i18n.localize("mythic.skillNames.evasion")} (AGI) ${game.i18n.localize("mythic.chat.test.title")}`,
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
  await postChatMessage(result, actor);
}

async function rollInitiative(element, mod, actor) {
  const dataset = element.dataset;
  if (dataset.roll) {
    const circumstance = `${mod > 0 ? " + " + mod : mod}`;
    const roll = await new Roll(dataset.roll + circumstance, actor.data.data, { async: true });
    const result = await roll.roll({ async: true });
    result.toMessage({
      speaker: ChatMessage.getSpeaker({ actor: actor }),
      flavor: game.i18n.localize("mythic.characterWeaponSummary.initiative")
    });
  }
}

async function postChatMessage(data, actor) {
  await AudioHelper.play({src: "sounds/dice.wav", volume: 0.8, autoplay: true, loop: false}, true);
  await ChatMessage.create({
    user: game.user.id,
    speaker: ChatMessage.getSpeaker({ actor: actor }),
    flavor: data.flavor,
    content: await buildChatMessageContent(data)
  }, {});
}

function _processAttackOptions(form) {
  return {
    circumstance: form.circumstance.value,
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