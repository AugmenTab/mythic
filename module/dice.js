/** @module dice */

import { calculateCharacteristicModifier } from "./calculations.js";

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
const THRESHOLD = 98;

/**
 * Roll attacks from an Actor sheet.
 * @param {Element} element - The HTML element the listener originated from.
 * @param {Actor} actor - The Actor that fired the listener.
 */
export async function rollAttacks(element, actor) {
  const attackOptions = await getAttackRollOptions();
  let mod = 0;
  try {
    mod = eval(attackOptions.circumstance);
    if (isNaN(mod)) {
      ui.notifications.error(game.i18n.localize("mythic.chat.error.nan"));
      attackOptions.cancelled = true;
    }
  } catch {
    ui.notifications.error(game.i18n.localize("mythic.chat.error.nan"));
    attackOptions.cancelled = true;
  }
  if (!attackOptions.cancelled) {
    const weapon = await actor.items.get(element.getAttribute("data-item-id"));
    const target = weapon.data.data.attack.target + parseInt(attackOptions.circumstance);
    const type = element.value;
    await getAttackAndDamageOutcomes(actor, weapon, target, type);
  }
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
  try {
    mod = eval((testOptions.circumstance));
    if (isNaN(mod)) {
      ui.notifications.error(game.i18n.localize("mythic.chat.error.nan"));
      testOptions.cancelled = true;
    }
  } catch {
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

async function determineHitDigit(root) {
  const roll = await new Roll("1D5").roll({ async: true });
  if (roll.total === 5) return game.i18n.localize(`${root}.digits.thumb`);
  if (roll.total === 4) return game.i18n.localize(`${root}.digits.index`);
  if (roll.total === 3) return game.i18n.localize(`${root}.digits.middle`);
  if (roll.total === 2) return game.i18n.localize(`${root}.digits.ring`);
  if (roll.total === 1) return game.i18n.localize(`${root}.digits.pinky`);
}

async function determineHitLocation(roll) {
  const key = parseInt(String(roll).split("").reverse().join(""));
  const root = "mythic.hitLocations.body";
  let location = "";
  if (key >= 61) {
    location = game.i18n.localize(`${root}.chest.location`);
  } else if (key >= 46) {
    location = game.i18n.localize(`${root}.leg.right`);
  } else if (key >= 31) {
    location = game.i18n.localize(`${root}.leg.left`);
  } else if (key >= 21) {
    location = game.i18n.localize(`${root}.arm.right`);
  } else if (key >= 11) {
    location = game.i18n.localize(`${root}.arm.left`);
  } else {
    location = game.i18n.localize(`${root}.head.location`);
  }
  const sublocation = await determineHitSublocation(key, root);
  return `${location} - ${sublocation}`;
}

async function determineHitSide() {
  const roll = await new Roll("1D2").roll({ async: true });
  const root = "mythic.hitLocations.side";
  return game.i18n.localize(`${root}.${roll.total === 2 ? "right" : "left"}`);
}

async function determineHitSublocation(key, root) {
  // Chest
  if (key >= 97) return game.i18n.localize(`${root}.chest.noOrgan`);
  if (key >= 90) {
    const side = await determineHitSide();
    return `${side} ${game.i18n.localize(`${root}.chest.lung`)}`;
  }
  if (key >= 85) return game.i18n.localize(`${root}.chest.heart`);
  if (key >= 79) return game.i18n.localize(`${root}.chest.splanchnic`);
  if (key >= 73) {
    const side = await determineHitSide();
    return `${side} ${game.i18n.localize(`${root}.chest.kidney`)}`;
  }
  if (key >= 67) return game.i18n.localize(`${root}.chest.small`);
  if (key >= 61) return game.i18n.localize(`${root}.chest.large`);

  // Right Leg
  if (key >= 59) return game.i18n.localize(`${root}.leg.pelvis`);
  if (key >= 55) return game.i18n.localize(`${root}.leg.thigh`);
  if (key >= 54) return game.i18n.localize(`${root}.leg.knee`);
  if (key >= 49) return game.i18n.localize(`${root}.leg.shin`);
  if (key >= 48) return game.i18n.localize(`${root}.leg.ankle`);
  if (key >= 47) return game.i18n.localize(`${root}.leg.foot`);
  if (key >= 46) {
    const digit = await determineHitDigit(root);
    return `${digit} ${game.i18n.localize(`${root}.leg.toe`)}`;
  }

  // Left Leg
  if (key >= 44) return game.i18n.localize(`${root}.leg.pelvis`);
  if (key >= 39) return game.i18n.localize(`${root}.leg.thigh`);
  if (key >= 38) return game.i18n.localize(`${root}.leg.knee`);
  if (key >= 34) return game.i18n.localize(`${root}.leg.shin`);
  if (key >= 33) return game.i18n.localize(`${root}.leg.ankle`);
  if (key >= 32) return game.i18n.localize(`${root}.leg.foot`);
  if (key >= 31) {
    const digit = await determineHitDigit(root);
    return `${digit} ${game.i18n.localize(`${root}.leg.toe`)}`;
  }

  // Right Arm
  if (key >= 30) return game.i18n.localize(`${root}.arm.shoulder`);
  if (key >= 27) return game.i18n.localize(`${root}.arm.bicep`);
  if (key >= 26) return game.i18n.localize(`${root}.arm.elbow`);
  if (key >= 23) return game.i18n.localize(`${root}.arm.forearm`);
  if (key >= 22) return game.i18n.localize(`${root}.arm.hand`);
  if (key >= 21) {
    const digit = await determineHitDigit(root); 
    return `${digit} ${game.i18n.localize(`${root}.arm.finger`)}`;
  }

  // Left Arm
  if (key >= 20) return game.i18n.localize(`${root}.arm.shoulder`);
  if (key >= 17) return game.i18n.localize(`${root}.arm.bicep`);
  if (key >= 16) return game.i18n.localize(`${root}.arm.elbow`);
  if (key >= 13) return game.i18n.localize(`${root}.arm.forearm`);
  if (key >= 12) return game.i18n.localize(`${root}.arm.hand`);
  if (key >= 11) {
    const digit = await determineHitDigit(root)
    return `${digit} ${game.i18n.localize(`${root}.arm.finger`)}`;
  }

  // Head
  if (key >= 10) {
    const side = await determineHitSide();
    return `${side} ${game.i18n.localize(`${root}.head.ear`)}`;
  }
  if (key >= 9) return game.i18n.localize(`${root}.head.forehead`);
  if (key >= 8) {
    const side = await determineHitSide();
    return `${side} ${game.i18n.localize(`${root}.head.eye`)}`;
  }
  if (key >= 6) {
    const side = await determineHitSide();
    return `${side} ${game.i18n.localize(`${root}.head.cheek`)}`;
  }
  if (key >= 4) return game.i18n.localize(`${root}.head.nose`);
  if (key >= 3) return game.i18n.localize(`${root}.head.mouth`);
  if (key >= 2) return game.i18n.localize(`${root}.head.chin`);
  if (key >= 1) return game.i18n.localize(`${root}.head.neck`);
}

function determineRollOutcome(roll, target) {
  let outcome = { color: "black", critical: false };
  const d = (target - roll) / 10;
  outcome.degrees = Math.abs(d).toFixed(1);
  if (roll >= THRESHOLD) {
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

async function getAttackAndDamageOutcomes(actor, weapon, target, type) {
  const fireMode = weapon.data.data.attack.fireMode.split("-")[0];
  let result = {
    name: weapon.data.data.nickname,
    img: weapon.img,
    wfm: actor.data.data.characteristics.wfm.total,
    weaponData: weapon.data.data,
    attacks: [],
    type: type,
    target: target,
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
    const attack = await rollAttackAndDamage(actor, weapon, target, i, damagesPerAttack);
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

async function rollAttackAndDamage(actor, weapon, target, attackNumber, damages) {
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
    attack.location = await determineHitLocation(roll.total);
    let damage = weapon.data.data.attack.damageRoll;
    let min = weapon.data.data.special.diceMinimum.has
      ? weapon.data.data.special.diceMinimum.value
      : 0;
    if (roll.total === 1 && min < 5) min = 5;
    if (min > 0) damage += `min${min}`;

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
    target: target,
    critical: false,
    outcome: "",
    template: "test",
    flavor: `${test} ${game.i18n.localize("mythic.chat.test.title")}`,
    ...outcome
  };
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
    circumstance: form.circumstance.value
  };
}

function _processTestOptions(form) {
  return {
    circumstance: form.circumstance.value
  };
}