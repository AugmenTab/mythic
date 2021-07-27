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

export async function rollAttacks(element, actor) {
  const weapon = await actor.items.get(element.getAttribute("data-item-id"));
  const attackOptions = await getAttackRollOptions();
  const target = weapon.data.data.attack.target + parseInt(attackOptions.circumstance);
  const type = element.value;
  if (!attackOptions.cancelled) {
    await getAttackAndDamageOutcomes(actor, weapon, target, type);
  }
}

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
  const mod = parseInt(testOptions.circumstance);
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
  if (roll >= THRESHOLD) {
    outcome.critical = true;
    outcome.outcome = "failure";
    outcome.color = "red";
  } else if (roll === 1) {
    outcome.critical = true;
    outcome.outcome = "success";
    outcome.color = "green";
  } else {
    const d = (target - roll) / 10;
    outcome.outcome = d >= 0 ? "success" : "failure";
    outcome.degrees = Math.abs(d).toFixed(1);
  }
  return outcome;
}

async function getAttackAndDamageOutcomes(actor, weapon, target, type) {
  let result = {
    name: weapon.name,
    img: weapon.img,
    weaponData: weapon.data.data,
    attacks: [],
    type: type,
    target: target,
    template: "attack",
    flavor: `${capitalize(weapon.data.data.group)} ${capitalize(type)} ${game.i18n.localize("mythic.chat.attack.title")}`
  };
  const fireMode = weapon.data.data.attack.fireMode.split("-")[0];
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
    target: target,
    roll: roll.total,
    ...outcome
  };

  if (attack.outcome === "success") {
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
    degrees: 0,
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