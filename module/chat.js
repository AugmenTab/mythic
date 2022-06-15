/** @module chat */

import { calculateCharacteristicModifier } from "./calculations.js";
import { localize, makeUIError } from "./common.js";

/**
 * Add event listeners to chat messages.
 *
 * @param {jQuery.fn} html - The HTML hook.
 */
export function addChatListeners(html) {
  html.on("click", ".scatter", onScatter);
  html.on("click", ".special-rule-dmg", onSpecial);
}

/**
 * Build the content for a chat message.
 *
 * @async
 * @param {object} data - The data that will be passed to the Handlebars
 * template.
 * @returns {string} The HTML of the chat message to be posted.
 */
export async function buildChatMessageContent(data) {
  const template = `systems/mythic/templates/chat/${data.template}-chat.hbs`;
  return await renderTemplate(template, data);
}

/*
 * Provides the localization path for postable item flavor text.
 *
 * @param {object} item - The Item that is being rolled to chat.
 * @returns {string} The localization path.
 */
export function getPostableItemFlavorPath(item) {
  switch(item.type) {
    case "ability":
      return `mythic.characterTalents.abilities.type.${item.data.data.type}`;
    case "education":
      return `mythic.chat.education.flavor`;
    default: return "";
  }
}

/*
 * Post a message to chat.
 *
 * @async
 * @param {object} data - The data that will be passed to the Handlebars
 * template.
 * @param {object} actor - The Actor that is rolling the message to chat.
 */
export async function postChatMessage(data, actor) {
  const audioOptions = {
    src: "sounds/dice.wav",
    volume: 0.8,
    autoplay: true,
    loop: false
  };

  await AudioHelper.play(audioOptions, true);
  await ChatMessage.create({
    user: game.user.id,
    speaker: ChatMessage.getSpeaker({ actor: actor }),
    flavor: data.flavor,
    content: await buildChatMessageContent(data)
  }, {});
}

function getArrow(str) {
  const compass = {
    "N": "up",
    "E": "right",
    "S": "down",
    "W": "left"
  }
  return `<i class="fas fa-arrow-circle-${compass[str]}"></i>`;
}

function getScatterDirection(roll) {
  if (roll >= 10) return "NW";
  if (roll >= 9) return "W";
  if (roll >= 8) return "SW";
  if (roll >= 6) return "S";
  if (roll >= 5) return "SE";
  if (roll >= 4) return "E";
  if (roll >= 3) return "NE";
  if (roll >= 1) return "N";
}

async function getScatterOptions(degrees = 0) {
  const template = "systems/mythic/templates/chat/scatter-dialog.hbs";
  const html = await renderTemplate(template, {dof: degrees});
  return new Promise(resolve => {
    const data = {
      title: localize("mythic.chat.scatter.title"),
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

async function getSpecialOptions(rule) {
  const template = "systems/mythic/templates/chat/special-rule-dialog.hbs";
  const html = await renderTemplate(template, {rule: rule});
  const name = localize("mythic.weaponSheet.specialRules." + rule);
  return new Promise(resolve => {
    const data = {
      title: `${localize("mythic.chat.special.title")}: ${name}`,
      content: html,
      buttons: {
        roll: {
          label: localize("mythic.chat.actions.roll"),
          callback: html => resolve(_processSpecialOptions(html[0].querySelector("form")))
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

async function onScatter(event) {
  const element = event.currentTarget;
  const options = await getScatterOptions(element.dataset.dof);
  if (options.cancelled) return;
  if (isNaN(options.distance) || isNaN(options.dof)) {
    makeUIError("mythic.chat.error.nan");
    options.cancelled = true;
  }
  if (!options.cancelled) {
    let msg = "";
    let times = options.isZeroG ? 2 : 1;
    for (let i = 1; i <= times; i++) {
      const roll = await new Roll("1D10").roll({ async: true });
      if (i === 2) msg += " | ";
      let mod = 0;
      mod += Math.floor(options.dof);
      mod += Math.floor(options.distance / 100);
      if (options.distance > element.dataset.range) {
        if (options.distance > element.dataset.range * 2) { mod *= 5 }
        else { mod *= 2 }
      }
      mod -= calculateCharacteristicModifier(parseInt(element.dataset.wfm));
      const dice = await new Roll(`${mod > 1 ? mod : 1}D10`).roll({ async: true });
      msg += getScatterDirection(roll.total).split("").map(x => getArrow(x)).join("");
      msg += ` ${dice.total} m`;
    }
    await AudioHelper.play({src: "sounds/dice.wav", volume: 0.8, autoplay: true, loop: false}, true);
    element.classList.remove("scatter");
    element.innerHTML = msg;
  }
}

async function onSpecial(event) {
  const element = event.currentTarget;
  const options = element.dataset.rule !== "needle"
    ? await getSpecialOptions(element.dataset.rule) : { cancelled: false };
  if (options.cancelled) return;
  if (element.dataset.rule !== "needle" && isNaN(options.hits)) {
    makeUIError("mythic.chat.error.nan");
  } else {
    const formula = element.dataset.formula.toLowerCase().split("d");
    let newFormula = "";
    if (element.dataset.rule === "needle") {
      newFormula = `${formula[0]}D10`;
    } else if (["cryo", "flame"].includes(element.dataset.rule)) {
      newFormula += `${options.hits * parseInt(formula[0])}D`;
      newFormula += formula.length === 2 ? formula[1] : "1";
    }
    const roll = await new Roll(newFormula).roll({ async: true });

    const data = {
      hits: options.hits || 1,
      render: await roll.render(),
      rule: element.dataset.rule,
      template: "special-rule"
    };
    await postChatMessage(data, game.actors.get(element.dataset.actor_id));
  }
}

function _processSpecialOptions(form) {
  return { hits: parseInt(form.hits.value) };
}

function _processTestOptions(form) {
  return {
    dof: parseInt(form.dof.value),
    distance: parseInt(form.distance.value),
    isZeroG: form.isZeroG.checked
  };
}
