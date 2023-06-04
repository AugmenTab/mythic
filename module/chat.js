/** @module chat */

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
  const template = `systems/mythic/templates/chat/${data.template}.hbs`;
  return await renderTemplate(template, data);
}

/**
 * Builds the i tag holding the scatter direction arrow.
 *
 * @param {number} roll - The roll result for the scatter direction.
 * @returns {string} The HTML i tag with the rotated scatter arrow.
 */
export function getScatterArrow(roll) {
  const compass = {
    10: 315,
     9: 270,
     8: 225,
     7: 180,
     6: 180,
     5: 135,
     4: 90,
     3: 45,
     2: 0,
     1: 0
  }
  const rotation = `transform: rotate(${compass[roll]}deg)`;
  return `<i class="fas fa-arrow-circle-up" style="${rotation}"></i>`;
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
      return `mythic.characterTalents.abilities.type.${item.system.type}`;
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
  const mode = game.settings.get("core", "rollMode");
  const audioOptions = {
    src: "sounds/dice.wav",
    volume: 0.8,
    autoplay: true,
    loop: false
  };

  const chatOptions = {
    user: game.user.id,
    speaker: ChatMessage.getSpeaker({ actor: actor }),
    flavor: data.flavor,
    content: await buildChatMessageContent(data)
  };

  await AudioHelper.play(audioOptions, true);
  await ChatMessage.create(ChatMessage.applyRollMode(chatOptions, mode), {});
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
  await AudioHelper.play({
    src: "sounds/dice.wav",
    volume: 0.8,
    autoplay: true,
    loop: false
  }, true);

  const element = event.currentTarget;
  element.classList.remove("scatter");
  element.innerHTML = element.dataset.scatter;
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
      template: "special-rule-chat"
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
