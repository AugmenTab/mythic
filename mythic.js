/** @module mythic */

import * as Chat from "./module/chat.js";
import * as Helpers from "./module/helpers.js";
import { mythic } from "./module/config.js";
import { MythicActor } from "./module/MythicActor.js";
import MythicItemSheet from "./module/sheets/MythicItemSheet.js";
import MythicNamedCharacterSheet from "./module/sheets/MythicNamedCharacterSheet.js";

/**
 * Loads all registered Handlebars partials.
 * @async
 * @returns {Array.<Function>} An array of functions to establish partials.
 */
async function preloadHandlebarsTemplates() {
  const templatePaths = [
    "systems/mythic/templates/partials/armor-sheet-info.hbs",
    "systems/mythic/templates/partials/character-details.hbs",
    "systems/mythic/templates/partials/character-sheet-ability.hbs",
    "systems/mythic/templates/partials/character-sheet-abilities.hbs",
    "systems/mythic/templates/partials/character-sheet-educations.hbs",
    "systems/mythic/templates/partials/character-sheet-experience.hbs",
    "systems/mythic/templates/partials/character-sheet-inventory.hbs",
    "systems/mythic/templates/partials/character-sheet-items.hbs",
    "systems/mythic/templates/partials/character-sheet-settings.hbs",
    "systems/mythic/templates/partials/character-sheet-skills.hbs",
    "systems/mythic/templates/partials/character-sheet-summary.hbs",
    "systems/mythic/templates/partials/character-sheet-talents.hbs",
    "systems/mythic/templates/partials/character-sheet-trainings.hbs",
    "systems/mythic/templates/partials/character-stat-block.hbs",
    "systems/mythic/templates/partials/character-weapon-summary.hbs",
    "systems/mythic/templates/partials/editor.hbs"
  ];
  return loadTemplates(templatePaths);
}

/**
 * Establish all available system-specific settings for players and GMs.
 */
function registerSystemSettings() {
  // Critical Failure Threshold
  game.settings.register("mythic", "criticalFailureThreshold", {
    config: true,
    scope: "world",
    name: "SETTINGS.criticalFailureThreshold.name",
    hint: "SETTINGS.criticalFailureThreshold.label",
    type: Number,
    range: {
      min: 1,
      max: 100,
      step: 1
    },
    default: 98
  });

  // Critical Hit Result
  game.settings.register("mythic", "criticalHitResult", {
    config: true,
    scope: "world",
    name: "SETTINGS.criticalHitResult.name",
    hint: "SETTINGS.criticalHitResult.label",
    type: String,
    choices: {
      "special": "Special Damage",
      "x": "Explode",
      "xo": "Explode Once"
    },
    default: "special"
  });

  // Enduring Version
  game.settings.register("mythic", "enduringVersion", {
    config: true,
    scope: "world",
    name: "SETTINGS.enduringVersion.name",
    hint: "SETTINGS.enduringVersion.label",
    type: Number,
    range: {
      mix: 1,
      max: 2,
      step: 1
    },
    default: 1
  });
}

/** Hook to set up config, Actor and Item sheets, and load Handlebars templates. */
Hooks.once("init", function() {
  console.log("mythic | Initializing Mythic 4.0 System");

  CONFIG.mythic = mythic;
  CONFIG.Actor.documentClass = MythicActor;

  Actors.unregisterSheet("core", ActorSheet);
  Actors.registerSheet("mythic", MythicNamedCharacterSheet, { makeDefault: true });

  Items.unregisterSheet("core", ItemSheet);
  Items.registerSheet("mythic", MythicItemSheet, { makeDefault: true });

  preloadHandlebarsTemplates();

  registerSystemSettings();
});

/** Hook to establish event listeners on the chat log. */
Hooks.on("renderChatMessage", (app, html, data) => Chat.addChatListeners(html));
