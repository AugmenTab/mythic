/** @module mythic */

import { mythic } from "./module/config.js";
import * as Chat from "./module/chat.js";
import * as Helpers from "./module/helpers.js";
import MythicActor from "./module/MythicActor.js";
import MythicCombat from "./module/MythicCombat.js";
import MythicItem from "./module/MythicItem.js";
import MythicItemSheet from "./module/sheets/MythicItemSheet.js";
import MythicBestiaryCharacterSheet from "./module/sheets/MythicBestiaryCharacterSheet.js"
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
      "special": "SETTINGS.criticalHitResult.special",
      "x": "SETTINGS.criticalHitResult.explode",
      "xo": "SETTINGS.criticalHitResult.explodeOnce"
    },
    default: "special"
  });

  // Enduring Version (set config to true & default to 2 on release of 4.5)
  game.settings.register("mythic", "enduringVersion", {
    config: false,
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

  // Strong Back Version (set config to true & default to v45 on release of 4.5)
  game.settings.register("mythic", "strongBackVersion", {
    config: false,
    scope: "world",
    name: "SETTINGS.strongBackVersion.name",
    hint: "SETTINGS.strongBackVersion.label",
    type: String,
    choices: {
      "v40": "Mythic 4.0",
      "v45": "Mythic 4.5"
    },
    default: "v40"
  });
}

/** Hook to set up config, Actor and Item sheets, and load Handlebars templates. */
Hooks.once("init", function() {
  console.log("mythic | Initializing Mythic 4.0 System");

  CONFIG.mythic = mythic;
  CONFIG.Actor.documentClass = MythicActor;
  CONFIG.Combat.documentClass = MythicCombat;
  CONFIG.Item.documentClass = MythicItem;
  CONFIG.time.roundTime = 6;

  Actors.unregisterSheet("core", ActorSheet);
  Actors.registerSheet("mythic", MythicNamedCharacterSheet, { types: ["Named Character"], makeDefault: true });
  Actors.registerSheet("mythic", MythicBestiaryCharacterSheet, { types: ["Bestiary Character"], makeDefault: true });

  Items.unregisterSheet("core", ItemSheet);
  Items.registerSheet("mythic", MythicItemSheet, { makeDefault: true });

  preloadHandlebarsTemplates();

  registerSystemSettings();
});

/** Hook to establish event listeners on the chat log. */
Hooks.on("renderChatMessage", (app, html, data) => Chat.addChatListeners(html));
