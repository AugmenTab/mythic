/** @module mythic */

import { mythic } from "./module/config.js";
import * as Chat from "./module/chat.js";
import * as Helpers from "./module/helpers.js";
import * as Migrations from "./module/migrations.js";
import MythicActor from "./module/MythicActor.js";
import MythicCombat from "./module/MythicCombat.js";
import MythicItem from "./module/MythicItem.js";
import MythicItemSheet from "./module/sheets/MythicItemSheet.js";
import MythicBestiaryCharacterSheet from "./module/sheets/MythicBestiaryCharacterSheet.js";
import MythicFloodCharacterSheet from "./module/sheets/MythicFloodCharacterSheet.js";
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
    "systems/mythic/templates/partials/editor.hbs",
    "systems/mythic/templates/partials/flood-details.hbs",
    "systems/mythic/templates/partials/flood-inventory.hbs",
    "systems/mythic/templates/partials/flood-items.hbs",
    "systems/mythic/templates/partials/flood-settings.hbs",
    "systems/mythic/templates/partials/flood-stat-block.hbs",
    "systems/mythic/templates/partials/weapon-sheet-info.hbs",
    "systems/mythic/templates/partials/weapon-sheet-settings.hbs"
  ];
  return loadTemplates(templatePaths);
}

/**
 * Establish all available system-specific settings for players and GMs.
 */
function registerSystemSettings() {
  // System Migration Version
  game.settings.register("mythic", "systemMigrationVersion", {
    config: false,
    scope: "world",
    type: String,
    default: "0.2.5"
  });

  // Critical Failure Threshold
  game.settings.register("mythic", "criticalFailureThreshold", {
    config: true,
    scope: "world",
    name: "SETTINGS.criticalFailureThreshold.name",
    hint: "SETTINGS.criticalFailureThreshold.label",
    type: Number,
    range: {
      min: 95,
      max: 100,
      step: 1
    },
    default: 100
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

  // Strong Back Version
  game.settings.register("mythic", "strongBackVersion", {
    config: true,
    scope: "world",
    name: "SETTINGS.strongBackVersion.name",
    hint: "SETTINGS.strongBackVersion.label",
    type: String,
    choices: {
      "v40": "Mythic 4.0",
      "v45": "Mythic 4.5+"
    },
    default: "v45"
  });

  // Flood Contamination Level
  game.settings.register("mythic", "contaminationLevel", {
    config: true,
    scope: "world",
    name: "SETTINGS.contaminationLevel.name",
    hint: "SETTINGS.contaminationLevel.hint",
    type: Number,
    range: {
      min: 0,
      max: 100,
      step: 1
    },
    default: 0
  });

  // Flood Swarm Version
  game.settings.register("mythic", "swarmVersion", {
    config: true,
    scope: "world",
    name: "SETTINGS.swarmVersion.name",
    hint: "SETTINGS.swarmVersion.hint",
    type: String,
    choices: {
      "contamination": "SETTINGS.swarmVersion.contamination",
      "difficulty": "SETTINGS.swarmVersion.difficulty",
      "manual": "SETTINGS.swarmVersion.manual"
    },
    default: "contamination"
  });

  // Flood Difficulty
  game.settings.register("mythic", "floodDifficulty", {
    config: true,
    scope: "world",
    name: "SETTINGS.floodDifficulty.name",
    hint: "SETTINGS.floodDifficulty.hint",
    type: Number,
    range: {
      min: 1,
      max: 5,
      step: 1
    },
    default: 1
  });

  // Magazine / Ammo Pool Management
  game.settings.register("mythic", "ammoTracking", {
    config: true,
    scope: "world",
    name: "SETTINGS.ammoTracking.name",
    hint: "SETTINGS.ammoTracking.hint",
    type: String,
    choices: {
      "magazines": "SETTINGS.ammoTracking.magazines",
      "ammoPool": "SETTINGS.ammoTracking.ammoPool",
      "selfManaged": "SETTINGS.ammoTracking.selfManaged"
    },
    default: "magazines"
  });
}

/** Hook to set up config, Actor and Item sheets, and load Handlebars templates. */
Hooks.once("init", function() {
  console.log("mythic | Initializing Mythic 5.0 System");

  CONFIG.mythic = mythic;
  CONFIG.Actor.documentClass = MythicActor;
  CONFIG.Combat.documentClass = MythicCombat;
  CONFIG.Item.documentClass = MythicItem;
  CONFIG.time.roundTime = 6;

  Actors.unregisterSheet("core", ActorSheet);
  Actors.registerSheet("mythic", MythicBestiaryCharacterSheet, { types: ["Bestiary Character"], makeDefault: true });
  Actors.registerSheet("mythic", MythicFloodCharacterSheet, { types: ["Flood"], makeDefault: true });
  Actors.registerSheet("mythic", MythicNamedCharacterSheet, { types: ["Named Character"], makeDefault: true });

  Items.unregisterSheet("core", ItemSheet);
  Items.registerSheet("mythic", MythicItemSheet, { makeDefault: true });

  preloadHandlebarsTemplates();

  registerSystemSettings();
});

/** Hook to perform data migration. */
Hooks.once("ready", function () {
  if (game.user.isGM) {
    const current = game.settings.get("mythic", "systemMigrationVersion");

    if (!current) {
      if (isNewerVersion("0.01",  current)) Migrations.migrateWorld();
      if (isNewerVersion("0.2.1", current)) Migrations.migrateV2_1();
      if (isNewerVersion("0.2.2", current)) Migrations.migrateCauterize();
    }
  }

  game.settings.set("mythic", "systemMigrationVersion", game.system.data.version);
});

/** Hook to establish event listeners on the chat log. */
Hooks.on("renderChatMessage", (app, html, data) => Chat.addChatListeners(html));
