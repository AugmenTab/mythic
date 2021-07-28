import * as Chat from "./module/chat.js";
import * as Helpers from "./module/helpers.js";
import { mythic } from "./module/config.js";
import { MythicActor } from "./module/MythicActor.js";
import MythicItemSheet from "./module/sheets/MythicItemSheet.js";
import MythicNamedCharacterSheet from "./module/sheets/MythicNamedCharacterSheet.js";

async function preloadHandlebarsTemplates() {
  const templatePaths = [
    "systems/mythic/templates/partials/character-details.hbs",
    "systems/mythic/templates/partials/character-nav.hbs",
    "systems/mythic/templates/partials/character-sheet-ability.hbs",
    "systems/mythic/templates/partials/character-sheet-abilities.hbs",
    "systems/mythic/templates/partials/character-sheet-body.hbs",
    "systems/mythic/templates/partials/character-sheet-editor.hbs",
    "systems/mythic/templates/partials/character-sheet-educations.hbs",
    "systems/mythic/templates/partials/character-sheet-experience.hbs",
    "systems/mythic/templates/partials/character-sheet-inventory.hbs",
    "systems/mythic/templates/partials/character-sheet-settings.hbs",
    "systems/mythic/templates/partials/character-sheet-skills.hbs",
    "systems/mythic/templates/partials/character-sheet-summary.hbs",
    "systems/mythic/templates/partials/character-sheet-talents.hbs",
    "systems/mythic/templates/partials/character-sheet-trainings.hbs",
    "systems/mythic/templates/partials/character-stat-block.hbs",
    "systems/mythic/templates/partials/character-weapon-summary.hbs"
  ];
  return loadTemplates(templatePaths);
}

Hooks.once("init", function() {
  console.log("mythic | Initializing Mythic 4.0 System");

  CONFIG.mythic = mythic;
  CONFIG.Actor.documentClass = MythicActor;

  Actors.unregisterSheet("core", ActorSheet);
  Actors.registerSheet("mythic", MythicNamedCharacterSheet, { makeDefault: true });

  Items.unregisterSheet("core", ItemSheet);
  Items.registerSheet("mythic", MythicItemSheet, { makeDefault: true });

  preloadHandlebarsTemplates();
});

Hooks.on("renderChatMessage", (app, html, data) => Chat.addChatListeners(html));
