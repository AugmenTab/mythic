import { mythic } from "./module/config.js";
import MythicItemSheet from "./module/sheets/MythicItemSheet.js";
import MythicNamedCharacterSheet from "./module/sheets/MythicNamedCharacterSheet.js";

async function preloadHandlebarsTemplates() {
  const templatePaths = [
    "systems/mythic/templates/partials/character-details.hbs",
    "systems/mythic/templates/partials/character-nav.hbs",
    "systems/mythic/templates/partials/character-sheet-body.hbs",
    "systems/mythic/templates/partials/character-sheet-settings.hbs",
    "systems/mythic/templates/partials/character-sheet-skills.hbs",
    "systems/mythic/templates/partials/character-sheet-summary.hbs",
    "systems/mythic/templates/partials/character-stat-block.hbs",
    "systems/mythic/templates/partials/character-weapon-summary.hbs"
  ];
  return loadTemplates(templatePaths);
}

Hooks.once("init", function() {
  console.log("mythic | Initializing Mythic 4.0 System");

  CONFIG.mythic = mythic;

  Actors.unregisterSheet("core", ActorSheet);
  Actors.registerSheet("mythic", MythicNamedCharacterSheet, { makeDefault: true });

  Items.unregisterSheet("core", ItemSheet);
  Items.registerSheet("mythic", MythicItemSheet, { makeDefault: true });

  preloadHandlebarsTemplates();
});
