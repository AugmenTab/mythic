import { mythic } from "./module/config.js";
import MythicItemSheet from "./module/sheets/MythicItemSheet.js";
import MythicNamedCharacterSheet from "./module/sheets/MythicNamedCharacterSheet.js";

Hooks.once("init", function() {
  console.log("mythic | Initializing Mythic 4.0 System");

  CONFIG.mythic = mythic;

  Actors.unregisterSheet("core", ActorSheet);
  Actors.registerSheet("mythic", MythicNamedCharacterSheet, { makeDefault: true });

  Items.unregisterSheet("core", ItemSheet);
  Items.registerSheet("mythic", MythicItemSheet, { makeDefault: true });
});