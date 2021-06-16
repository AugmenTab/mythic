import { mythic } from "./module/config.js";
import MythicItemSheet from "./module/sheets/MythicItemSheet.js";

Hooks.once("init", function() {
  console.log("mythic | Initializing Mythic 4.0 System");

  CONFIG.mythic = mythic;

  Items.unregisterSheet("core", ItemSheet);
  Items.registerSheet("mythic", MythicItemSheet, { makeDefault: true });
});