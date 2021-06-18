export default class MythicNamedCharacterSheet extends ActorSheet {
  static get defaultOptions() {
    return mergeObject(super.defaultOptions, {
      template: "systems/mythic/templates/sheets/namedCharacter-sheet.html",
      classes: ["mythic", "sheet", "namedCharacter"]
    });
  }
}