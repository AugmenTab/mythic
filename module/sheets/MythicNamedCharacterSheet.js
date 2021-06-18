export default class MythicNamedCharacterSheet extends ActorSheet {
  static get defaultOptions() {
    return mergeObject(super.defaultOptions, {
      template: "systems/mythic/templates/sheets/namedCharacter-sheet.hbs",
      classes: ["mythic", "sheet", "namedCharacter"]
    });
  }
}