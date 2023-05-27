/** @module MythicVehicleSheet */

/**
 * Class representing the unique features of this system's Vehicle sheet.
 *
 * @extends ActorSheet
 */
export default class MythicVehicleSheet extends ActorSheet {

  /**
   * Establish default size and class options for the ActorSheet, establish tab
   * navigation on the sheet, and define the path to the Handlebars template.
   *
   * @override
   * @returns {object} The original source object including updated, inserted,
   * or overwritten records.
   */
  static get defaultOptions() {
    return mergeObject(super.defaultOptions, {
      classes: ["mythic", "sheet", "vehicle"],
      height: 500,
      width: 500,
      tabs: [
        {
          navSelector: '.tabs',
          contentSelector: '.sheet-body',
          initial: 'summary'
        },
      ],
      template: "systems/mythic/templates/sheets/vehicle-sheet.hbs"
    });
  }

  /**
   * Prepares the ActorData.
   *
   * @override
   * @returns {ActorData} The prepared ActorData.
   */
  getData() {
    const data = super.getData();

    data.system = data.actor.system;
    data.config = CONFIG.mythic;

    return data;
  }

  /**
   * Prepares the RollData so attributes can be referenced in the game.
   *
   * @override
   * @returns {RollData} The prepared RollData.
   */
  getRollData() {
    const data = super.getRollData();
    return data;
  }
}
