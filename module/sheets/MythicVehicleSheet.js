/** @module MythicVehicleSheet */

import * as Calc from "../calculations.js";
import { makeUIError } from "../common.js";

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
      classes: ["mythic", "sheet", "vehicle", "character"],
      height: 765,
      width: 820,
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

    data.weapons = Calc.sortAndFilterItems(data.items, "weapon", "nickname");
    data.equippedWeapons = data.weapons.filter(w => w.system.weight.equipped);

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

  /**
   * Establishes event listeners on the ActorSheet.
   *
   * @override
   * @param {jQuery.fn} html - The HTML hook.
   */
  activateListeners(html) {
    super.activateListeners(html);

    html.find(".crew-create").click(this._onCrewCreate.bind(this));
    html.find(".crew-delete").click(this._onCrewDelete.bind(this));
    html.find(".crew-edit").change(this._onCrewEdit.bind(this));
  }

  async _onCrewCreate(event) {
    event.preventDefault;
    const element = event.currentTarget;

    switch(element.dataset.role) {
      case "complement":
        let complement = Calc.setupCrew(this.actor.system.crew.complement);
        complement.push({
          idx: complement.length,
          id: null,
          display: null
        });
        return await this.actor.update({ "system.crew.complement": complement });

      case "gunner":
        let gunners = Calc.setupCrew(this.actor.system.crew.gunners);
        gunners.push({
          idx: gunners.length,
          id: null,
          display: null
        });
        return await this.actor.update({ "system.crew.gunners": gunners });

      case "operator":
        let operators = Calc.setupCrew(this.actor.system.crew.operators);
        operators.push({
          idx: operators.length,
          id: null,
          display: null
        });
        return await this.actor.update({ "system.crew.operators": operators });

      default:
        // This should be unreachable since these are hard-coded classes, but
        // it's weird enough if it does happen that it should produce an error
        // for the user to see.
        makeUIError("mythic.chat.error.unknownCrewRole");
    }
  }

  async _onCrewDelete(event) {
    event.preventDefault;
    const element = event.currentTarget;
    const idx = element.dataset.index;

    switch(element.dataset.role) {
      case "complement":
        let complement =
          this.actor.system.crew.complement.filter(x => x.idx != idx);

        return await this.actor.update({
          "system.crew.complement": Calc.setupCrew(complement)
        });

      case "gunner":
        let gunners = this.actor.system.crew.gunners.filter(x => x.idx != idx);
        return await this.actor.update({
          "system.crew.gunners": Calc.setupCrew(gunners)
        });

      case "operator":
        let operators =
          this.actor.system.crew.operators.filter(x => x.idx != idx);

        return await this.actor.update({
          "system.crew.operators": Calc.setupCrew(operators)
        });

      default:
        // This should be unreachable since these are hard-coded classes, but
        // it's weird enough if it does happen that it should produce an error
        // for the user to see.
        makeUIError("mythic.chat.error.unknownCrewRole");
    }
  }

  async _onCrewEdit(event) {
    event.preventDefault;
    const element = event.currentTarget;

    switch(element.dataset.role) {
      case "complement":
        let complement = this.actor.system.crew.complement;
        complement[element.dataset.idx].id = element.value;

        return await this.actor.update({
          "system.crew.complement": Calc.setupCrew(complement)
        });

      case "gunner":
        let gunners = this.actor.system.crew.gunners;
        gunners[element.dataset.idx].id = element.value;

        return await this.actor.update({
          "system.crew.gunners": Calc.setupCrew(operators)
        });

      case "operator":
        let operators = this.actor.system.crew.operators;
        operators[element.dataset.index].id = element.value;

        return await this.actor.update({
          "system.crew.operators": Calc.setupCrew(operators)
        });

      default:
        // This should be unreachable since these are hard-coded classes, but
        // it's weird enough if it does happen that it should produce an error
        // for the user to see.
        makeUIError("mythic.chat.error.unknownCrewRole");
    }
  }
}
