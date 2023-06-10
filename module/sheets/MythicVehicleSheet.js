/** @module MythicVehicleSheet */

import * as Calc from "../calculations.js";
import { getPostableItemFlavorPath, postChatMessage } from "../chat.js";
import { localize, makeUIError } from "../common.js";
import { rollAttacks, rollVehicleAttack, rollTest } from "../dice.js";

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

    data.cargo =
      data.items.filter(item => [ "armor", "equipment" ].includes(item.type));

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
    html.find(".doom").click(this._onDoomDetonate.bind(this));
    html.find(".item-create").click(this._onItemCreate.bind(this));
    html.find(".item-delete").click(this._onItemDelete.bind(this));
    html.find(".item-edit").click(this._onItemEdit.bind(this));
    html.find(".item-edit-inline").change(this._onItemEditInline.bind(this));
    html.find(".postable-item").click(this._onPostItem.bind(this));
    html.find(".recharge").click(this._onShieldRecharge.bind(this));
    html.find(".reload").click(this._onReload.bind(this));
    html.find(".rollable").click(this._onRoll.bind(this));
    html.find(".speed-change").click(this._onSpeedChange.bind(this));
    html.find(".vehicle-attack").click(this._onVehicleAttack.bind(this));
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
        complement[element.dataset.index].id = element.value;

        return await this.actor.update({
          "system.crew.complement": Calc.setupCrew(complement)
        });

      case "gunner":
        let gunners = this.actor.system.crew.gunners;
        gunners[element.dataset.index].id = element.value;

        return await this.actor.update({
          "system.crew.gunners": Calc.setupCrew(gunners)
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

  async _onDoomDetonate(event) {
    event.preventDefault();
  }

  async _onItemCreate(event) {
    event.preventDefault();
    const element = event.currentTarget;
    const itemType = element.getAttribute("data-item-type");
    const itemSubtype = element.getAttribute("data-item-subtype");

    await Item.create({
      name: localize(`mythic.${itemType}Sheet.newItem`),
      type: itemType,
      system: Calc.generateBaseItemData(itemType, itemSubtype)
    }, { parent: this.actor });
  }

  async _onItemDelete(event) {
    event.preventDefault();
    const element = event.currentTarget;
    const item = await this.actor.items.get(element.getAttribute("data-item-id"));
    await item.delete();
  }

  async _onItemEdit(event) {
    event.preventDefault();
    const element = event.currentTarget;
    const item = await this.actor.items.get(element.getAttribute("data-item-id"));
    item.sheet.render(true);
  }

  async _onItemEditInline(event) {
    event.preventDefault();
    const element = event.currentTarget;
    const item = await this.actor.items.get(element.getAttribute("data-item-id"));
    const key = `system.${element.dataset.field}`;

    if (element.type === "checkbox") {
      await item.update({ [key]: element.checked })
    } else {
      const val = parseInt(element.value);
      await item.update({ [key]: isNaN(val) ? element.value : val });
    }
  }

  async _onPostItem(event) {
    event.preventDefault();
    const element = event.currentTarget;
    const item =
      await this.actor.items.get(element.getAttribute("data-item-id"));

    await postChatMessage({
      flavor: localize(getPostableItemFlavorPath(item)),
      template: `postable-${item.type}`,
      ...item
    }, this.actor);
  }

  async _onReload(event) {
    event.preventDefault();
    const element = event.currentTarget;
    const item = await this.actor.items.get(element.getAttribute("data-item-id"));
    await item.update({ "system": Calc.handleReloadMagCount(item.system) });
  }

  async _onRoll(event) {
    event.preventDefault();
    const element = event.currentTarget;
    if (element.classList[0] === "attack") {
      const item = await this.actor.items.get(element.getAttribute("data-item-id"));
      const newMag = await rollAttacks(element, this.actor, item);
      if (!isNaN(newMag)) {
        item.system.ammoList[item.system.currentAmmo].currentMag = newMag;
        await item.update({ "system": item.system });
      }
    } else {
      await rollTest(element, this.actor);
    }
  }

  async _onShieldRecharge(event) {
    event.preventDefault();
    const shields = this.actor.system.shields;
    const update = Math.min(shields.integrity.max, shields.integrity.current + shields.recharge);
    await this.actor.update({ "system.shields.integrity.current": update });
  }

  async _onSpeedChange(event) {
    event.preventDefault();
    const element = event.currentTarget;
    const operation = element.getAttribute("data-operation");
    const mvmt = this.actor.system.movement;

    let speed = mvmt.speed.current;

    if (operation === "inc") {
      speed = Math.min(mvmt.speed.max, speed + mvmt.accelerate.max);
    } else if (operation === "dec") {
      speed = Math.max(0, speed - mvmt.brake.max);
    }

    await this.actor.update({ "system.movement.speed.current": speed });
  }

  async _onVehicleAttack(event) {
    event.preventDefault();
    const element = event.currentTarget;
    const atkType = element.getAttribute("data-atk-type");
    await rollVehicleAttack(this.actor, atkType);
  }
}
