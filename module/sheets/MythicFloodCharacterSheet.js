/** @module MythicFloodCharacterSheet */

import { handleReloadMagCount, sortAndFilterItems } from "../calculations.js";
import { getPostableItemFlavorPath } from "../chat.js";
import { localize, makeUIError } from "../common.js";
import { rollAttacks, rollEvasionBatch, rollTest } from "../dice.js";

/**
 * Class representing the unique features of this system's Flood Character
 * sheet.
 *
 * @extends ActorSheet
 */
export default class MythicFloodCharacterSheet extends ActorSheet {

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
      classes: ["mythic", "sheet", "flood", "character"],
      height: 745,
      width: 800,
      tabs: [
        {
          navSelector: '.tabs',
          contentSelector: '.sheet-body',
          initial: 'summary'
        },
        {
          navSelector:'.talent-tabs',
          contentSelector: '.talent-body',
          initial: 'skills'
        }
      ],
      template: "systems/mythic/templates/sheets/floodCharacter-sheet.hbs"
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

    const a = sortAndFilterItems(data.items, "ability");
    data.abilities = a.filter(i => i.type === "ability");
    data.augmentations = a.filter(i => i.type === "augmentation");
    data.racials = a.filter(i => i.type === "racial");
    data.traits = a.filter(i => i.type === "trait");

    data.armors = sortAndFilterItems(data.items, "armor");
    data.equipment = sortAndFilterItems(data.items, "equipment");
    data.shields = data.items.filter(i => i.type !== "armor" && i.system.shields.has);
    data.equippedShields = data.shields.filter(i => i.system.weight.equipped);
    data.weapons = sortAndFilterItems(data.items, "weapon", "nickname");
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

    html.find(".evade").click(this._onEvade.bind(this));
    html.find(".item-delete").click(this._onItemDelete.bind(this));
    html.find(".item-edit").click(this._onItemEdit.bind(this));
    html.find(".item-edit-inline").change(this._onItemEditInline.bind(this));
    html.find(".postable-details").click(this._onPostDetails.bind(this));
    html.find(".postable-item").click(this._onPostItem.bind(this));
    html.find(".recharge").click(this._onShieldRecharge.bind(this));
    html.find(".recharge-item").click(this._onShieldItemRecharge.bind(this));
    html.find(".recharge-all-items").click(this._onShieldItemRechargeAll.bind(this));
    html.find(".reload").click(this._onReload.bind(this));
    html.find(".rollable").click(this._onRoll.bind(this));
    html.find(".special-focus").focus(this._onItemEditInline.bind(this));
  }

  async _onEvade(event) {
    event.preventDefault();
    await rollEvasionBatch(event.currentTarget, this.actor);
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
      if (item.type === "armor" && element.dataset.field === "weight.equipped" && element.checked) {
        await this.actor.update({ "system.shields.value": 0 });
        let armors = this.actor.items.filter(a => a.type === "armor" && a.id !== item.id);
        for (let armor of armors) {
          await armor.update({
            "system.weight.equipped": false,
            "system.weight.selfSupported": false
          });
        }
      }
      await item.update({ [key]: element.checked })
    } else {
      const val = parseInt(element.value);
      await item.update({ [key]: isNaN(val) ? element.value : val });
    }
  }

  async _onPostDetails(event) {
    event.preventDefault();
    const element = event.currentTarget;
    const postable = element.dataset.roll;
    const template = `systems/mythic/templates/chat/postable-${postable}.hbs`;
    await ChatMessage.create({
      user: game.user.id,
      speaker: ChatMessage.getSpeaker({ actor: this.actor }),
      flavor: localize(`mythic.chat.${postable}.flavor`),
      content: await renderTemplate(template, this.actor)
    });
  }

  async _onPostItem(event) {
    event.preventDefault();
    const element = event.currentTarget;
    const item = await this.actor.items.get(element.getAttribute("data-item-id"));
    const template = `systems/mythic/templates/chat/postable-${item.type}.hbs`;
    await ChatMessage.create({
      user: game.user.id,
      speaker: ChatMessage.getSpeaker({ actor: this.actor }),
      flavor: localize(getPostableItemFlavorPath(item)),
      content: await renderTemplate(template, item)
    });
  }

  async _onReload(event) {
    event.preventDefault();
    const element = event.currentTarget;
    const item = await this.actor.items.get(element.getAttribute("data-item-id"));
    await item.update({ "system": handleReloadMagCount(item.system) });
  }

  async _onShieldItemRecharge(event) {
    event.preventDefault();
    const element = event.currentTarget;
    const item = await this.actor.items.get(element.getAttribute("data-item-id"));
    const shields = item.system.shields;
    const val = shields.integrity.current + shields.recharge.total;
    await item.update({
      "system.shields.integrity.current": Math.min(val, shields.integrity.total)
    });
  }

  async _onShieldItemRechargeAll(event) {
    event.preventDefault();
    await this.actor.items.filter(i => i.type !== "armor" && i.system.weight.equipped).forEach(async item => {
      const shields = item.system.shields;
      const val = shields.integrity.current + shields.recharge.total;
      await item.update({
        "system.shields.integrity.current": Math.min(val, shields.integrity.total)
      });
    });
  }

  async _onShieldRecharge(event) {
    event.preventDefault();
    let system = this.actor.system;
    const val = system.shields.value + system.shields.recharge;
    const update = val > system.shields.max ? system.shields.max : val;
    await this.actor.update({ "system.shields.value": update });
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
}
