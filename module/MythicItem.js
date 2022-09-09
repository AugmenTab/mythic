/** @module MythicItem */

import * as Calc from "./calculations.js";

/**
 * Prepares Item data using callbacks.
 *
 * @extends Item
 */
export default class MythicItem extends Item {

  /**
   * Prepares ItemData.
   *
   * @override
   */
  prepareData() {
    super.prepareData();

    const itemData = this.data;
    const actorData = this.actor ? this.actor.data : {};
    const data = itemData.data;

    if (itemData.type === "armor") this._prepareArmor(data);
    if (itemData.type === "weapon") this._prepareWeapon(data);
  }

  /**
   * Prepares armor values.
   *
   * @param {ItemData} data - The prepared armor ItemData.
   */
  _prepareArmor(data) {
    Calc.calculateArmorValues(data)
  }

  /**
   * Prepares weapon values.
   *
   * @param {ItemData} data - The prepared weapon ItemData.
   */
  _prepareWeapon(data) {
    Calc.calculateWeaponValues(data);
  }
}
