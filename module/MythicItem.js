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

    const itemData = this.system;
    const actorData = this.actor ? this.actor.data : {};

    if (this.type === "armor") this._prepareArmor(itemData);
    if (this.type === "weapon") this._prepareWeapon(itemData);
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
