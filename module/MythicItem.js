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

    if (this.type === "armor")     this._prepareArmor(this.system);
    if (this.type === "equipment") this._prepareEquipment(this.system);
    if (this.type === "weapon")    this._prepareWeapon(this.system);
  }

  /**
   * Prepares armor values.
   *
   * @param {ItemData} data - The armor ItemData to prepare.
   */
  _prepareArmor(data) {
    Calc.calculateArmorValues(data)
  }

  /**
   * Prepares equipment values.
   *
   * @param {ItemData} data - The equipment ItemData to prepare.
   */
  _prepareEquipment(data) {
    Calc.calculateEquipmentValues(data);
  }

  /**
   * Prepares weapon values.
   *
   * @param {ItemData} data - The weapon ItemData to prepare.
   */
  _prepareWeapon(data) {
    Calc.calculateWeaponValues(data);
  }
}
