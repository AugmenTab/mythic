/** @module MythicActor */

import * as Calc from "./calculations.js";

/**
 * Prepares Actor data using callbacks.
 *
 * @extends Actor
 */
export default class MythicActor extends Actor {

  /**
   * Prepares ActorData.
   *
   * @override
   */
  prepareData() {
    super.prepareData();
  }

  /**
   * Calculates all Actor base data - anything not dependent on other entities.
   *
   * @override
   */
  prepareBaseData() {
    const flags = this.flags.boilerplate || {};
    switch (this.type) {
      case "Bestiary Character": return Calc.prepareBestiaryBase(this);
      case "Flood":              return Calc.prepareFloodBase(this);
      case "Named Character":    return Calc.prepareNamedCharacterBase(this);
      case "Vehicle":            return Calc.prepareVehicleBase(this);
    }
  }

  /**
   * Calculates all values for entities embedded in the Actor.
   *
   * @override
   */
  prepareEmbeddedEntities() {
    const flags = this.flags.boilerplate || {};
    switch (this.type) {
      case "Vehicle": return;
      default:        return Calc.prepareCharacterEmbedded(this);
    }
  }

  /**
   * Calculates all values derived from other entities.
   *
   * @override
   */
  prepareDerivedData() {
    const flags = this.flags.boilerplate || {};
    switch (this.type) {
      case "Bestiary Character": return Calc.prepareBestiaryDerived(this);
      case "Flood":              return Calc.prepareFloodDerived(this);
      case "Named Character":    return Calc.prepareNamedCharacterDerived(this);
      case "Vehicle":            return Calc.prepareVehicleDerived(this);
    }
  }
}
