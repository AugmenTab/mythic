/** @module MythicActor */

import * as Calc from "./calculations.js";

/**
 * Prepares Actor data using callbacks.
 * @extends Actor
 */
export class MythicActor extends Actor {

  /** Prepares ActorData. */
  prepareData() {
    super.prepareData();
  }

  /** Calculates all Actor base data - anything not dependent on other entities. */
  prepareBaseData() {
    const actorData = this.data;
    const flags = actorData.flags.boilerplate || {};
    this._prepareCharacterBaseData(actorData);
  }

  /** Calculates all values for entities embedded in the Actor. */
  prepareEmbeddedEntities() {
    const actorData = this.data;
    const flags = actorData.flags.boilerplate || {};
    this._prepareCharacterEmbedded(actorData);
  }
  
  /** Calculates all values derived from other entities. */
  prepareDerivedData() {
    const actorData = this.data;
    const flags = actorData.flags.boilerplate || {};
    this._prepareCharacterDerivedData(actorData);
  }

  /**
   * Prepares base character data for an Actor.
   * @param {ActorData} actorData - The prepared ActorData.
   */
  _prepareCharacterBaseData(actorData) {
    switch (actorData.type) {
      case "Bestiary Enemy":
        Calc.prepareBestiaryBase(actorData);
        break;
      case "Flood":
        Calc.prepareFloodBase(actorData);
        break;
      case "Named Character":
        Calc.prepareNamedCharacterBase(actorData);
        break;
      case "Vehicle":
        Calc.prepareVehicleBase(actorData);
        break;
    }
  }

  /**
   * Prepares data for all embedded entities on an Actor.
   * @param {ActorData} actorData - The prepared ActorData.
   */
  _prepareCharacterEmbedded(actorData) {
    switch (actorData.type) {
      case "Flood":
        Calc.prepareFloodEmbedded(actorData);
        break;
      case "Vehicle":
        Calc.prepareVehicleEmbedded(actorData);
        break;
      default: Calc.prepareCharacterEmbedded(actorData);
    }
  }

  /**
   * Prepares all data for an Actor that is dependent on embedded entities.
   * @param {ActorData} actorData - The prepared ActorData.
   */
  _prepareCharacterDerivedData(actorData) {
    switch (actorData.type) {
      case "Bestiary Enemy":
        Calc.prepareBestiaryDerived(actorData);
        break;
      case "Flood":
        Calc.prepareFloodDerived(actorData);
        break;
      case "Named Character":
        Calc.prepareNamedCharacterDerived(actorData);
        break;
      case "Vehicle":
        Calc.prepareVehicleDerived(actorData);
        break;
    }
  }
}