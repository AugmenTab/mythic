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

    this._prepareBestiaryCharacterBaseData(actorData);
    this._prepareFloodCharacterBaseData(actorData);
    this._prepareNamedCharacterBaseData(actorData);
    this._prepareVehicleBaseData(actorData);
  }

  /** Calculates all values for entities embedded in the Actor. */
  prepareEmbeddedEntities() {}
  
  /** Calculates all values derived from other entities. */
  prepareDerivedData() {
    const actorData = this.data;
    const flags = actorData.flags.boilerplate || {};

    this._prepareBestiaryCharacterDerivedData(actorData);
    this._prepareFloodCharacterDerivedData(actorData);
    this._prepareNamedCharacterDerivedData(actorData);
  }

  /** 
   * Prepares base character data for the Bestiary Enemy Actor type.
   * @param {ActorData} actorData - The prepared ActorData.
   */
  _prepareBestiaryCharacterBaseData(actorData) {
    if (actorData.type !== "Bestiary Character") return;
    Calc.prepareBestiaryBase(actorData);
  }

  /**
   * Prepares derived character data for the Bestiary Enemy Actor type.
   * @param {ActorData} actorData - The prepared ActorData.
   */
  _prepareBestiaryCharacterDerivedData(actorData) {
    if (actorData.type !== "Bestiary Character") return;
    Calc.prepareBestiaryDerived(actorData);
  }

  /**
   * Prepares base character data for the Flood Actor type.
   * @param {ActorData} actorData - The prepared ActorData.
   */
  _prepareFloodCharacterBaseData(actorData) {
    if (actorData.type !== "Flood") return;
    Calc.prepareFloodBase(actorData);
  }

  /**
   * Prepares derived character data for the Flood Actor type.
   * @param {ActorData} actorData - The prepared ActorData.
   */
  _prepareFloodCharacterDerivedData(actorData) {
    if (actorData.type !== "Flood") return;
    Calc.prepareFloodDerived(actorData);
  }

  /**
   * Prepares base character data for the Named Character Actor type.
   * @param {ActorData} actorData - The prepared ActorData.
   */
  _prepareNamedCharacterBaseData(actorData) {
    if (actorData.type !== "Named Character") return;
    Calc.prepareNamedCharacterBase(actorData);
  }

  /**
   * Prepares derived character data for the Named Character Actor type.
   * @param {ActorData} actorData - The prepared ActorData.
   */
  _prepareNamedCharacterDerivedData(actorData) {
    if (actorData.type !== "Named Character") return;
    Calc.prepareNamedCharacterDerived(actorData);
  }

  /**
   * Prepares base character data for the Vehicle Actor type.
   * @param {ActorData} actorData - The prepared ActorData.
   */
  _prepareVehicleBaseData(actorData) {
    if (actorData.type !== "Vehicle") return;
    Calc.prepareVehicleBase(actorData);
  }

  /**
   * Prepares derived character data for the Vehicle Actor type.
   * @param {ActorData} actorData - The prepared ActorData.
   */
  _prepareVehicleDerived(actorData) {
    if (actorData.type !== "Vehicle") return;
    Calc.prepareVehicleDerived(actorData);
  }
}