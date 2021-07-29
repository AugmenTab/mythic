/** @module MythicActor */

import { prepareBestiary, prepareFlood, prepareNamedCharacter, prepareVehicle } from "./calculations.js";

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

    this._prepareNamedCharacterData(actorData);
    this._prepareBestiaryCharacterData(actorData);
    this._prepareFloodCharacterData(actorData);
    this._prepareVehicleData(actorData);
  }

  /** Calculates all values for entities embedded in the Actor. */
  prepareEmbeddedEntities() {}
  
  /** Calculates all values derived from other entities */
  prepareDerivedData() {}

  /** Prepares character data for the Bestiary Enemy Actor type.
   * @param {ActorData} actorData - The prepared ActorData.
  */
  _prepareBestiaryCharacterData(actorData) {
    if (actorData.type === "Bestiary Character") {
      prepareBestiary(actorData);
    }
  }

/**
 * Prepares character data for the Flood Actor type.
 * @param {ActorData} actorData - The prepared ActorData.
 */
  _prepareFloodCharacterData(actorData) {
    if (actorData.type === "Flood") {
      prepareFlood(actorData);
    }
  }

  /**
   * Prepares character data for the Named Character Actor type.
   * @param {ActorData} actorData - The prepared ActorData.
   */
  _prepareNamedCharacterData(actorData) {
    if (actorData.type === "Named Character") {
      prepareNamedCharacter(actorData);
    }
  }

  /**
   * Prepares character data for the Vehicle Actor type.
   * @param {ActorData} actorData - The prepared ActorData.
   */
  _prepareVehicleData(actorData) {
    if (actorData.type === "Vehicle") {
      prepareVehicle(actorData);
    }
  }
}