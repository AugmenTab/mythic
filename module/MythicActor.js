import { prepareBestiary, prepareFlood, prepareNamedCharacter, prepareVehicle } from "./calculations.js";

export class MythicActor extends Actor {
  prepareData() {
    super.prepareData();
  }

  prepareBaseData() {
    const actorData = this.data;
    const flags = actorData.flags.boilerplate || {};

    this._prepareNamedCharacterData(actorData);
    this._prepareBestiaryCharacterData(actorData);
    this._prepareFloodCharacterData(actorData);
    this._prepareVehicleData(actorData);
  }

  prepareEmbeddedEntities() {}
  
  prepareDerivedData() {}

  _prepareBestiaryCharacterData(actorData) {
    if (actorData.type === "Bestiary Character") {
      prepareBestiary(actorData);
    }
  }

  _prepareFloodCharacterData(actorData) {
    if (actorData.type === "Flood") {
      prepareFlood(actorData);
    }
  }

  _prepareNamedCharacterData(actorData) {
    if (actorData.type === "Named Character") {
      prepareNamedCharacter(actorData);
    }
  }

  _prepareVehicleData(actorData) {
    if (actorData.type === "Vehicle") {
      prepareVehicle(actorData);
    }
  }
}