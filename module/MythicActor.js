export class MythicActor extends Actor {
  prepareData() {
    super.prepareData();
  }

  prepareBaseData() {

  }

  prepareDerivedData() {
    const actorData = this.data;
    const data = actorData.data;
    const flags = actorData.flags.boilerplate || {};

    this._prepareNamedCharacterData(actorData);
    // this._prepareBestiaryCharacterData(actorData);
    // this._prepareFloodCharacterData(actorData);
    // this._prepareVehicleData(actorData);
  }

  _prepareNamedCharacterData(actorData) {
    if (actorData.type !== "Named Character") return;
    console.log(actorData);
  }
}