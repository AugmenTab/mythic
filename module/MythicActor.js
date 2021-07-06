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
    actorData.data.characteristics.extra.poolTotal = (
      actorData.data.characteristics.str.abilityPool +
      actorData.data.characteristics.tou.abilityPool +
      actorData.data.characteristics.agi.abilityPool +
      actorData.data.characteristics.wfr.abilityPool +
      actorData.data.characteristics.wfm.abilityPool +
      actorData.data.characteristics.int.abilityPool +
      actorData.data.characteristics.per.abilityPool +
      actorData.data.characteristics.cr.abilityPool +
      actorData.data.characteristics.ch.abilityPool +
      actorData.data.characteristics.ld.abilityPool
    );
  }
}