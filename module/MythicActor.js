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

    // Calculate Ability Pool
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

    // Calculate Characteristics
    for (const [key, value] of Object.entries(actorData.data.characteristics)) {
      if (key != "extra") {
        value.total = (value.soldierType + value.abilityPool + value.background
          + value.equipment + value.advancements + value.other);
        const f = actorData.data.fatigue;
        const feltFatigue = f.enduring ? f.current - 2 : f.current;
        const roll = value.total + (-5 * (feltFatigue < 0 ? 0 : feltFatigue));
        value.roll = roll > 0 ? roll : 0;
      }
    }

    // Calculate Mythic Characteristics
    for (const [key, value] of Object.entries(actorData.data.mythicCharacteristics)) {
      if (key != "notes") {
        value.total = value.soldierType + value.equipment + value.advancements + value.other;
      }
    }

    // Calculate Max Fatigue
    const tou = actorData.data.characteristics.tou.total;
    actorData.data.fatigue.max = 2 * (tou < 0 ? 0 : Math.floor(tou / 10));
  }
}