export class MythicActor extends Actor {
  prepareData() {
    super.prepareData();
  }

  prepareBaseData() {

  }

  prepareDerivedData() {
    const actorData = this.data;
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
    const f = actorData.data.fatigue;
    const feltFatigue = f.enduring ? f.current - 2 : f.current;
    for (const [key, value] of Object.entries(actorData.data.characteristics)) {
      if (key != "extra") {
        const total = (
          value.soldierType + value.abilityPool + value.background +
          value.equipment + (parseInt(value.advancements) * 5) + value.other
        );
        value.total = total >= 0 ? total : 0;
        const roll = value.total + (-5 * (feltFatigue < 0 ? 0 : feltFatigue));
        value.roll = roll > 0 ? roll : 0;
      }
    }

    // Calculate Mythic Characteristics
    for (const [key, value] of Object.entries(actorData.data.mythicCharacteristics)) {
      if (key != "notes") {
        const total = (value.soldierType + value.equipment + value.other + 
          parseInt(value.advancements));
        value.total = total >= 0 ? total : 0;
      }
    }

    const str = actorData.data.characteristics.str.total;
    const strMod = str < 0 ? 0 : Math.floor(str / 10);
    const tou = actorData.data.characteristics.tou.total;
    const touMod = tou < 0 ? 0 : Math.floor(tou / 10);
    const agi = actorData.data.characteristics.agi.total;
    const agiMod = agi < 0 ? 0 : Math.floor(agi / 10);
    const int = actorData.data.characteristics.int.total;
    const intMod = int < 0 ? 0 : Math.floor(int / 10);

    // Calculate Wounds
    actorData.data.wounds.max = 20 + (
      (2 * (actorData.data.wounds.doubleTou ? touMod * 2 : touMod)) + 
      actorData.data.wounds.other + (actorData.data.wounds.aiDegen * -5) +
      (parseInt(actorData.data.wounds.advancements) * 4)
    );

    // Calculate Max Fatigue
    actorData.data.fatigue.max = 2 * touMod;

    // Calculate Luck
    const max = (
      actorData.data.luck.starting + actorData.data.luck.advancements +
      actorData.data.luck.other - actorData.data.luck.burnt
    );
    actorData.data.luck.max = max > 0 ? max : 0;

    // Calculate Support Points
    actorData.data.supportPoints.max = (
      actorData.data.supportPoints.rank + actorData.data.supportPoints.other
    );

    // Calculate Carry Weight
    const carry = (
      (actorData.data.carryingCapacity.doubleStr ? str * 2 : str) +
      ((actorData.data.carryingCapacity.strongBack ? tou + (touMod * 3) : tou) *
      (actorData.data.carryingCapacity.doubleTou ? 2 : 1)) +
      (actorData.data.mythicCharacteristics.str.total * 10) +
      (actorData.data.mythicCharacteristics.tou.total * 10)
    );
    actorData.data.carryingCapacity.carry = carry;
    actorData.data.carryingCapacity.lift = carry * 2;
    actorData.data.carryingCapacity.push = carry * 4;

    // Calculate Movement Distances
    const base = agiMod + actorData.data.mythicCharacteristics.agi.total;
    if (base <= 0) {
      actorData.data.movement.half = 0.5;
      actorData.data.movement.full = 1;
      actorData.data.movement.charge = 2;
      actorData.data.movement.run = 3;
    } else {
      actorData.data.movement.half = base;
      actorData.data.movement.full = base * 2;
      actorData.data.movement.charge = (
        ((actorData.data.movement.doubleAgiRunCharge ? base * 2 : base) * 3) +
        (actorData.data.movement.rush ? base : 0)
      );
      actorData.data.movement.run = (
        (actorData.data.movement.doubleAgiRunCharge ? base * 2 : base) * 6
      );
      actorData.data.movement.sprint = actorData.data.movement.blur ? base * 8 : "--";
    }
    const strLeap = Math.floor(strMod / 2);
    const agiLeap = Math.floor(agiMod / 2) + actorData.data.movement.leapAgiBonus;
    actorData.data.movement.jump = (strMod * actorData.data.movement.jumpMultiplier) / 4;
    actorData.data.movement.leap = (
      (strLeap > agiLeap ? strLeap : agiLeap) * actorData.data.movement.leapMultiplier
    );

    // Calculate Initiative
    const mythicAgi = actorData.data.mythicCharacteristics.agi.total;
    const battlemind = actorData.data.initiative.battleMind;
    let formula = [];
    formula.push(actorData.data.initiative.fastFoot ? "2d10kh" : "1d10");
    formula.push((battlemind ? intMod : agiMod).toString());
    if (!battlemind && mythicAgi > 0) {
      const bonus = Math.floor(mythicAgi / 2);
      formula.push(bonus > 1 ? bonus : 1);
    };
    formula.push(-5 * (feltFatigue < 0 ? 0 : feltFatigue));
    const mods = eval(formula.slice(1).join("+"));
    actorData.data.initiative.mods = (mods > 0 ? "+" : "") + mods.toString();
    actorData.data.initiative.formula = formula.join("+");
    
    // Calculate Skill Test Target Numbers
    for (const [key, value] of Object.entries(actorData.data.skills)) {
      if (key != "notes") {
        let target = value.mods;
        const stats = actorData.data.characteristics;
        target += stats[value.characteristic.toLowerCase()].roll;
        const tier = value.training.tier;
        if (tier === "none") {
          target -= (20 * value.training.penalty);
        } else if (tier === "plus10") {
          target += 10;
        } else if (tier === "plus20") {
          target += 20;
        }
        // TODO Add penalties for missing trainings on Technology tests.
        value.roll = target <= 0 ? 0 : target;
      }
    }

    //
  }
}