import * as Calc from "./calculations.js";

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
    if (actorData.type !== "Bestiary Character") return;
  }

  _prepareFloodCharacterData(actorData) {
    if (actorData.type !== "Flood") return;
  }

  _prepareNamedCharacterData(actorData) {
    if (actorData.type !== "Named Character") return;

    // Calculate Ability Pool
    Calc.calculateAbilityPool(actorData);

    // Calculate Characteristics
    const f = actorData.data.fatigue;
    const feltFatigue = f.enduring ? f.current - 2 : f.current;
    Calc.calculateCharacteristics(actorData, feltFatigue);

    // Calculate Mythic Characteristics
    Calc.calculateMythicCharacteristics(actorData);

    // Reference Characteristics and Modifiers
    const str = actorData.data.characteristics.str.total;
    const strMod = (Calc.calculateCharacteristicModifier(str)
      + actorData.data.mythicCharacteristics.str.total);
    const tou = actorData.data.characteristics.tou.total;
    const touMod = Calc.calculateCharacteristicModifier(tou);
    const agi = actorData.data.characteristics.agi.total;
    const agiMod = Calc.calculateCharacteristicModifier(agi);
    const int = actorData.data.characteristics.int.total;
    const intMod = Calc.calculateCharacteristicModifier(int);

    // Calculate Toughness DR
    actorData.data.characteristics.extra.touDR = touMod + actorData.data.mythicCharacteristics.tou.total;

    // Calculate Experience
    Calc.calculateExperience(actorData);

    // Calculate Wounds
    Calc.calculateWounds(actorData, touMod);

    // Calculate Max Fatigue
    Calc.calculateMaxFatigue(actorData, touMod);

    // Calculate Luck
    Calc.calculateLuck(actorData);

    // Calculate Support Points
    Calc.calculateSupportPoints(actorData);

    // Calculate Carry Weight
    Calc.calculateCarryWeight(actorData, str, tou);

    // Calculate Movement Distances
    Calc.calculateMovementDistances(actorData, strMod, agiMod);

    // Calculate Initiative
    Calc.calculateInitiative(actorData, agiMod, intMod, feltFatigue);
    
    // Calculate Skill Test Target Numbers
    Calc.calculateSkillTargets(actorData);

    // Calculate Education Test Target Numbers
    Calc.calculateEducationTargets(actorData);

    // Fix Talent Dependencies
    if (!actorData.data.trainings.weapons.hth) actorData.data.trainings.weapons.mac = false;

    // Calculate Weapon Attacks
    Calc.calculateWeaponSummaryAttackData(actorData);
  }

  _prepareVehicleData(actorData) {
    if (actorData.type !== "Vehicle") return;
  }
}