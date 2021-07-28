const MELEE_REACH_SIZE_BONUS = {
  "mini": 1,
  "small": 1,
  "normal": 1,
  "large": 2,
  "huge": 2,
  "hulking": 3,
  "giant": 3,
  "immense": 4,
  "massive": 4,
  "great": 5,
  "monumental": 5
};

export function calculateCharacteristicModifier(score) {
  return score < 0 ? 0 : Math.floor(score / 10);
}

export function prepareBestiary(actorData) {
  return;
}

export function prepareFlood(actorData) {
  return;
}

export function prepareNamedCharacter(actorData) {
  // Calculate Ability Pool
  calculateAbilityPool(actorData);

  // Calculate Characteristics
  const f = actorData.data.fatigue;
  const feltFatigue = f.enduring ? f.current - 2 : f.current;
  calculateCharacteristics(actorData, feltFatigue);

  // Calculate Mythic Characteristics
  calculateMythicCharacteristics(actorData);

  // Reference Characteristics and Modifiers
  const str = actorData.data.characteristics.str.total;
  const strMod = (calculateCharacteristicModifier(str)
    + actorData.data.mythicCharacteristics.str.total);
  const tou = actorData.data.characteristics.tou.total;
  const touMod = calculateCharacteristicModifier(tou);
  const agi = actorData.data.characteristics.agi.total;
  const agiMod = calculateCharacteristicModifier(agi);
  const int = actorData.data.characteristics.int.total;
  const intMod = calculateCharacteristicModifier(int);

  // Calculate Toughness DR
  actorData.data.characteristics.extra.touDR = touMod + actorData.data.mythicCharacteristics.tou.total;

  // Calculate Experience
  calculateExperience(actorData);

  // Calculate Wounds
  calculateWounds(actorData, touMod);

  // Calculate Max Fatigue
  calculateMaxFatigue(actorData, touMod);

  // Calculate Luck
  calculateLuck(actorData);

  // Calculate Support Points
  calculateSupportPoints(actorData);

  // Calculate Carry Weight
  calculateCarryWeight(actorData, str, tou);

  // Calculate Movement Distances
  calculateMovementDistances(actorData, strMod, agiMod);

  // Calculate Initiative
  calculateInitiative(actorData, agiMod, intMod, feltFatigue);
  
  // Calculate Skill Test Target Numbers
  calculateSkillTargets(actorData);

  // Calculate Education Test Target Numbers
  calculateEducationTargets(actorData);

  // Fix Talent Dependencies
  if (!actorData.data.trainings.weapons.hth) actorData.data.trainings.weapons.mac = false;

  // Calculate Weapon Attacks
  calculateWeaponSummaryAttackData(actorData);
}

export function prepareVehicle(actorData) {
  return;
}

export function sortAndFilterItems(items, filterParam, sortParam = "name") {
  let f = items.filter(function(item) { return item.type === filterParam });
  console.log()
  if (sortParam === "name") {
    return f.sort((a, b) => a.name < b.name ? -1 : (a.name > b.name ? 1 : 0));
  } else if (sortParam === "nickname") {
    return f.sort((a, b) => (
      a.data.nickname < b.data.nickname ? -1 : (a.data.nickname > b.data.nickname ? 1 : 0))
    );
  }
}

function calculateAbilityPool(actorData) {
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

function calculateCarryWeight(actorData, str, tou) {
  const touMod = calculateCharacteristicModifier(tou);
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
}

function calculateCharacteristics(actorData, feltFatigue) {
  for (const [key, value] of Object.entries(actorData.data.characteristics)) {
    if (key != "extra") {
      const total = (
        value.soldierType + value.abilityPool + value.background +
        value.equipment + (parseInt(value.advancements) * 5) + value.other
      );
      value.total = Math.floor(total >= 0 ? total : 0);
      const roll = value.total + (-5 * (feltFatigue < 0 ? 0 : feltFatigue));
      value.roll = Math.floor(roll > 0 ? roll : 0);
    }
  }
}

function calculateEducationTargets(actorData) {
  let educations = actorData.items.filter(function(item) { return item.type === "education" });
  for (let e of Object.values(educations)) {
    const base = e.data.data.roll.skill === "int"
      ? actorData.data.characteristics.int.roll
      : actorData.data.skills[e.data.data.roll.skill].roll;
    const training = e.data.data.roll.training !== "none"
      ? parseInt(e.data.data.roll.training.replace("plus", ""))
      : 0;
    e.data.data.roll.roll = base + training + e.data.data.roll.mods;
  }
}

function calculateExperience(actorData) {
  const totalExp = actorData.data.experience.total;
  actorData.data.experience.current = totalExp - actorData.data.experience.spent;
  if (totalExp >= 32000) {
    actorData.data.experience.tier = 7;
  } else if (totalExp >= 16000 ) {
    actorData.data.experience.tier = 6;
  } else if (totalExp >= 8000) {
    actorData.data.experience.tier = 5;
  } else if (totalExp >= 4000) {
    actorData.data.experience.tier = 4;
  } else if (totalExp >= 2000) {
    actorData.data.experience.tier = 3;
  } else if (totalExp >= 1000) {
    actorData.data.experience.tier = 2;
  } else if (totalExp >= 500) {
    actorData.data.experience.tier = 1;
  } else {
    actorData.data.experience.tier = 0;
  }
}

function calculateInitiative(actorData, agiMod, intMod, feltFatigue) {
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
}

function calculateLuck(actorData) {
  const max = (
    actorData.data.luck.starting + actorData.data.luck.advancements +
    actorData.data.luck.other - actorData.data.luck.burnt
  );
  actorData.data.luck.max = max > 0 ? max : 0;
}

function calculateMaxFatigue(actorData, touMod) {
  actorData.data.fatigue.max = 2 * touMod;
}

function calculateMovementDistances(actorData, strMod, agiMod) {
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
}

function calculateMythicCharacteristics(actorData) {
  for (const [key, value] of Object.entries(actorData.data.mythicCharacteristics)) {
    if (key != "notes") {
      const total = (value.soldierType + value.equipment + value.other + 
        parseInt(value.advancements));
      value.total = total >= 0 ? total : 0;
    }
  }
}

function calculateSkillTargets(actorData) {
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
      if ( (key === "techHuman" && !actorData.data.trainings.faction.unsc)
        || (key === "techCovenant" && !actorData.data.trainings.faction.covenant)
        || (key === "techForerunner" && !actorData.data.trainings.faction.forerunner)
      ) {
        target -= actorData.data.trainings.alienTech ? 10 : 20;
      }
      value.roll = target <= 0 ? 0 : target;
    }
  }
}

function calculateSupportPoints(actorData) {
  actorData.data.supportPoints.max = (
    actorData.data.supportPoints.rank + actorData.data.supportPoints.other
  );
}

function calculateWeaponAttacksMelee(actorData, weapon) {
  const mod = calculateCharacteristicModifier(actorData.data.characteristics.wfm.total);
  let half = mod > 7 ? 4 : Math.ceil(mod / 2);
  let full = half * 2;
  if (actorData.data.trainings.weapons.hth) {
    half += 1;
    full += 2;
  }
  if (actorData.data.trainings.weapons.mac) full += full >= 10 ? 0 : 1;
  weapon.data.data.attack.half = half;
  weapon.data.data.attack.full = full;
  weapon.data.data.attack.fireMode = "melee";
}

function calculateWeaponAttacksRanged(weapon) {
  const a = weapon.data.data.attack.fireMode.split("-");
  const mode = a[0], attacks = parseInt(a[1]);
  if (["auto", "sustained"].includes(mode)) {
    const half = Math.floor(attacks / 2);
    const mag = weapon.data.data.magazine.current;
    weapon.data.data.attack.half = mag > half ? half : mag;
    weapon.data.data.attack.full = mag > attacks ? attacks : mag;
  } else if (["burst", "pump", "semi"].includes(mode)) {
    const full = attacks * 2;
    const mag = weapon.data.data.magazine.current;
    weapon.data.data.attack.half = mag > attacks ? attacks : mag;
    weapon.data.data.attack.full = mag > full ? full : mag;
  } else if (mode === "charge") {
    weapon.data.data.attack.half = 1;
  } else if (mode === "drawback") {
    weapon.data.data.attack.half = 1;
    weapon.data.data.attack.full = 1;
  } else if (mode === "flintlock") {
    weapon.data.data.attack.full = 1;
  }
}

function calculateWeaponRangeMelee(actorData, weapon) {
  weapon.data.data.range.melee = (
    weapon.data.data.range.close + MELEE_REACH_SIZE_BONUS[actorData.data.size]
  );
}

function calculateWeaponRangeThrown(actorData, weapon) {
  const base = (
    calculateCharacteristicModifier(actorData.data.characteristics.str.total) +
    actorData.data.mythicCharacteristics.str.total
  );
  let mult = 15;
  const penalty = calculateWeightPenaltyThrown(base);
  mult -= Math.floor(weapon.data.data.weight.each / penalty.weight) * penalty.multiplier;
  if (weapon.data.data.range.grip === "slight") mult -= 1;
  if (["partial", "sloppy"].includes(weapon.data.data.range.grip)) mult -= 2;
  const range = (base * mult) / (weapon.data.data.range.grip === "sloppy" ? 2 : 1);
  weapon.data.data.range.thrown = range > 0.5 ? range : 0;
}

function calculateWeaponReloadStandard(actorData, weapon) {
  let base = weapon.data.data.reload.base;
  if (actorData.data.trainings.weapons.rapidReload) base = Math.ceil(base / 2);
  const agiMod = calculateCharacteristicModifier(actorData.data.characteristics.agi.total);
  const wfrMod = calculateCharacteristicModifier(actorData.data.characteristics.wfr.total);
  const final = base - Math.floor(agiMod / 2) - Math.floor(wfrMod / 2);
  weapon.data.data.reload.total = final < 1 ? "R" : final;
}

function calculateWeaponReloadSingleLoading(actorData, weapon) {
  let base = 1;
  const agiMod = calculateCharacteristicModifier(actorData.data.characteristics.agi.total);
  const wfrMod = calculateCharacteristicModifier(actorData.data.characteristics.wfr.total);
  const final = base + Math.floor(agiMod / 2) + Math.floor(wfrMod / 2);
  weapon.data.data.reload.total = final > 3 ? 3 : final;
}

function calculateWeaponTarget(actorData, weapon) {
  const group = weapon.data.data.group;
  const mode = weapon.data.data.attack.fireMode.split("-")[0];
  const stat = group === "ranged"
    ? actorData.data.characteristics.wfr.roll
    : actorData.data.characteristics.wfm.roll;
  let mod = stat + weapon.data.data.attack.attackBonus;
  if (!actorData.data.trainings.faction[weapon.data.data.trainings.faction]) {
    mod -= actorData.data.trainings.alienTech ? 10 : 20;
  }
  if (!actorData.data.trainings.equipment[weapon.data.data.trainings.equipment]) {
    mod -= 10;
  }
  if (["burst", "semi", "sustained"].includes(mode)) {
    mod += 10;
  }
  if (group === "melee" && actorData.data.trainings.weapons.hth) {
    mod += 5;
  }
  if (group === "melee" && actorData.data.trainings.weapons.mac) {
    mod += 10;
  }
  weapon.data.data.attack.target = mod > 0 ? mod : 0;
}

function calculateWeaponSummaryAttackData(actorData) {
  let weapons = actorData.items.filter(function(item) { return item.type === "weapon" });
  for (let weapon of Object.values(weapons)) {
    if (weapon.data.data.group === "thrown") {
      calculateWeaponRangeThrown(actorData, weapon);
      weapon.data.data.attack.half = 1;
      weapon.data.data.attack.full = 1;
      weapon.data.data.attack.fireMode = "thrown"; 
    } else if (weapon.data.data.group === "melee") {
      calculateWeaponRangeMelee(actorData, weapon);
      calculateWeaponAttacksMelee(actorData, weapon);
    } else if (weapon.data.data.group === "ranged") {
      calculateWeaponAttacksRanged(weapon);
      if (weapon.data.data.special.singleLoading.has) {
        calculateWeaponReloadSingleLoading(actorData, weapon);
      } else calculateWeaponReloadStandard(actorData, weapon);
    }
    calculateWeaponTarget(actorData, weapon);
  }
}

function calculateWeightPenaltyThrown(mod) {
  let penalty = { weight: 1, multiplier: 1 };
  if (mod >= 19) penalty.weight = 7;
  else if (mod >= 13) penalty.weight = 6;
  else if (mod >= 10) penalty.weight = 5;
  else if (mod >= 8) penalty.weight = 2;
  else if (mod >= 3) penalty.multiplier = 2;
  else penalty.multiplier = 3;
  return penalty;
}

function calculateWounds(actorData, touMod) {
  actorData.data.wounds.max = 20 + (
    (2 * (actorData.data.wounds.doubleTou ? touMod * 2 : touMod)) + 
    actorData.data.wounds.other + (actorData.data.wounds.aiDegen * -5) +
    (parseInt(actorData.data.wounds.advancements) * 4)
  );
}