export function calculateAbilityPool(actorData) {
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

export function calculateCarryWeight(actorData, str, tou) {
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

export function calculateCharacteristics(actorData, feltFatigue) {
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

export function calculateCharacteristicModifier(score) {
  return score < 0 ? 0 : Math.floor(score / 10);
}

export function calculateEducationTargets(actorData) {
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

export function calculateExperience(actorData) {
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

export function calculateInitiative(actorData, agiMod, intMod, feltFatigue) {
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

export function calculateLuck(actorData) {
  const max = (
    actorData.data.luck.starting + actorData.data.luck.advancements +
    actorData.data.luck.other - actorData.data.luck.burnt
  );
  actorData.data.luck.max = max > 0 ? max : 0;
}

export function calculateMaxFatigue(actorData, touMod) {
  actorData.data.fatigue.max = 2 * touMod;
}

export function calculateMovementDistances(actorData, strMod, agiMod) {
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

export function calculateMythicCharacteristics(actorData) {
  for (const [key, value] of Object.entries(actorData.data.mythicCharacteristics)) {
    if (key != "notes") {
      const total = (value.soldierType + value.equipment + value.other + 
        parseInt(value.advancements));
      value.total = total >= 0 ? total : 0;
    }
  }
}

export function calculateSkillTargets(actorData) {
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

export function calculateSupportPoints(actorData) {
  actorData.data.supportPoints.max = (
    actorData.data.supportPoints.rank + actorData.data.supportPoints.other
  );
}

export function calculateWeaponAttacksMelee(actorData, weapon) {
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
}

export function calculateWeaponAttacksRanged(weapon) {
  const a = weapon.data.data.attack.fireMode.split("-");
  const mode = a[0], attacks = parseInt(a[1]);
  if (["auto", "sustained"].includes(mode)) {
    weapon.data.data.attack.half = Math.floor(attacks / 2);
    weapon.data.data.attack.full = attacks;
  } else if (["burst", "pump", "semi"].includes(mode)) {
    weapon.data.data.attack.half = attacks;
    weapon.data.data.attack.full = attacks * 2;
  } else if (mode === "charge") {
    weapon.data.data.attack.half = 1;
  } else if (mode === "drawback") {
    weapon.data.data.attack.half = 1;
    weapon.data.data.attack.full = 1;
  } else if (mode === "flintlock") {
    weapon.data.data.attack.full = 1;
  }
}

export function calculateWeaponSummaryAttackData(actorData) {
  let weapons = actorData.items.filter(function(item) { return item.type === "weapon" });
  for (let weapon of Object.values(weapons)) {
    if (weapon.data.data.group === "thrown") {
      weapon.data.data.attack.half = 1;
      weapon.data.data.attack.full = 1;  
    } else if (weapon.data.data.group === "melee") {
      calculateWeaponAttacksMelee(actorData, weapon);
    } else if (weapon.data.data.group === "ranged") {
      calculateWeaponAttacksRanged(weapon);
    }
  }
}

export function calculateWounds(actorData, touMod) {
  actorData.data.wounds.max = 20 + (
    (2 * (actorData.data.wounds.doubleTou ? touMod * 2 : touMod)) + 
    actorData.data.wounds.other + (actorData.data.wounds.aiDegen * -5) +
    (parseInt(actorData.data.wounds.advancements) * 4)
  );
}

export function sortAndFilterItems(items, filterParam, sortParam = "name") {
  let f = items.filter(function(item) { return item.type === filterParam });
  if (sortParam === "name") {
    return f.sort((a, b) => a.name < b.name ? -1 : (a.name > b.name ? 1 : 0));
  } else if (sortParam === "nickname") {
    return f.sort((a, b) => a.nickname < b.nickname ? -1 : (a.nickname > b.nickname ? 1 : 0));
  }
}