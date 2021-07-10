const FORMULA = "D100";
const CHARACTERISTICS = {
  "str": "Strength",
  "tou": "Toughness",
  "agi": "Agility",
  "wfr": "Warfare Ranged",
  "wfm": "Warfare Melee",
  "int": "Intellect",
  "per": "Perception",
  "cr": "Courage",
  "ch": "Charisma",
  "ld": "Leadership"
};

export async function rollCharacteristicTest(stat, target) {
  const roll = await new Roll(FORMULA).roll({ async: true });
  let result = {
    test: CHARACTERISTICS[stat],
    roll: roll.total,
    target: target,
    critical: false,
    degrees: 0,
    outcome: ""
  };
  if (roll.total >= 98) {
    result.critical = true;
    result.outcome = "failure";
  } else if (roll.total === 1) {
    result.critical = true;
    result.outcome = "success";
  } else {
    const d = (target - roll.total) / 10;
    result.outcome = d >= 0 ? "success" : "failure";
    result.degrees = Math.abs(d).toFixed(1);
  }
  return result;
}