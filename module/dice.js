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

export function rollCharacteristicTest(stat, target) {
  return `${CHARACTERISTICS[stat]}: ${target}`;
}