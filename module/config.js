/** @module config */

/**
 * The config object.
 */
export const mythic = {};

mythic.weaponGroups = {
  ranged: "mythic.weaponGroups.ranged",
  melee: "mythic.weaponGroups.melee",
  thrown: "mythic.weaponGroups.thrown",
}

mythic.ammoGroups = {
  none: "mythic.ammoGroups.none", 
  std: "mythic.ammoGroups.std", 
  shotgun: "mythic.ammoGroups.shotgun", 
  flamethrower: "mythic.ammoGroups.flamethrower", 
  sniper: "mythic.ammoGroups.sniper", 
  grenade: "mythic.ammoGroups.grenade", 
  mrc: "mythic.ammoGroups.mrc", 
  bruteShot: "mythic.ammoGroups.bruteShot", 
}

mythic.trainings = {
  equipment: {
    basic: "mythic.trainings.equipment.basic",
    infantry: "mythic.trainings.equipment.infantry",
    advanced: "mythic.trainings.equipment.advanced",
    longRange: "mythic.trainings.equipment.longRange",
    vehicle: "mythic.trainings.equipment.vehicle",
    explosive: "mythic.trainings.equipment.explosive",
    melee: "mythic.trainings.equipment.melee",
    heavy: "mythic.trainings.equipment.heavy"
  },
  examples: {
    basic: "mythic.trainings.examples.basic",
    infantry: "mythic.trainings.examples.infantry",
    advanced: "mythic.trainings.examples.advanced",
    longRange: "mythic.trainings.examples.longRange",
    vehicle: "mythic.trainings.examples.vehicle",
    explosive: "mythic.trainings.examples.explosive",
    melee: "mythic.trainings.examples.melee",
    heavy: "mythic.trainings.examples.heavy"
  },
  factions: {
    unsc: "mythic.trainings.factions.unsc",
    covenant: "mythic.trainings.factions.covenant",
    forerunner: "mythic.trainings.factions.forerunner"
  }
}

mythic.abilityTypes = {
  ability: "mythic.characterTalents.abilities.type.ability",
  racial: "mythic.characterTalents.abilities.type.racial",
  trait: "mythic.characterTalents.abilities.type.trait",
  augmentation: "mythic.characterTalents.abilities.type.augmentation"
}

mythic.advancements = {
  characteristics: {
    0: "--",
    1: "+5",
    2: "+10",
    3: "+15",
    4: "+20",
    5: "+25"
  },
  mythicCharacteristics: {
    0: "--",
    1: "+1",
    2: "+2"
  },
  wounds: {
    0: "+0",
    1: "+4",
    2: "+8",
    3: "+12",
    4: "+16"
  }
}

mythic.difficulty = {
  0: "mythic.difficulty.easy",
  1: "mythic.difficulty.normal",
  2: "mythic.difficulty.heroic",
  3: "mythic.difficulty.legendary",
  4: "mythic.difficulty.nemesis"
}

mythic.educationType = {
  biological: "mythic.educationSheet.types.biological",
  computer: "mythic.educationSheet.types.computer",
  cultural: "mythic.educationSheet.types.cultural",
  dimensional: "mythic.educationSheet.types.dimensional",
  engineer: "mythic.educationSheet.types.engineer",
  military: "mythic.educationSheet.types.military",
  piloting: "mythic.educationSheet.types.piloting",
  restricted: "mythic.educationSheet.types.restricted",
  space: "mythic.educationSheet.types.space"
}

mythic.grips = {
  solid: "mythic.weaponSheet.grips.solid",
  slight: "mythic.weaponSheet.grips.slight",
  partial: "mythic.weaponSheet.grips.partial",
  sloppy: "mythic.weaponSheet.grips.sloppy"
}

mythic.skills = {
  appeal: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.social",
    characteristics: ["CH"],
    floodCharacteristics: []
  },
  athletics: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.move",
    characteristics: ["AGI", "STR"],
    floodCharacteristics: ["AGI", "STR"]
  },
  camouflage: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT", "PER"],
    floodCharacteristics: ["INT", "PER"]
  },
  command: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.social",
    characteristics: ["LD"],
    floodCharacteristics: []
  },
  cryptography: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT"],
    floodCharacteristics: ["INT"]
  },
  deception: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.social",
    characteristics: ["CH"],
    floodCharacteristics: []
  },
  demolition: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT"],
    floodCharacteristics: ["INT"]
  },
  evasion: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.move",
    characteristics: ["AGI"],
    floodCharacteristics: ["AGI"]
  },
  gambling: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.social",
    characteristics: ["INT", "CH"],
    floodCharacteristics: []
  },
  interrogation: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.social",
    characteristics: ["CH", "LD", "INT"],
    floodCharacteristics: []
  },
  intimidation: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.social",
    characteristics: ["STR", "CH", "LD", "INT"],
    floodCharacteristics: ["STR", "INT"]
  },
  investigation: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.social",
    characteristics: ["INT", "PER"],
    floodCharacteristics: ["INT", "PER"]
  },
  medHuman: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT"],
    floodCharacteristics: []
  },
  medCovenant: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT"],
    floodCharacteristics: []
  },
  medMgalekgolo: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT"],
    floodCharacteristics: []
  },
  navGroundAir: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT", "PER"],
    floodCharacteristics: ["INT", "PER"]
  },
  navSpace: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT", "PER"],
    floodCharacteristics: ["INT", "PER"]
  },
  navSlipspace: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT", "PER"],
    floodCharacteristics: ["INT", "PER"]
  },
  negotiation: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.social",
    characteristics: ["CH"],
    floodCharacteristics: []
  },
  pilotGround: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.field",
    characteristics: ["AGI", "INT"],
    floodCharacteristics: ["AGI", "INT"]
  },
  pilotAir: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.field",
    characteristics: ["AGI", "INT"],
    floodCharacteristics: ["AGI", "INT"]
  },
  pilotSpace: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.field",
    characteristics: ["AGI", "INT"],
    floodCharacteristics: ["AGI", "INT"]
  },
  security: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT"],
    floodCharacteristics: ["INT"]
  },
  stunting: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.move",
    characteristics: ["AGI"],
    floodCharacteristics: ["AGI"]
  },
  survival: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT", "PER"],
    floodCharacteristics: ["INT", "PER"]
  },
  techHuman: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT"],
    floodCharacteristics: ["INT"]
  },
  techCovenant: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT"],
    floodCharacteristics: ["INT"]
  },
  techForerunner: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT"],
    floodCharacteristics: ["INT"]
  }
}

mythic.skillDifficulty = {
  basic: "mythic.skillSheet.difficulties.basic",
  adv: "mythic.skillSheet.difficulties.adv"
}

mythic.skillTraining = {
  "none": "mythic.skillTraining.none",
  "trained": "mythic.skillTraining.trained",
  "plus10": "mythic.skillTraining.plus10",
  "plus20": "mythic.skillTraining.plus20"
}

mythic.educationTraining = {
  "none": "mythic.educationTraining.none",
  "plus5": "mythic.educationTraining.plus5",
  "plus10": "mythic.educationTraining.plus10"
}

mythic.skillType = {
  social: "mythic.skillSheet.types.social",
  move: "mythic.skillSheet.types.move",
  field: "mythic.skillSheet.types.field",
}

mythic.size = {
  mini: "mythic.sizes.mini",
  small: "mythic.sizes.small",
  normal: "mythic.sizes.normal",
  large: "mythic.sizes.large",
  huge: "mythic.sizes.huge",
  hulking: "mythic.sizes.hulking",
  giant: "mythic.sizes.giant",
  immense: "mythic.sizes.immense",
  massive: "mythic.sizes.massive",
  great: "mythic.sizes.great",
  monumental: "mythic.sizes.monumental",
  colossal: "mythic.sizes.colossal",
  vast: "mythic.sizes.vast"
}

mythic.characteristic = {
  str: "STR",
  tou: "TOU",
  agi: "AGI",
  wfr: "WFR",
  wfm: "WFM",
  int: "INT",
  per: "PER",
  cr: "CR",
  ch: "CH",
  ld: "LD"
}