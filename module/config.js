export const mythic = {};

mythic.weaponGroups = {
  ranged: "mythic.weaponGroups.ranged",
  melee: "mythic.weaponGroups.melee",
  thrown: "mythic.weaponGroups.thrown",
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

mythic.abilitySheet = {
  summary: "mythic.abilitySheet.summary",
  prerequisite: "mythic.abilitySheet.prerequisite",
  cost: "mythic.abilitySheet.cost",
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

mythic.educationType = {
  piloting: "mythic.educationSheet.types.piloting",
  engineer: "mythic.educationSheet.types.engineer",
  space: "mythic.educationSheet.types.space",
  cultural: "mythic.educationSheet.types.cultural",
  computer: "mythic.educationSheet.types.computer",
  biological: "mythic.educationSheet.types.biological",
  military: "mythic.educationSheet.types.military",
  restricted: "mythic.educationSheet.types.restricted",
  dimensional: "mythic.educationSheet.types.dimensional"
}

mythic.skills = {
  appeal: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.social",
    characteristics: ["CH"]
  },
  athletics: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.move",
    characteristics: ["AGI", "STR"]
  },
  camouflage: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT", "PER"]
  },
  command: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.social",
    characteristics: ["LD"]
  },
  cryptography: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT"]
  },
  deception: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.social",
    characteristics: ["CH"]
  },
  demolition: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT"]
  },
  evasion: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.move",
    characteristics: ["AGI"]
  },
  gambling: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.social",
    characteristics: ["INT", "CH"]
  },
  interrogation: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.social",
    characteristics: ["CH", "LD", "INT"]
  },
  intimidation: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.social",
    characteristics: ["STR", "CH", "LD", "INT"]
  },
  investigation: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.social",
    characteristics: ["INT", "PER"]
  },
  medHuman: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT"]
  },
  medCovenant: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT"]
  },
  medMgalekgolo: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT"]
  },
  navGroundAir: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT", "PER"]
  },
  navSpace: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT", "PER"]
  },
  navSlipspace: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT", "PER"]
  },
  negotiation: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.social",
    characteristics: ["CH"]
  },
  pilotGround: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.field",
    characteristics: ["AGI", "INT"]
  },
  pilotAir: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.field",
    characteristics: ["AGI", "INT"]
  },
  pilotSpace: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.field",
    characteristics: ["AGI", "INT"]
  },
  security: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT"]
  },
  stunting: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.move",
    characteristics: ["AGI"]
  },
  survival: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT", "PER"]
  },
  techHuman: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT"]
  },
  techCovenant: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT"]
  },
  techForerunner: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: ["INT"]
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
  monumental: "mythic.sizes.monumental"
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