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
  cryosprayer: "mythic.ammoGroups.cryosprayer",
  sniper: "mythic.ammoGroups.sniper",
  grenade: "mythic.ammoGroups.grenade",
  mrc: "mythic.ammoGroups.mrc",
  bruteShot: "mythic.ammoGroups.bruteShot",
}

mythic.trainings = {
  equipment: {
    basic: "mythic.trainings.equipment.basic",
    infantry: "mythic.trainings.equipment.infantry",
    heavy: "mythic.trainings.equipment.heavy",
    advanced: "mythic.trainings.equipment.advanced",
    launcher: "mythic.trainings.equipment.launcher",
    range: "mythic.trainings.equipment.range",
    ordnance: "mythic.trainings.equipment.ordnance",
    cannon: "mythic.trainings.equipment.cannon",
    melee: "mythic.trainings.equipment.melee"
  },
  examples: {
    basic: "mythic.trainings.examples.basic",
    infantry: "mythic.trainings.examples.infantry",
    heavy: "mythic.trainings.examples.heavy",
    advanced: "mythic.trainings.examples.advanced",
    launcher: "mythic.trainings.examples.launcher",
    range: "mythic.trainings.examples.range",
    ordnance: "mythic.trainings.examples.ordnance",
    cannon: "mythic.trainings.examples.cannon",
    melee: "mythic.trainings.examples.melee"
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
    characteristics: {
      CHA: "CHA"
    },
    floodCharacteristics: {}
  },
  athletics: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.move",
    characteristics: {
      AGI: "AGI",
      STR: "STR"
    },
    floodCharacteristics: {
      AGI: "AGI",
      STR: "STR"
    }
  },
  camouflage: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.field",
    characteristics: {
      INT: "INT",
      PER: "PER"
    },
    floodCharacteristics: {
      INT: "INT",
      PER: "PER"
    },
  },
  command: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.social",
    characteristics: {
      LDR: "LDR"
    },
    floodCharacteristics: {}
  },
  cryptography: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: {
      INT: "INT"
    },
    floodCharacteristics: {
      INT: "INT"
    }
  },
  deception: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.social",
    characteristics: {
      CHA: "CHA",
      LDR: "LDR"
    },
    floodCharacteristics: {}
  },
  demolition: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: {
      INT: "INT"
    },
    floodCharacteristics: {
      INT: "INT"
    }
  },
  evasion: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.move",
    characteristics: {
      AGI: "AGI",
      WFM: "WFM"
    },
    floodCharacteristics: {
      AGI: "AGI",
      WFM: "WFM"
    }
  },
  gambling: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.social",
    characteristics: {
      INT: "INT",
      CHA: "CHA"
    },
    floodCharacteristics: {}
  },
  interrogation: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.social",
    characteristics: {
      CHA: "CHA",
      LDR: "LDR",
      INT: "INT"
    },
    floodCharacteristics: {}
  },
  intimidation: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.social",
    characteristics: {
      STR: "STR",
      CHA: "CHA",
      LDR: "LDR",
      INT: "INT"
    },
    floodCharacteristics: {
      STR: "STR",
      INT: "INT"
    }
  },
  investigation: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.social",
    characteristics: {
      INT: "INT",
      PER: "PER"
    },
    floodCharacteristics: {
      INT: "INT",
      PER: "PER"
    }
  },
  medHuman: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: {
      INT: "INT"
    },
    floodCharacteristics: {}
  },
  medCovenant: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: {
      INT: "INT"
    },
    floodCharacteristics: {}
  },
  medXenophile: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: {
      INT: "INT"
    },
    floodCharacteristics: {}
  },
  navGroundAir: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.field",
    characteristics: {
      INT: "INT",
      PER: "PER"
    },
    floodCharacteristics: {
      INT: "INT",
      PER: "PER"
    }
  },
  navSpace: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.field",
    characteristics: {
      INT: "INT",
      PER: "PER"
    },
    floodCharacteristics: {
      INT: "INT",
      PER: "PER"
    }
  },
  negotiation: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.social",
    characteristics: {
      CHA: "CHA"
    },
    floodCharacteristics: {}
  },
  pilotGround: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.field",
    characteristics: {
      AGI: "AGI",
      INT: "INT"
    },
    floodCharacteristics: {
      AGI: "AGI",
      INT: "INT"
    }
  },
  pilotAir: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.field",
    characteristics: {
      AGI: "AGI",
      INT: "INT"
    },
    floodCharacteristics: {
      AGI: "AGI",
      INT: "INT"
    }
  },
  pilotSpace: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.field",
    characteristics: {
      AGI: "AGI",
      INT: "INT"
    },
    floodCharacteristics: {
      AGI: "AGI",
      INT: "INT"
    }
  },
  security: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: {
      INT: "INT"
    },
    floodCharacteristics: {
      INT: "INT"
    }
  },
  stunting: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.move",
    characteristics: {
      AGI: "AGI"
    },
    floodCharacteristics: {
      AGI: "AGI"
    },
  },
  survival: {
    difficulty: "mythic.skillSheet.difficulties.basic",
    type: "mythic.skillSheet.types.field",
    characteristics: {
      INT: "INT",
      PER: "PER"
    },
    floodCharacteristics: {
      INT: "INT",
      PER: "PER"
    }
  },
  techHuman: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: {
      INT: "INT"
    },
    floodCharacteristics: {
      INT: "INT"
    }
  },
  techCovenant: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: {
      INT: "INT"
    },
    floodCharacteristics: {
      INT: "INT"
    }
  },
  techForerunner: {
    difficulty: "mythic.skillSheet.difficulties.adv",
    type: "mythic.skillSheet.types.field",
    characteristics: {
      INT: "INT"
    },
    floodCharacteristics: {
      INT: "INT"
    }
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
  cr: "CRG",
  ch: "CHA",
  ld: "LDR"
}

mythic.weaponSettings = {
  firearmType: {
    firearms: "mythic.weaponSheet.settings.firearmType.firearms",
    cannons: "mythic.weaponSheet.settings.firearmType.cannons",
    shotguns: "mythic.weaponSheet.settings.firearmType.shotguns"
  },
  firearmTypeDescriptions: {
    firearms: "mythic.weaponSheet.settings.firearmTypeDescriptions.firearms",
    cannons: "mythic.weaponSheet.settings.firearmTypeDescriptions.cannons",
    shotguns: "mythic.weaponSheet.settings.firearmTypeDescriptions.shotguns"
  },
  barrel: {
    xs: "mythic.weaponSheet.settings.barrel.xs",
    s: "mythic.weaponSheet.settings.barrel.s",
    m: "mythic.weaponSheet.settings.barrel.m",
    l: "mythic.weaponSheet.settings.barrel.l",
    xl: "mythic.weaponSheet.settings.barrel.xl",
    xxl: "mythic.weaponSheet.settings.barrel.xxl",
  },
  barrelDescriptions: {
    xs: "mythic.weaponSheet.settings.barrelDescriptions.xs",
    s: "mythic.weaponSheet.settings.barrelDescriptions.s",
    m: "mythic.weaponSheet.settings.barrelDescriptions.m",
    l: "mythic.weaponSheet.settings.barrelDescriptions.l",
    xl: "mythic.weaponSheet.settings.barrelDescriptions.xl",
    xxl: "mythic.weaponSheet.settings.barrelDescriptions.xxl",
  }
}

mythic.specialRules = {
  weapons: {
    acid: "number",
    blast: "number",
    cauterize: "none",
    charge: "number",
    cryo: "text",
    diceMinimum: "number",
    electrified: "text",
    emp: "text",
    flame: "text",
    flashbang: "none",
    gravimetricPulse: "number",
    gravity: "number",
    hardlight: "none",
    headshot: "none",
    homing: "number",
    kill: "number",
    kinetic: "none",
    linked: "number",
    longBarrel: "none",
    needle: "number",
    nonlethal: "none",
    overheat: "number",
    penetrating: "none",
    rechargeRate: "number",
    singleLoading: "none",
    slow: "none",
    smoke: "number",
    spike: "none",
    spin: "number",
    spread: "none",
    sticky: "none",
    stun: "number",
    tearGas: "none",
    tranquilize: "number",
    vehicleLock: "none",
  },
  vehicles: {
    allTerrain: "none",
    antiGrav: "none",
    autoloader: "none",
    boost: "number",
    continuousTrack: "none",
    enclosedTop: "none",
    heavyPlating: "none",
    neuralInterface: "none",
    openTop: "none",
    slipspace: "none",
    walkerStomp: "none"
  }
}

mythic.armorHardpoints = {
  head: 3,
  chest: 6,
  arm: 2,
  leg: 3
}

mythic.vehicle = {
  armor: {
    front: "mythic.vehicleSheet.armor.front",
    back: "mythic.vehicleSheet.armor.back",
    side: "mythic.vehicleSheet.armor.side",
    top: "mythic.vehicleSheet.armor.top",
    bottom: "mythic.vehicleSheet.armor.bottom"
  },
  breakpoints: {
    wep: "mythic.vehicleSheet.breakpoints.wep",
    mob: "mythic.vehicleSheet.breakpoints.mob",
    eng: "mythic.vehicleSheet.breakpoints.eng",
    op: "mythic.vehicleSheet.breakpoints.op",
    hull: "mythic.vehicleSheet.breakpoints.hull"
  },
  mobility: {
    legs: {
      2: 2,
      4: 4,
      6: 6
    },
    treads: {
      2: 2,
      4: 4,
      6: 6,
      8: 8
    },
    wheels: {
      3: 3,
      4: 4,
      6: 6,
      8: 8
    }
  },
  propulsion: {
    none: "mythic.vehicleSheet.propulsion.none",
    legs: "mythic.vehicleSheet.propulsion.legs",
    thrusters: "mythic.vehicleSheet.propulsion.thrusters",
    treads: "mythic.vehicleSheet.propulsion.treads",
    wheels: "mythic.vehicleSheet.propulsion.wheels"
  },
  types: {
    ground: "mythic.vehicleSheet.types.ground",
    air: "mythic.vehicleSheet.types.air",
    space: "mythic.vehicleSheet.types.space"
  }
}

mythic.strAttack = {
  damage: {
    "0": "--",
    "0.5": "½ STR",
    "1": "STR",
    "2": "STR × 2"
  },
  piercing: {
    "0": "--",
    "0.5": "½ STR",
    "1": "STR"
  }
}
