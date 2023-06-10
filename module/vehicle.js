/** @module vehicle */

import * as Common from "./common.js";

// This defines the default values for a vehicle's propulsion when it's in
// perfect working order. This is a multiplier of x1 to all movement stats or
// characteristics, and no to-hit penalty.
const DEFAULT_PROPULSION = { multiplier: 1, toHit: 0 };

// This defines the default values for a vehicle's propulsion when it's in been
// disabled completely. This is a multiplier of x0 to all movement stats or
// characteristics, and no to-hit penalty (only walkers receive one, so their
// default disabled stats are stated explicitly).
const DEFAULT_DISABLED = { multiplier: 0, toHit: 0 };

/**
 * Calculates Vehicle movement values.
 *
 * @param {Actor} veh - The Vehicle Actor.
 */
export function calculateVehicleMovement(veh) {
  switch(veh.system.propulsion.type) {
    case "legs":      calculateWalkerMovement(veh);
    case "thrusters": calculateMovement(veh, [ "accelerate", "brake", "speed" ]);
    case "treads":    calculateMovement(veh, [ "accelerate", "speed" ]);
    case "wheels":    calculateMovement(veh, [ "accelerate", "speed" ]);
  }
}

/**
 * Provides vehicle doom state data based on its current hull breakpoints.
 *
 * @param {number} hull - The vehicle's current hull breakpoints.
 * @returns {object} The vehicle doom state data.
 */
export function getDoom(hull) {
  const bp = hull * -1;

  if (bp >= 100) return { level: "tier_9", armor: 5, blast: 4, kill: 3, move: false };
  if (bp >=  81) return { level: "tier_8", armor: 5, blast: 3, kill: 2, move: false };
  if (bp >=  66) return { level: "tier_7", armor: 5, blast: 2, kill: 1, move: false };
  if (bp >=  51) return { level: "tier_6", armor: 5, blast: 0, kill: 0, move: false };
  if (bp >=  41) return { level: "tier_5", armor: 4, blast: 0, kill: 0, move: false };
  if (bp >=  31) return { level: "tier_4", armor: 3, blast: 0, kill: 0, move: true  };
  if (bp >=  21) return { level: "tier_3", armor: 2, blast: 0, kill: 0, move: true  };
  if (bp >=  11) return { level: "tier_2", armor: 1, blast: 0, kill: 0, move: true  };
  if (bp >=   0) return { level: "tier_1", armor: 0, blast: 0, kill: 0, move: true  };

  return { level: "tier_0", armor: 0, blast: 0, kill: 0, move: true };
}

/**
 * Provides details around a Vehicle doom detonation - the damage roll formula,
 * the pierce value of the explosion, and any applicable special rules.
 *
 * @param {Actor} veh - The Vehicle Actor.
 * @returns {object} The doom detonation details.
 */
export function getDoomDetails(veh) {
  const doom = veh.system.breakpoints.hull.doom;
  const critType = game.settings.get("mythic", "criticalHitResult");
  const sp = veh.system.sizePoints;
  const wp = veh.system.weaponPoints;

  const formula = (
      `${sp + wp}D10`
    + (critType !== "special" ? `${critType}>=10` : "")
    + ` + ${5 * sp}`
  );

  const specials = {
    blast: { has: doom.blast > 0, value: doom.blast * sp },
    kill:  { has: doom.kill  > 0, value: doom.kill  * sp }
  };

  return { formula: formula, pierce: wp * 5, specials: specials };
}

/**
 * Determines the state of the vehicle's propulsion - that is, its movement
 * multiplier and to-hit penalty based on a comparison of its intact propulsion
 * devices to its total propulsion devices.
 *
 * @param {object} propulsion - The Vehicle Actor's `propulsion` value.
 * @returns {object} The current vehicle propulsion state.
 */
export function getPropulsion(propulsion) {
  switch(propulsion.type) {
    case "legs":      return getLegsState(propulsion);
    case "thrusters": return getThrustersState(propulsion);
    case "treads":    return getTreadsState(propulsion);
    case "wheels":    return getWheelsState(propulsion);
  }
}

/**
 * Finds the role owner for a given role assignment - that is, the abbreviation
 * for a role followed by the Actor ID, separated by an underscore ('_').
 *
 * @param {string} assignment - The assignment string.
 * @return {Actor|null} The current Actor assigned to the role, if applicable.
 */
export function getRoleOwner(assignment) {
  const actorId = assignment.split("_")[1];
  const actor = game.actors.get(actorId);

  if (!actor && !actorId) {
    return null;
  } else if (!actor) {
    Common.makeUIError("mythic.chat.error.unknownActor");
    return null;
  } else {
    return actor;
  }
}

/**
 * Provides details around a Vehicle trample attack - the relevant Evasion
 * penalty and the damage roll formula.
 *
 * @param {Actor} veh - The Vehicle Actor.
 * @returns {object} The trample details.
 */
export function getTrampleDetails(veh) {
  return {
    evasionPenalty: "+20 / 0",
    formula: `${Math.floor(veh.system.dimensions.weight)}`
  };
}

/**
 * Provides details around a Vehicle wreck - any applicable penalty to Evasion
 * tests, the damage roll formula, and any applicable special rules.
 *
 * @param {Actor} veh - The Vehicle Actor.
 * @param {string} atkType - The type of attack: doom, splatter, step, or wreck.
 * @returns {object} The wreck details.
 */
export function getWreckDetails(veh, atkType) {
  const dice = Math.floor(veh.system.movement.speed.current / 20);
  const critType = game.settings.get("mythic", "criticalHitResult");
  const formula =
    `${dice}D10` + (critType !== "special" ? `${critType}>=10` : "");

  return {
    evasionPenalty: atkType === "splatter" ? dice * -5 : 0,
    formula: formula,
    specials: []
  };
}

/**
 * Updates all Vehicle Actors that list the provided Actor ID as assigned to one
 * of its onboard roles.
 *
 * @param {string} actorId - The ID of the Actor.
 */
export function updateDependentVehicles(actorId) {
  game.actors.filter(v => isOnBoard(actorId, v)).map(vehicle => {
    vehicle.prepareData();
    vehicle.render();
  });
}

function calculateManeuver(veh) {
  const op = getRoleOwner(veh.system.movement.maneuver.owner);
  const immobile = [
    !op,
    !veh.system.breakpoints.hull.doom.move,
    veh.system.movement.speed.current === 0,
    veh.system.movement.speed.max === 0
  ].some(Boolean);

  if (immobile) {
    veh.system.movement.maneuver.total = 0;
    return;
  }

  let mod = 0;
  if (op.system.skills.stunting.training.tier === "trained") mod =  5;
  if (op.system.skills.stunting.training.tier === "plus10")  mod = 10;
  if (op.system.skills.stunting.training.tier === "plus20")  mod = 20;

  veh.system.movement.maneuver.total =
    Math.min(veh.system.movement.maneuver.base + mod, op.system.skills.evasion.roll);
}

function calculateMovement(veh, penalized) {
  Object.entries(veh.system.movement).splice(0, 3).forEach(([ key, val ]) => {
    if (veh.system.breakpoints.hull.doom.move) {
      const mult =
        penalized.includes(key) ? veh.system.propulsion.state.multiplier : 1;

      val.max = val.base * mult;
    } else {
      val.max = 0;
      val.current = 0;
    }
  });

  calculateManeuver(veh);
}

function calculateSkillValue(stat, training, modifier) {
  const mods = {
    "none": -20,
    "trained": 0,
    "plus10": 10,
    "plus20": 20
  };

  const target = stat + modifier + (mods[training] || 0);
  return target > 0 ? target : 0;
}

function calculateWalkerEvasion(veh) {
  const op = getRoleOwner(veh.system.movement.walker.owner);
  const immobile = [
    !op,
    !veh.system.breakpoints.hull.doom.move,
    veh.system.movement.walker.half === 0,
  ].some(Boolean);

  if (immobile) {
    veh.system.movement.walker.evasion = 0;
    return;
  }

  const training = op.system.skills.evasion.training.tier;
  const mods = op.system.skills.evasion.mods;
  const agi =
    Math.min(op.system.characteristics.agi.roll, veh.system.characteristics.agi);

  veh.system.movement.walker.evasion = calculateSkillValue(agi, training, mods);
}

function calculateWalkerParry(veh) {
  const op = getRoleOwner(veh.system.movement.walker.owner);
  const immobile = [
    !op,
    !veh.system.breakpoints.hull.doom.move,
    veh.system.movement.walker.half === 0,
  ].some(Boolean);

  if (immobile) {
    veh.system.movement.walker.parry = 0;
    return;
  }

  const training = op.system.skills.evasion.training.tier;
  const wfm = op.system.characteristics.wfm.roll;
  const stat = op.system.trainings.weapons.hth ? wfm + 5 : wfm;
  const mods = op.system.skills.evasion.mods;

  veh.system.movement.walker.parry = calculateSkillValue(stat, training, mods);
}

function calculateWalkerMovement(veh) {
  const mult = veh.system.propulsion.state.multiplier;
  const agi = Common.getCharacteristicModifier(veh.system.characteristics.agi);
  const base = agi + veh.system.characteristics.mythicAgi;
  const half =
    veh.system.breakpoints.hull.doom.move
      ? Math.max(0, Math.floor(base * mult))
      : 0;

  veh.system.movement.walker.half = half;
  veh.system.movement.walker.full = half * 2;
  veh.system.movement.walker.charge = half * 3;
  veh.system.movement.walker.run = half * 6;

  calculateWalkerEvasion(veh);
  calculateWalkerParry(veh);
}

function getLegsState(propulsion) {
  const legs = propulsion.current;

  function fromMultiplier(x) {
    return { multiplier: normalizeFloat(1 - x), toHit: 0 };
  }

  switch(propulsion.max) {
    case "2":
      if (legs >= 2) return DEFAULT_PROPULSION;
      if (legs >= 1) return DEFAULT_DISABLED;
      return { multiplier: 0, toHit: -20 };

    case "4":
      if (legs >= 4) return DEFAULT_PROPULSION;
      if (legs >= 3) return fromMultiplier(0.50);
      if (legs >= 2) return fromMultiplier(0.75);
      if (legs >= 1) return DEFAULT_DISABLED;
      return { multiplier: 0, toHit: -20 };

    case "6":
      if (legs >= 6) return DEFAULT_PROPULSION;
      if (legs >= 5) return fromMultiplier(0.25);
      if (legs >= 4) return fromMultiplier(0.40);
      if (legs >= 3) return fromMultiplier(0.60);
      if (legs >= 2) return fromMultiplier(0.80);
      if (legs >= 1) return DEFAULT_DISABLED;
      return { multiplier: 0, toHit: -20 };
  }

  return DEFAULT_DISABLED;
}

function getThrustersState(propulsion) {
  const thrusters = propulsion.max;
  const intact = propulsion.current;
  const disabled = thrusters - intact;

  if (intact >= thrusters) return DEFAULT_PROPULSION;

  function fromMultiplier(x) {
    return {
      multiplier: normalizeFloat(Math.min(1, Math.max(1 - (disabled * x), 0))),
      toHit: 0
    };
  }

  if (thrusters >= 20) return fromMultiplier(0.20);
  if (thrusters >= 18) return fromMultiplier(0.25);
  if (thrusters >= 16) return fromMultiplier(0.30);
  if (thrusters >= 15) return fromMultiplier(0.35);
  if (thrusters >= 14) return fromMultiplier(0.40);
  if (thrusters >=  1) return fromMultiplier(1 - (0.05 * (thrusters - 1)));
  return DEFAULT_DISABLED;
}

function getTreadsState(propulsion) {
  const treads = propulsion.current;

  function fromMultiplier(x) {
    return { multiplier: normalizeFloat(1 - x), toHit: 0 };
  }

  switch(propulsion.max) {
    case "2":
      if (treads >= 2) return DEFAULT_PROPULSION;
      if (treads >= 1) return fromMultiplier(0.7);
      return DEFAULT_DISABLED;

    case "4":
      if (treads >= 4) return DEFAULT_PROPULSION;
      if (treads >= 3) return fromMultiplier(0.2);
      if (treads >= 2) return fromMultiplier(0.4);
      if (treads >= 1) return fromMultiplier(0.8);
      return DEFAULT_DISABLED;

    case "6":
      if (treads >= 6) return DEFAULT_PROPULSION;
      if (treads >= 5) return fromMultiplier(0.10);
      if (treads >= 4) return fromMultiplier(0.25);
      if (treads >= 3) return fromMultiplier(0.40);
      if (treads >= 2) return fromMultiplier(0.75);
      if (treads >= 1) return fromMultiplier(0.95);
      return DEFAULT_DISABLED;

    case "8":
      if (treads >= 8) return DEFAULT_PROPULSION;
      if (treads >= 7) return fromMultiplier(0.10);
      if (treads >= 6) return fromMultiplier(0.20);
      if (treads >= 5) return fromMultiplier(0.30);
      if (treads >= 4) return fromMultiplier(0.45);
      if (treads >= 3) return fromMultiplier(0.60);
      if (treads >= 2) return fromMultiplier(0.85);
      if (treads >= 1) return fromMultiplier(0.99);
      return DEFAULT_DISABLED;
  }

  return DEFAULT_DISABLED;
}

function getWheelsState(propulsion) {
  const wheels = propulsion.current;

  function fromMultiplier(x) {
    return { multiplier: normalizeFloat(1 - x), toHit: 0 };
  }

  switch(propulsion.max) {
    case "3":
      if (wheels >= 3) return DEFAULT_PROPULSION;
      if (wheels >= 2) return fromMultiplier(0.5);
      if (wheels >= 1) return fromMultiplier(0.8);
      return DEFAULT_DISABLED;

    case "4":
      if (wheels >= 4) return DEFAULT_PROPULSION;
      if (wheels >= 3) return fromMultiplier(0.3);
      if (wheels >= 2) return fromMultiplier(0.6);
      if (wheels >= 1) return fromMultiplier(0.9);
      return DEFAULT_DISABLED;

    case "6":
      if (wheels >= 6) return DEFAULT_PROPULSION;
      if (wheels >= 5) return fromMultiplier(0.15);
      if (wheels >= 4) return fromMultiplier(0.30);
      if (wheels >= 3) return fromMultiplier(0.50);
      if (wheels >= 2) return fromMultiplier(0.75);
      if (wheels >= 1) return fromMultiplier(0.95);
      return DEFAULT_DISABLED;

    case "8":
      if (wheels >= 8) return DEFAULT_PROPULSION;
      if (wheels >= 7) return fromMultiplier(0.10);
      if (wheels >= 6) return fromMultiplier(0.20);
      if (wheels >= 5) return fromMultiplier(0.40);
      if (wheels >= 4) return fromMultiplier(0.55);
      if (wheels >= 3) return fromMultiplier(0.75);
      if (wheels >= 2) return fromMultiplier(0.90);
      if (wheels >= 1) return fromMultiplier(0.95);
      return DEFAULT_DISABLED;
  }

  return DEFAULT_DISABLED;
}

function isOnBoard(actorId, veh) {
  if (veh.type !== "Vehicle") return false;

  return new Array(
    veh.system.crew.operators,
    veh.system.crew.gunners,
    veh.system.crew.complement
  ).flat().some(crew => crew.id === actorId);
}

function normalizeFloat(x) {
  return parseFloat(x.toFixed(2));
}
