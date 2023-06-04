/** @module vehicle */

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
}

function getThrustersState(propulsion) {
  const thrusters = propulsion.max;
  const disabled = propulsion.max - propulsion.current;

  if (disabled <= 0) return DEFAULT_PROPULSION;

  function fromMultiplier(x) {
    return {
      multiplier: normalizeFloat(Math.max(1 - (disabled * x), 0)),
      toHit: 0
    };
  }

  if (thrusters >= 20) return fromMultiplier(0.20);
  if (thrusters >= 18) return fromMultiplier(0.25);
  if (thrusters >= 16) return fromMultiplier(0.30);
  if (thrusters >= 15) return fromMultiplier(0.35);
  if (thrusters >= 14) return fromMultiplier(0.40);
  if (thrusters >=  1) return fromMultiplier(0.05 * (thrusters - 1));
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
}

function normalizeFloat(x) {
  return parseFloat(x.toFixed(2));
}
