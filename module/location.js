/** @module location */

import { localize } from "./common.js";

/**
 * Determines the hit location of a successful attack using the provided key
 * (the result of reversing the digits on the die from the attack roll) and
 * whether or not the target is a vehicle.
 *
 * @async
 * @param {number} key - The hit location key value.
 * @param {boolean} veh - Whether or not the attack target is a Vehicle.
 * @returns {string} The complete hit location to display in the chat.
 */
export async function determineHitLocation(key, veh) {
  return veh ? determineHitLocationVehicle(key) : determineHitLocationCreature(key);
}

async function determineHitDigit(root) {
  const roll = await new Roll("1D5").roll({ async: true });
  if (roll.total === 5) return localize(`${root}.digits.thumb`);
  if (roll.total === 4) return localize(`${root}.digits.index`);
  if (roll.total === 3) return localize(`${root}.digits.middle`);
  if (roll.total === 2) return localize(`${root}.digits.ring`);
  if (roll.total === 1) return localize(`${root}.digits.pinky`);
}

async function determineHitLocationCreature(key) {
  const root = "mythic.hitLocations.body";
  let location = "";
  if (key >= 61) {
    location = localize(`${root}.chest.location`);
  } else if (key >= 46) {
    location = localize(`${root}.leg.right`);
  } else if (key >= 31) {
    location = localize(`${root}.leg.left`);
  } else if (key >= 21) {
    location = localize(`${root}.arm.right`);
  } else if (key >= 11) {
    location = localize(`${root}.arm.left`);
  } else {
    location = localize(`${root}.head.location`);
  }
  const sublocation = await determineHitSublocation(key, root);
  return `${location} - ${sublocation}`;
}

async function determineHitLocationVehicle(key) {
  const root = "mythic.hitLocations.vehicle";
  if (key >= 81) {
    return localize(`${root}.crew`);
  } else if (key >= 61) {
    return localize(`${root}.op`);
  } else if (key >= 41) {
    return localize(`${root}.eng`);
  } else if (key >= 21) {
    return localize(`${root}.mob`);
  } else {
    return localize(`${root}.wep`);
  }
}

async function determineHitSide() {
  const roll = await new Roll("1D2").roll({ async: true });
  const root = "mythic.hitLocations.side";
  return localize(`${root}.${roll.total === 2 ? "right" : "left"}`);
}

async function determineHitSublocation(key, root) {
  // Chest
  if (key >= 97) return localize(`${root}.chest.noOrgan`);
  if (key >= 90) {
    const side = await determineHitSide();
    return `${side} ${localize(`${root}.chest.lung`)}`;
  }
  if (key >= 85) return localize(`${root}.chest.heart`);
  if (key >= 79) return localize(`${root}.chest.splanchnic`);
  if (key >= 73) {
    const side = await determineHitSide();
    return `${side} ${localize(`${root}.chest.kidney`)}`;
  }
  if (key >= 67) return localize(`${root}.chest.small`);
  if (key >= 61) return localize(`${root}.chest.large`);

  // Right Leg
  if (key >= 59) return localize(`${root}.leg.pelvis`);
  if (key >= 55) return localize(`${root}.leg.thigh`);
  if (key >= 54) return localize(`${root}.leg.knee`);
  if (key >= 49) return localize(`${root}.leg.shin`);
  if (key >= 48) return localize(`${root}.leg.ankle`);
  if (key >= 47) return localize(`${root}.leg.foot`);
  if (key >= 46) {
    const digit = await determineHitDigit(root);
    return `${digit} ${game.i18n.localize(`${root}.leg.toe`)}`;
  }

  // Left Leg
  if (key >= 44) return localize(`${root}.leg.pelvis`);
  if (key >= 39) return localize(`${root}.leg.thigh`);
  if (key >= 38) return localize(`${root}.leg.knee`);
  if (key >= 34) return localize(`${root}.leg.shin`);
  if (key >= 33) return localize(`${root}.leg.ankle`);
  if (key >= 32) return localize(`${root}.leg.foot`);
  if (key >= 31) {
    const digit = await determineHitDigit(root);
    return `${digit} ${game.i18n.localize(`${root}.leg.toe`)}`;
  }

  // Right Arm
  if (key >= 30) return localize(`${root}.arm.shoulder`);
  if (key >= 27) return localize(`${root}.arm.bicep`);
  if (key >= 26) return localize(`${root}.arm.elbow`);
  if (key >= 23) return localize(`${root}.arm.forearm`);
  if (key >= 22) return localize(`${root}.arm.hand`);
  if (key >= 21) {
    const digit = await determineHitDigit(root);
    return `${digit} ${game.i18n.localize(`${root}.arm.finger`)}`;
  }

  // Left Arm
  if (key >= 20) return localize(`${root}.arm.shoulder`);
  if (key >= 17) return localize(`${root}.arm.bicep`);
  if (key >= 16) return localize(`${root}.arm.elbow`);
  if (key >= 13) return localize(`${root}.arm.forearm`);
  if (key >= 12) return localize(`${root}.arm.hand`);
  if (key >= 11) {
    const digit = await determineHitDigit(root)
    return `${digit} ${localize(`${root}.arm.finger`)}`;
  }

  // Head
  if (key >= 10) {
    const side = await determineHitSide();
    return `${side} ${localize(`${root}.head.ear`)}`;
  }
  if (key >= 9) return localize(`${root}.head.forehead`);
  if (key >= 8) {
    const side = await determineHitSide();
    return `${side} ${localize(`${root}.head.eye`)}`;
  }
  if (key >= 6) {
    const side = await determineHitSide();
    return `${side} ${localize(`${root}.head.cheek`)}`;
  }
  if (key >= 4) return localize(`${root}.head.nose`);
  if (key >= 3) return localize(`${root}.head.mouth`);
  if (key >= 2) return localize(`${root}.head.chin`);
  if (key >= 1) return localize(`${root}.head.neck`);
}
