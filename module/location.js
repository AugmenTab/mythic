/** @module location */

/**
 * Determines the hit location of a successful attack using the provided key
 * (the result of reversing the digits on the die from the attack roll) and
 * whether or not the target is a vehicle.
 * @param {number} key - The hit location key value.
 * @param {boolean} veh - Whether or not the attack target is a Vehicle.
 * @returns {string} The complete hit location to display in the chat.
 */
export async function determineHitLocation(key, veh) {
  return veh ? determineHitLocationVehicle(key) : determineHitLocationCreature(key);
}

async function determineHitDigit(root) {
  const roll = await new Roll("1D5").roll({ async: true });
  if (roll.total === 5) return game.i18n.localize(`${root}.digits.thumb`);
  if (roll.total === 4) return game.i18n.localize(`${root}.digits.index`);
  if (roll.total === 3) return game.i18n.localize(`${root}.digits.middle`);
  if (roll.total === 2) return game.i18n.localize(`${root}.digits.ring`);
  if (roll.total === 1) return game.i18n.localize(`${root}.digits.pinky`);
}

async function determineHitLocationCreature(key) {
  const root = "mythic.hitLocations.body";
  let location = "";
  if (key >= 61) {
    location = game.i18n.localize(`${root}.chest.location`);
  } else if (key >= 46) {
    location = game.i18n.localize(`${root}.leg.right`);
  } else if (key >= 31) {
    location = game.i18n.localize(`${root}.leg.left`);
  } else if (key >= 21) {
    location = game.i18n.localize(`${root}.arm.right`);
  } else if (key >= 11) {
    location = game.i18n.localize(`${root}.arm.left`);
  } else {
    location = game.i18n.localize(`${root}.head.location`);
  }
  const sublocation = await determineHitSublocation(key, root);
  return `${location} - ${sublocation}`;
}

async function determineHitLocationVehicle(key) {
  const root = "mythic.hitLocations.vehicle";
  if (key >= 81) {
    return game.i18n.localize(`${root}.crew`);
  } else if (key >= 61) {
    return game.i18n.localize(`${root}.op`);
  } else if (key >= 41) {
    return game.i18n.localize(`${root}.eng`);
  } else if (key >= 21) {
    return game.i18n.localize(`${root}.mob`);
  } else {
    return game.i18n.localize(`${root}.wep`);
  }
}

async function determineHitSide() {
  const roll = await new Roll("1D2").roll({ async: true });
  const root = "mythic.hitLocations.side";
  return game.i18n.localize(`${root}.${roll.total === 2 ? "right" : "left"}`);
}

async function determineHitSublocation(key, root) {
  // Chest
  if (key >= 97) return game.i18n.localize(`${root}.chest.noOrgan`);
  if (key >= 90) {
    const side = await determineHitSide();
    return `${side} ${game.i18n.localize(`${root}.chest.lung`)}`;
  }
  if (key >= 85) return game.i18n.localize(`${root}.chest.heart`);
  if (key >= 79) return game.i18n.localize(`${root}.chest.splanchnic`);
  if (key >= 73) {
    const side = await determineHitSide();
    return `${side} ${game.i18n.localize(`${root}.chest.kidney`)}`;
  }
  if (key >= 67) return game.i18n.localize(`${root}.chest.small`);
  if (key >= 61) return game.i18n.localize(`${root}.chest.large`);

  // Right Leg
  if (key >= 59) return game.i18n.localize(`${root}.leg.pelvis`);
  if (key >= 55) return game.i18n.localize(`${root}.leg.thigh`);
  if (key >= 54) return game.i18n.localize(`${root}.leg.knee`);
  if (key >= 49) return game.i18n.localize(`${root}.leg.shin`);
  if (key >= 48) return game.i18n.localize(`${root}.leg.ankle`);
  if (key >= 47) return game.i18n.localize(`${root}.leg.foot`);
  if (key >= 46) {
    const digit = await determineHitDigit(root);
    return `${digit} ${game.i18n.localize(`${root}.leg.toe`)}`;
  }

  // Left Leg
  if (key >= 44) return game.i18n.localize(`${root}.leg.pelvis`);
  if (key >= 39) return game.i18n.localize(`${root}.leg.thigh`);
  if (key >= 38) return game.i18n.localize(`${root}.leg.knee`);
  if (key >= 34) return game.i18n.localize(`${root}.leg.shin`);
  if (key >= 33) return game.i18n.localize(`${root}.leg.ankle`);
  if (key >= 32) return game.i18n.localize(`${root}.leg.foot`);
  if (key >= 31) {
    const digit = await determineHitDigit(root);
    return `${digit} ${game.i18n.localize(`${root}.leg.toe`)}`;
  }

  // Right Arm
  if (key >= 30) return game.i18n.localize(`${root}.arm.shoulder`);
  if (key >= 27) return game.i18n.localize(`${root}.arm.bicep`);
  if (key >= 26) return game.i18n.localize(`${root}.arm.elbow`);
  if (key >= 23) return game.i18n.localize(`${root}.arm.forearm`);
  if (key >= 22) return game.i18n.localize(`${root}.arm.hand`);
  if (key >= 21) {
    const digit = await determineHitDigit(root); 
    return `${digit} ${game.i18n.localize(`${root}.arm.finger`)}`;
  }

  // Left Arm
  if (key >= 20) return game.i18n.localize(`${root}.arm.shoulder`);
  if (key >= 17) return game.i18n.localize(`${root}.arm.bicep`);
  if (key >= 16) return game.i18n.localize(`${root}.arm.elbow`);
  if (key >= 13) return game.i18n.localize(`${root}.arm.forearm`);
  if (key >= 12) return game.i18n.localize(`${root}.arm.hand`);
  if (key >= 11) {
    const digit = await determineHitDigit(root)
    return `${digit} ${game.i18n.localize(`${root}.arm.finger`)}`;
  }

  // Head
  if (key >= 10) {
    const side = await determineHitSide();
    return `${side} ${game.i18n.localize(`${root}.head.ear`)}`;
  }
  if (key >= 9) return game.i18n.localize(`${root}.head.forehead`);
  if (key >= 8) {
    const side = await determineHitSide();
    return `${side} ${game.i18n.localize(`${root}.head.eye`)}`;
  }
  if (key >= 6) {
    const side = await determineHitSide();
    return `${side} ${game.i18n.localize(`${root}.head.cheek`)}`;
  }
  if (key >= 4) return game.i18n.localize(`${root}.head.nose`);
  if (key >= 3) return game.i18n.localize(`${root}.head.mouth`);
  if (key >= 2) return game.i18n.localize(`${root}.head.chin`);
  if (key >= 1) return game.i18n.localize(`${root}.head.neck`);
}