/** @module MythicCombat */

import { calculateCharacteristicModifier } from "./calculations.js";

/**
 * Class representing the unique features of this system's Combat mechanics.
 * @extends Combat
 */
export default class MythicCombat extends Combat {

  /**
   * Sorts Combatants to determine their initiative order.
   * @param {Combatant} a - The first Combatant to sort.
   * @param {Combatant} b - The second Combatant to sort.
   * @returns {number} The result of the comparison between the two Combatants.
   */
  _sortCombatants(a, b) {
    const initA = Number.isNumeric(a.initiative) ? a.initiative : -9999;
    const initB = Number.isNumeric(b.initiative) ? b.initiative : -9999;
    const initDifference = initB - initA;
    if (initDifference !== 0) return initDifference;

    const agilityA = a.actor.data.data.characteristics.agi.roll;
    const agilityB = b.actor.data.data.characteristics.agi.roll;

    const agiModA = (
      calculateCharacteristicModifier(agilityA) + 
      a.actor.data.data.mythicCharacteristics.agi.total
    );
    const agiModB = (
      calculateCharacteristicModifier(agilityB) + 
      b.actor.data.data.mythicCharacteristics.agi.total
    );
    const modDifference = agiModB - agiModA;
    if (modDifference !== 0) return modDifference;

    return agilityB - agilityA;
  }

  /**
   * Advances the Combat to the next Turn.
   * @returns {Promise.<Combat>} The updated Combat with the Turn advanced.
   * @override
   */
  async nextTurn() {
    const missing = this.combatants.filter(c => c.initiative === null);
    if (missing.length > 0) {
      missing.forEach(c => ui.notifications.error(
        game.i18n.format("mythic.combat.error.missingInit"),
        { token: c.token.name }
      ));
      return this;
    }
  }

  /**
   * Advances the Combat to the next Round.
   * @returns {Promise.<Combat>} The updated Combat with the Round advanced.
   * @override
   */
  async nextRound() {
    return this.update({ advanceTime: CONFIG.time.roundTime });
  }
}
