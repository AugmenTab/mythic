/** @module MythicCombat */

/**
 * Class representing the unique features of this system's Combat mechanics.
 *
 * @extends Combat
 */
export default class MythicCombat extends Combat {

  /**
   * Sorts Combatants to determine their initiative order.
   *
   * @param {Combatant} a - The first Combatant to sort.
   * @param {Combatant} b - The second Combatant to sort.
   * @returns {number} The result of the comparison between the two Combatants.
   */
  _sortCombatants(a, b) {
    const getTieBreaker = actor => {
      return .001 * (
          actor.system.characteristics.agi.roll
        + (10 * actor.system.mythicCharacteristics.agi.total)
      );
    };

    const initA = Number.isNumeric(a.initiative) ? a.initiative : -9999;
    const initB = Number.isNumeric(b.initiative) ? b.initiative : -9999;
    return (initB + getTieBreaker(b.actor)) - (initA + getTieBreaker(a.actor));
  }
}
