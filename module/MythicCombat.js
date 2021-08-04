export default class MythicCombat extends Combat {
  _sortCombatants(a, b) {
    const initA = Number.isNumeric(a.initiative) ? a.initiative : -9999;
    const initB = Number.isNumeric(b.initiative) ? b.initiative : -9999;
    const difference = initB - initA;
    if (difference !== 0) return difference;

    const agiA = (
      a.actor.data.data.characteristics.agi.roll + 
      a.actor.data.data.mythicCharacteristics.agi.total
    );
    const agiB = (
      b.actor.data.data.characteristics.agi.roll + 
      b.actor.data.data.mythicCharacteristics.agi.total
    );
    return agiB - agiA;
  }

  async nextTurn() {
    const missing = this.combatants.filter(c => c.initiative === null);
    if (missing.length > 0) {
      missing.forEach(c => ui.notifications.error(
        game.i18n.format("mythic."),
        { token: c.token.name }
      ));
      return this;
    }
  }

  async nextRound() {
    return this.update({ advanceTime: CONFIG.time.roundTime });
  }
}
