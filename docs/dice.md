## Dice

A module for rolling attacks, tests, and otherwise handling dice.

```javascript
/**
 * Evaluates a string of simple addition and subtraction expressions. This is
 * capable of handling dice as part of the calculation.
 *
 * @param {string} str - The string representation of the expression.
 * @returns {number} The number value of the evaluated expression.
 */
function interpretDiceRollModifiers(str) { .. }

/**
 * Roll attacks. Note that this does not manage the weapon's magazine after the
 * attack - this will have to be done by the caller after the attack is
 * complete.
 *
 * @async
 * @param {{value: string, innerHTML: number}} details - Details regarding the
 * attack being made. `value` should be one <single | half | full>, while
 * `innerHTML` is the number of attacks being made.
 * @param {Actor} actor - The Actor making the Attack.
 */
async function rollAttacks(details, actor, weapon) { .. }

/**
 * Roll a number of Evasion tests from an Actor sheet.
 *
 * @async
 * @param {{value: number}} details - Details regarding the evasions being
 * rolled. `value` should be the target for the first Evasion Test being rolled
 * as part of the batch.
 * @param {Actor} actor - The Actor making the Evasion Tests.
 */
async function rollEvasionBatch(details, actor) {

/**
 * Roll a special Vehicle attack from a Vehicle Actor sheet.
 *
 * @async
 * @param {Actor} veh - The Actor Vehicle making the Attack.
 * @param {string} atkType - The type of special Vehicle attack being made. It
 * should be one of <doom | splatter | trample | wreck>.
 */
async function rollVehicleAttack(veh, atkType) { .. }
```
