## Common

A module of commonly-used helper functions.

See the appropriate localization in [../lang](lang) for help with localization paths.

```javascript
/**
 * Get the characteristic modifier for a given characteristic score.
 *
 * @param {number} score - The characteristic score.
 * @returns {number} The characteristic modifier.
 */
function getCharacteristicModifier(score) { .. }

/**
 * Fetches a localization string.
 *
 * @param {string} path - The path to the localization text.
 * @returns {string} The localized text.
 */
function localize(path) { .. }

/**
 * Show an error in the Foundry UI.
 *
 * @param {string} path - The path to the error message localization text.
 */
function makeUIError(path) { .. }

/**
 * Show a warning in the Foundry UI.
 *
 * @param {string} path - The path to the warning message localization text.
 */
function makeUIWarning(path) { .. }

/**
 * Partitions an array based on a provided predicate function.
 * @param {function} pred - The predicate function to check.
 * @param {Array} arr - The array to partition.
 * @returns {{no: Array, yes: Array}} - An object with two arrays: results that
 * failed the predicate and results that passed the predicate.
 */
function partitionArray(pred, arr) { .. }
```
