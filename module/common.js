/** @module common */

/**
 * Get the characteristic modifier for a given characteristic score.
 *
 * @param {number} score - The characteristic score.
 * @returns {number} The characteristic modifier.
 */
export function getCharacteristicModifier(score) {
  return score < 0 ? 0 : Math.floor(score / 10);
}

/**
 * Fetches a localization string.
 *
 * @param {string} path - The path to the localization text.
 * @returns {string} The localized text.
 */
export function localize(path) {
  return game.i18n.localize(path);
}

/**
 * Show an error in the Foundry UI.
 *
 * @param {string} path - The path to the error message localization text.
 */
export function makeUIError(path) {
  ui.notifications.error(localize(path));
}

/**
 * Show a warning in the Foundry UI.
 *
 * @param {string} path - The path to the warning message localization text.
 */
export function makeUIWarning(path) {
  ui.notifications.warn(localize(path));
}

/**
 * Partitions an array based on a provided predicate function.
 * @param {function} pred - The predicate function to check.
 * @param {object} arr - The array to partition.
 * @returns {object} - An object with two arrays: results that failed the
 * predicate and results that passed the predicate.
 */
export function partitionArray(pred, arr) {
  let no = [], yes = [];
  arr.forEach(x => (pred(x) ? yes : no).push(x));
  return { no, yes };
}
