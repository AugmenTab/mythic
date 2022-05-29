/** @module common */

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
  ui.notifications.warning(localize(path));
}
