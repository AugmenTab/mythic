/** @module MythicItemSheet */

import { getPostableItemFlavorPath, postChatMessage } from "../chat.js";
import { localize } from "../common.js";

/**
 * Class representing the unique features of this system's ItemSheet.
 *
 * @extends ItemSheet
 */
export default class MythicItemSheet extends ItemSheet {

  /**
   * Establish default size and class options for the ItemSheet.
   *
   * @override
   * @returns {object} The original source object including updated, inserted,
   * or overwritten records.
   */
  static get defaultOptions() {
    return foundry.utils.mergeObject(super.defaultOptions, {
      classes: ["mythic", "sheet", "item"],
      height: 635,
      width: 585,
      tabs: [
        {
          navSelector: ".item-tabs",
          contentSelector: ".item-sheet-body",
          initial: "basic"
        }
      ]
    });
  }

  /**
   * Get the Handlebars template for the ItemSheet.
   *
   * @override
   * @returns {string} The path to the Handlebars template.
   */
  get template(){
    return `systems/mythic/templates/sheets/${this.item.type}-sheet.hbs`;
  }

  /**
   * Prepares the ItemData.
   *
   * @override
   * @returns {ItemData} The prepared ItemData.
   */
  getData() {
    const data = super.getData();

    data.system = data.item.system;
    data.config = CONFIG.mythic;

    return data;
  }

  /**
   * Establishes event listeners on the ItemSheet.
   *
   * @override
   * @param {jQuery.fn} html - The HTML hook.
   */
  activateListeners(html) {
    super.activateListeners(html);

    html.find(".postable-item-sheet").click(this._onPostItem.bind(this));
  }

  async _onPostItem(event) {
    event.preventDefault();
    const element = event.currentTarget;

    await postChatMessage({
      flavor: localize(getPostableItemFlavorPath(this.item)),
      template: `postable-${this.item.type}`,
      ...this.item
    }, this.item.actor);
  }
};
