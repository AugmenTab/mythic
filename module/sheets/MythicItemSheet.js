/** 
 * Class representing the unique features of this system's ItemSheet.
 * @extends ItemSheet
 */
export default class MythicItemSheet extends ItemSheet {

  /** 
   * Establish default size and class options for the ItemSheet.
   * @returns {object} The original source object including updated, inserted, or overwritten records.
   */
  static get defaultOptions() {
    return mergeObject(super.defaultOptions, {
      classes: ["mythic", "sheet", "item"],
      height: 620,
      width: 575,
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
   * @returns {string} The path to the Handlebars template.
   */
  get template(){
    return `systems/mythic/templates/sheets/${this.item.data.type}-sheet.hbs`;
  }

  /** 
   * Prepares the ItemData.
   * @returns {ItemData} The prepared ItemData.
   */
  getData() {
    const data = super.getData();
    data.config = CONFIG.mythic;
    return data;
  }

  /**
   * Establishes event listeners on the ItemSheet.
   * @param {jQuery.fn} html - The HTML hook.
   */
  activateListeners(html) {
    super.activateListeners(html);

    html.find("textarea").hover(this._onUpdateTextareaContent.bind(this));
  }

  /**
   * Increases the height of a textarea on mouseOver.
   * @param {Event} event - The triggering event.
   */
  _onUpdateTextareaContent(event) {
    event.preventDefault();
    let element = event.currentTarget;
    const newHeight = element.scrollHeight + 2;
    const oldHeight = element.style.height.valueOf();
    element.style.height = `${newHeight > oldHeight ? newHeight : oldHeight}px`;
  }
};