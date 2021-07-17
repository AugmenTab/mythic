export default class MythicItemSheet extends ItemSheet {

  static get defaultOptions() {
    return mergeObject(super.defaultOptions, {
      width: 575,
      height: 620,
      classes: ["mythic", "sheet", "item"]
    });
  }

  get template(){
    return `systems/mythic/templates/sheets/${this.item.data.type}-sheet.hbs`;
  }

  getData() {
    const data = super.getData();
    data.config = CONFIG.mythic;
    return data;
  }

  activateListeners(html) {
    super.activateListeners(html);

    html.find("textarea").hover(this._onUpdateTextareaContent.bind(this));
  }

  _onUpdateTextareaContent(event) {
    event.preventDefault();
    let element = event.currentTarget;
    const newHeight = element.scrollHeight + 2;
    const oldHeight = element.style.height.valueOf();
    element.style.height = `${newHeight > oldHeight ? newHeight : oldHeight}px`;
  }
};