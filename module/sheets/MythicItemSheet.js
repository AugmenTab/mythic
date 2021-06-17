export default class MythicItemSheet extends ItemSheet {

  static get defaultOptions() {
    return mergeObject(super.defaultOptions, {
      width: 530,
      height: 340,
      classes: ["mythic", "sheet", "item"]
    });
  }

  get template(){
    return `systems/mythic/templates/sheets/${this.item.data.type}-sheet.html`;
  }

  getData() {
    const data = super.getData();
    data.config = CONFIG.mythic;
    return data;
  }

};