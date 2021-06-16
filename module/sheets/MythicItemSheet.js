export default class MythicItemSheet extends ItemSheet {
  get template(){
    return `systems/mythic/templates/sheets/${this.item.data.type}-sheet.html`;
  }

  getData() {
    const data = super.getData();
    data.config = CONFIG.mythic;
    return data;
  }
};