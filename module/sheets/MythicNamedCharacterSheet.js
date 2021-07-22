import { rollAttacks, rollTest, sortItems } from "../dice.js";

export default class MythicNamedCharacterSheet extends ActorSheet {
  static get defaultOptions() {
    return mergeObject(super.defaultOptions, {
      classes: ["mythic", "sheet", "namedCharacter"],
      height: 710,
      width: 800,
      tabs: [
        {
          navSelector: '.tabs',
          contentSelector: '.sheet-body',
          initial: 'summary'
        },
        {
          navSelector:'.talent-tabs',
          contentSelector: '.talent-body',
          initial: 'skills'
        }
      ],
      template: "systems/mythic/templates/sheets/namedCharacter-sheet.hbs"
    });
  }

  getData() {
    const data = super.getData();
    data.config = CONFIG.mythic;
    let a = sortItems(data.items.filter(function(item) { return item.type === "ability" }));
    data.abilities = a.filter(function(item) { return item.data.type === "ability" });
    data.augmentations = a.filter(function(item) { return item.data.type === "augmentation" });
    data.racials = a.filter(function(item) { return item.data.type === "racial" });
    data.traits = a.filter(function(item) { return item.data.type === "trait" });
    // data.educations = data.items
    //   .filter(function(item) { return item.type === "education"})
    //   .sort((a, b) => a.name < b.name ? -1 : (a.name > b.name ? 1 : 0));
    // data.weapons = data.items
    //   .filter(function(item) { return item.type === "weapon"})
    //   .sort((a, b) => a.data.nickname < b.data.nickname ? -1 : 
    //      (a.data.nickname > b.data.nickname ? 1 : 0));
    return data;
  }

  activateListeners(html) {
    super.activateListeners(html);

    html.find(".exp-total-apply").click(this._onExpApply.bind(this));
    html.find(".exp-spent-apply").click(this._onExpApply.bind(this));
    html.find(".item-delete").click(this._onItemDelete.bind(this));
    html.find(".item-edit").click(this._onItemEdit.bind(this));
    html.find(".languages").blur(this._onLanguagesBlur.bind(this));
    html.find(".postable").click(this._onPostItem.bind(this));
    html.find(".rollable").click(this._onRoll.bind(this));
  }

  async _onExpApply(event) {
    event.preventDefault();
    const element = event.currentTarget;
    let data = duplicate(this.actor.data);
    let field;
    if (element.classList[0] === "exp-total-apply") {
      field = document.getElementById("exp-total-value");
      const val = Math.floor(parseInt(field.value));
      data.data.experience.total += (!isNaN(val)) ? val : 0;
    } else if (element.classList[0] === "exp-spent-apply") {
      field = document.getElementById("exp-spent-value");
      const val = Math.floor(parseInt(field.value));
      data.data.experience.spent += (!isNaN(val)) ? val : 0;
    }
    if (isNaN(parseInt(field.value))) {
      ui.notifications.error("Invalid entry: please enter a whole number.")
    }
    field.value = "";
    await this.actor.update(data);
  }

  async _onLanguagesBlur(event) {
    event.preventDefault();
    const element = event.currentTarget;
    let data = duplicate(this.actor.data);
    data.data.trainings.languages.list = element.value.split(";").map(x => x.trim());
    await this.actor.update(data);
  }

  async _onItemDelete(event) {
    event.preventDefault();
    const element = event.currentTarget;
    const item = await this.actor.items.get(element.getAttribute("data-item-id"));
    await item.delete();
  }

  async _onItemEdit(event) {
    event.preventDefault();
    const element = event.currentTarget;
    const item = await this.actor.items.get(element.getAttribute("data-item-id"));
    item.sheet.render(true);
  }

  async _onPostItem(event) {
    event.preventDefault();
    const element = event.currentTarget;
    const item = await this.actor.items.get(element.getAttribute("data-item-id"));
    const template = `systems/mythic/templates/chat/postable-${item.type}.hbs`;
    console.log(item.data.data.type);
    await ChatMessage.create({
      user: game.user.id,
      speaker: ChatMessage.getSpeaker({ actor: this.actor }),
      flavor: game.i18n.localize(`mythic.characterTalents.abilities.type.${item.data.data.type}`),
      content: await renderTemplate(template, item.data)
    });
  }

  async _onRoll(event) {
    event.preventDefault();
    const element = event.currentTarget;
    if (element.classList[0] === "attack") {
      await rollAttacks(element, this.actor);
    } else {
      await rollTest(element, this.actor);
    }
  }
}