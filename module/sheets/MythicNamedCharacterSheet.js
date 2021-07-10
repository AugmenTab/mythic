import * as Dice from "../dice.js";

export default class MythicNamedCharacterSheet extends ActorSheet {
  static get defaultOptions() {
    return mergeObject(super.defaultOptions, {
      classes: ["mythic", "sheet", "namedCharacter"],
      tabs: [
        {
          navSelector: '.tabs',
          contentSelector: '.sheet-body',
          initial: 'summary'
        }
      ],
      template: "systems/mythic/templates/sheets/namedCharacter-sheet.hbs"
    });
  }

  getData() {
    const data = super.getData();
    data.config = CONFIG.mythic;
    return data;
  }

  activateListeners(html) {
    super.activateListeners(html);

    html.find(".rollable").click(this._onRoll.bind(this));
  }

  async _onRoll(event) {
    event.preventDefault();
    const element = event.currentTarget;
    const test = element.classList[0];

    if (test === "characteristic") {
      const stat = element.name;
      const target = parseInt(element.value);
      const result = await Dice.rollCharacteristicTest(stat, target);
      console.log(result);
    }
  }
}