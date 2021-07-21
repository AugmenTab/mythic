import { rollAttacks, rollTest } from "../dice.js";

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
    return data;
  }

  activateListeners(html) {
    super.activateListeners(html);

    html.find(".rollable").click(this._onRoll.bind(this));
    html.find(".exp-total-apply").click(this._onExpApply.bind(this));
    html.find(".exp-spent-apply").click(this._onExpApply.bind(this));
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