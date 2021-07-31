/** @module MythicNamedCharacterSheet */

import { sortAndFilterItems } from "../calculations.js";
import { rollAttacks, rollEvasionBatch, rollTest } from "../dice.js";

/** Class representing the unique features of this system's Named Character sheet.
 * @extends ActorSheet
 */
export default class MythicNamedCharacterSheet extends ActorSheet {

  /**
   * Establish default size and class options for the ActorSheet, establish tab navigation on the sheet, and define the path to the Handlebars template.
   * @returns {object} The original source object including updated, inserted, or overwritten records.
   */
  static get defaultOptions() {
    return mergeObject(super.defaultOptions, {
      classes: ["mythic", "sheet", "namedCharacter"],
      height: 735,
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

  /**
   * Prepares the ActorData.
   * @returns {ActorData} The prepared ActorData.
   */
  getData() {
    const data = super.getData();
    data.config = CONFIG.mythic;

    const a = sortAndFilterItems(data.items, "ability");
    // data.abilities = a.filter(function(i) { return i.data.type === "ability" });
    data.abilities = a.filter(i => i.data.type === "ability");
    data.augmentations = a.filter(function(i) {
      return i.data.type === "augmentation"
    });
    data.racials = a.filter(function(i) { return i.data.type === "racial" });
    data.traits = a.filter(function(i) { return i.data.type === "trait" });

    data.armors = sortAndFilterItems(data.items, "armor");
    data.educations = sortAndFilterItems(data.items, "education");
    data.equipment = sortAndFilterItems(data.items, "equipment");
    data.weapons = sortAndFilterItems(data.items, "weapon", "nickname");
    data.equippedWeapons = data.weapons.filter(w => w.data.weight.equipped);
    return data;
  }

  /**
   * Prepares the RollData so attributes can be referenced in the game.
   * @returns {RollData} The prepared RollData.
   */
  getRollData() {
    const data = super.getRollData();
    return data;
  }

  /**
   * Establishes event listeners on the ActorSheet.
   * @param {jQuery.fn} html - The HTML hook.
   */
  activateListeners(html) {
    super.activateListeners(html);

    html.find(".evade").click(this._onEvade.bind(this));
    html.find(".exp-total-apply").click(this._onExpApply.bind(this));
    html.find(".exp-spent-apply").click(this._onExpApply.bind(this));
    html.find(".item-delete").click(this._onItemDelete.bind(this));
    html.find(".item-edit").click(this._onItemEdit.bind(this));
    html.find(".item-edit-inline").change(this._onItemEditInline.bind(this));
    html.find(".lang-add").click(this._onLanguageUpdate.bind(this));
    html.find(".lang-remove").click(this._onLanguageUpdate.bind(this));
    html.find(".postable").click(this._onPostItem.bind(this));
    html.find(".rollable").click(this._onRoll.bind(this));
    html.find(".reload").click(this._onReload.bind(this));
    html.find(".special-focus").focus(this._onItemEditInline.bind(this));
  }

  async _onEvade(event) {
    event.preventDefault();
    await rollEvasionBatch(event.currentTarget, this.actor);
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
      ui.notifications.error(game.i18n.localize("mythic.chat.error.nan"));
    }
    field.value = "";
    await this.actor.update(data);
  }

  async _onLanguageUpdate(event) {
    event.preventDefault();
    const element = event.currentTarget;
    const field = document.getElementById("lang-input");
    if (field.value === "") {
      ui.notifications.error(game.i18n.localize("mythic.characterTalents.trainings.emptyLang"));
      return;
    }

    let data = duplicate(this.actor.data);
    let langs = new Set(data.data.trainings.languages);
    if (element.classList[0] === "lang-add") {
      if (langs.has(field.value)) {
        ui.notifications.error(game.i18n.localize("mythic.characterTalents.trainings.hasLang"));
        return;
      } else langs.add(field.value);
    } else if (element.classList[0] === "lang-remove") {
      if (langs.has(field.value)) {
        langs.delete(field.value);
      } else {
        ui.notifications.error(game.i18n.localize("mythic.characterTalents.trainings.noLang"));
        return;
      }
    }
    data.data.trainings.languages = [...langs];
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

  async _onItemEditInline(event) {
    event.preventDefault();
    const element = event.currentTarget;
    const item = await this.actor.items.get(element.getAttribute("data-item-id"));
    const key = `data.${element.dataset.field}`;
    if (element.type === "checkbox") {
      await item.update({ [key]: element.checked })
    } else {
      const val = parseInt(element.value);
      await item.update({ [key]: isNaN(val) ? element.value : val });
    }
  }

  async _onPostItem(event) {
    event.preventDefault();
    const element = event.currentTarget;
    const item = await this.actor.items.get(element.getAttribute("data-item-id"));
    const template = `systems/mythic/templates/chat/postable-${item.type}.hbs`;
    await ChatMessage.create({
      user: game.user.id,
      speaker: ChatMessage.getSpeaker({ actor: this.actor }),
      flavor: game.i18n.localize(`mythic.characterTalents.abilities.type.${item.data.data.type}`),
      content: await renderTemplate(template, item.data)
    });
  }

  async _onReload(event) {
    event.preventDefault();
    const element = event.currentTarget;
    const item = await this.actor.items.get(element.getAttribute("data-item-id"));
    await item.update({ "data.magazine.current": item.data.data.magazine.max });
  }

  async _onRoll(event) {
    event.preventDefault();
    const element = event.currentTarget;
    if (element.classList[0] === "attack") {
      await rollAttacks(element, this.actor);
      const item = await this.actor.items.get(element.getAttribute("data-item-id"));
      const newMag = item.data.data.magazine.current -= parseInt(element.innerHTML);
      await item.update({ "data.magazine.current": newMag });
    } else {
      await rollTest(element, this.actor);
    }
  }
}