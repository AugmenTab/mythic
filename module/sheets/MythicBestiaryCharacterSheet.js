/** @module MythicBestiaryCharacterSheet */

import { sortAndFilterItems } from "../calculations.js";
import { getPostableItemFlavorPath } from "../chat.js";
import { localize, makeUIError } from "../common.js";
import { rollAttacks, rollEvasionBatch, rollTest } from "../dice.js";

/**
 * Class representing the unique features of this system's Bestiary Character sheet.
 * @extends ActorSheet
 */
export default class MythicBestiaryCharacterSheet extends ActorSheet {

  /**
   * Establish default size and class options for the ActorSheet, establish tab navigation on the sheet, and define the path to the Handlebars template.
   * @returns {object} The original source object including updated, inserted, or overwritten records.
   */
  static get defaultOptions() {
    return mergeObject(super.defaultOptions, {
      classes: ["mythic", "sheet", "bestiaryCharacter", "character"],
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
      template: "systems/mythic/templates/sheets/bestiaryCharacter-sheet.hbs"
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
    html.find(".item-delete").click(this._onItemDelete.bind(this));
    html.find(".item-edit").click(this._onItemEdit.bind(this));
    html.find(".item-edit-inline").change(this._onItemEditInline.bind(this));
    html.find(".lang-add").click(this._onLanguageAdd.bind(this));
    html.find(".lang-remove").click(this._onLanguageRemove.bind(this));
    html.find(".postable-details").click(this._onPostDetails.bind(this));
    html.find(".postable-item").click(this._onPostItem.bind(this));
    html.find(".recharge").click(this._onShieldRecharge.bind(this));
    html.find(".reload").click(this._onReload.bind(this));
    html.find(".rollable").click(this._onRoll.bind(this));
    html.find(".special-focus").focus(this._onItemEditInline.bind(this));
  }

  async _onEvade(event) {
    event.preventDefault();
    await rollEvasionBatch(event.currentTarget, this.actor);
  }

  async _onLanguageAdd(event) {
    event.preventDefault();
    const element = event.currentTarget;
    let field = document.getElementById("lang-input");
    if (field.value === "") {
      makeUIError("mythic.characterTalents.trainings.emptyLang");
      return;
    }

    let data = duplicate(this.actor.data);
    let langs = new Set(data.data.trainings.languages);
    if (langs.has(field.value)) {
      makeUIError("mythic.characterTalents.trainings.hasLang");
      field.value = "";
      return;
    } else {
      langs.add(field.value);
    }
    data.data.trainings.languages = [...langs];
    await this.actor.update(data);
  }

  async _onLanguageRemove(event) {
    event.preventDefault();
    const element = event.currentTarget;
    const lang = element.dataset.lang;

    let data = duplicate(this.actor.data);
    let langs = new Set(data.data.trainings.languages);
    if (langs.has(lang)) {
      langs.delete(lang);
    } else {
      makeUIError("mythic.characterTalents.trainings.noLang");
      return;
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
      if (item.type === "armor" && element.dataset.field === "weight.equipped" && element.checked) {
        await this.actor.update({ "data.shields.value": 0 });
        let armors = this.actor.items.filter(a => a.type === "armor" && a.id !== item.id);
        for (let armor of armors) {
          await armor.update({
            "data.weight.equipped": false,
            "data.weight.selfSupported": false
          });
        }
      }
      await item.update({ [key]: element.checked })
    } else {
      const val = parseInt(element.value);
      await item.update({ [key]: isNaN(val) ? element.value : val });
    }
  }

  async _onPostDetails(event) {
    event.preventDefault();
    const element = event.currentTarget;
    const postable = element.dataset.roll;
    const template = `systems/mythic/templates/chat/postable-${postable}.hbs`;
    console.log(template);
    await ChatMessage.create({
      user: game.user.id,
      speaker: ChatMessage.getSpeaker({ actor: this.actor }),
      flavor: localize(`mythic.chat.${postable}.flavor`),
      content: await renderTemplate(template, this.actor)
    });
  }

  async _onPostItem(event) {
    event.preventDefault();
    const element = event.currentTarget;
    const item = await this.actor.items.get(element.getAttribute("data-item-id"));
    const template = `systems/mythic/templates/chat/postable-${item.type}.hbs`;
    await ChatMessage.create({
      user: game.user.id,
      speaker: ChatMessage.getSpeaker({ actor: this.actor }),
      flavor: localize(getPostableItemFlavorPath(item)),
      content: await renderTemplate(template, item.data)
    });
  }

  async _onReload(event) {
    event.preventDefault();
    const element = event.currentTarget;
    const item = await this.actor.items.get(element.getAttribute("data-item-id"));
    await item.update({
      "data.ammoList.STD.currentMag": item.data.data.magazineCapacity
    });
  }

  async _onShieldRecharge(event) {
    event.preventDefault();
    let data = this.actor.data;
    const val = data.data.shields.value + data.data.shields.recharge;
    const update = val > data.data.shields.max ? data.data.shields.max : val;
    await this.actor.update({ "data.shields.value": update });
  }

  async _onRoll(event) {
    event.preventDefault();
    const element = event.currentTarget;
    if (element.classList[0] === "attack") {
      const item = await this.actor.items.get(element.getAttribute("data-item-id"));
      const newMag = await rollAttacks(element, this.actor, item);
      if (!isNaN(newMag)) await item.update({
        "data.ammoList.STD.currentMag": newMag
      });
    } else {
      await rollTest(element, this.actor);
    }
  }
}
