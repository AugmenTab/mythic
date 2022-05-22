import { sortAndFilterItems } from "../calculations.js";
import { rollAttacks, rollEvasionBatch, rollTest } from "../dice.js";

export default class MythicFloodCharacterSheet extends ActorSheet {
  static get defaultOptions() {
    return mergeObject(super.defaultOptions, {
      classes: ["mythic", "sheet", "flood", "character"],
      height: 620,
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
      template: "systems/mythic/templates/sheets/floodCharacter-sheet.hbs"
    });
  }

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
    data.equipment = sortAndFilterItems(data.items, "equipment");
    data.weapons = sortAndFilterItems(data.items, "weapon", "nickname");
    data.equippedWeapons = data.weapons.filter(w => w.data.weight.equipped);
    return data;
  }

  getRollData() {
    const data = super.getRollData();
    return data;
  }

  activateListeners(html) {
    super.activateListeners(html);

    html.find(".evade").click(this._onEvade.bind(this));
    html.find(".item-delete").click(this._onItemDelete.bind(this));
    html.find(".item-edit").click(this._onItemEdit.bind(this));
    html.find(".item-edit-inline").change(this._onItemEditInline.bind(this));
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
      flavor: game.i18n.localize(`mythic.chat.${postable}.flavor`),
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
      flavor: game.i18n.localize(`mythic.characterTalents.abilities.type.${item.data.data.type}`),
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
