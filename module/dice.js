const CHARACTERISTICS = {
  "str": "Strength",
  "tou": "Toughness",
  "agi": "Agility",
  "wfr": "Warfare Range",
  "wfm": "Warfare Melee",
  "int": "Intellect",
  "per": "Perception",
  "cr": "Courage",
  "ch": "Charisma",
  "ld": "Leadership"
};
const FORMULA = "D100";
const THRESHOLD = 98;

export async function rollTest(element, actor) {
  const type = element.classList[0];
  const test = type === "initiative" 
    ? `${type[0].toUpperCase()}${type.slice(1)}` 
    : (CHARACTERISTICS[element.name] != undefined 
        ? CHARACTERISTICS[element.name] 
        : element.name
    );
  const target = parseInt(element.value);
  const testOptions = await getTestOptions(test);
  const mod = parseInt(testOptions.circumstance);
  if (!testOptions.cancelled) {
    if (type === "initiative") {
      await rollInitiative(element, mod, actor);
    } else {
      return await rollBasicTest(target + mod, test, type, actor);
    }
  } else {
    return;
  }
}

async function getTestOptions(test) {
  const template = "systems/mythic/templates/chat/test-dialog.hbs";
  const html = await renderTemplate(template, {});
  return new Promise(resolve => {
    const data = {
      title: `${CHARACTERISTICS[test] != undefined 
        ? CHARACTERISTICS[test] 
        : test} ${game.i18n.format("mythic.chat.test.title")}`,
      content: html,
      buttons: {
        roll: {
          label: game.i18n.localize("mythic.chat.actions.roll"),
          callback: html => resolve(_processTestOptions(html[0].querySelector("form")))
        },
        cancel: {
          label: game.i18n.localize("mythic.chat.actions.cancel"),
          callback: html => resolve({cancelled: true})
        }
      },
      default: "roll",
      close: () => resolve({cancelled: true})
    };
    new Dialog(data, null).render(true);
  });
}

function _processTestOptions(form) {
  return {
    circumstance: form.circumstance.value
  }
}

async function postBasicTestChatMessage(data, actor) {
  await AudioHelper.play({src: "sounds/dice.wav", volume: 0.8, autoplay: true, loop: false}, true);
  await ChatMessage.create({
    user: game.user.id,
    speaker: ChatMessage.getSpeaker({ actor: actor }),
    flavor: `${data.test} ${game.i18n.localize("mythic.chat.test.title")}`,
    content: buildBasicTestChatMessage(data)
  }, {});
}

function buildBasicTestChatMessage(data) {
  console.log(data);
  const outcome = data.critical 
    ? `Critical ${data.outcome}` 
    : `${data.degrees} degree${Math.abs(parseFloat(data.degrees)) === 1 ? "" : "s"} of ${data.outcome}`;
  const message = `<p><b>${data.roll} vs. ${data.target}:</b> ${outcome}!</p>`;
  return message;
}

async function rollBasicTest(target, test, type, actor) {
  const roll = await new Roll(FORMULA).roll({ async: true });
  let result = {
    type: type,
    test: test,
    roll: roll.total,
    target: target,
    critical: false,
    degrees: 0,
    outcome: ""
  };

  if (roll.total >= THRESHOLD) {
    result.critical = true;
    result.outcome = "failure";
  } else if (roll.total === 1) {
    result.critical = true;
    result.outcome = "success";
  } else {
    const d = (target - roll.total) / 10;
    result.outcome = d >= 0 ? "success" : "failure";
    result.degrees = Math.abs(d).toFixed(1);
  }
  await postBasicTestChatMessage(result, actor);
}

async function rollInitiative(element, mod, actor) {
  const dataset = element.dataset;
  if (dataset.roll) {
    const circumstance = `${mod > 0 ? " + " + mod : mod}`;
    const roll = await new Roll(dataset.roll + circumstance, actor.data.data);
    const result = await roll.roll({  async: true });
    result.toMessage({
      speaker: ChatMessage.getSpeaker({ actor: actor }),
      flavor: dataset.label ? dataset.label : ""
    });
  }
}