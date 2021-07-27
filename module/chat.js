import { calculateCharacteristicModifier } from "./calculations.js";

export function addChatListeners(html) {
  html.on("click", ".scatter", onScatter);
}

function getArrow(str) {
  const compass = {
    "N": "up",
    "E": "right",
    "S": "down",
    "W": "left"
  }
  return `<i class="fas fa-arrow-circle-${compass[str]}"></i>`;
}

function getScatterDirection(roll) {
  if (roll >= 10) return "NW";
  if (roll >= 9) return "W";
  if (roll >= 8) return "SW";
  if (roll >= 6) return "S";
  if (roll >= 5) return "SE";
  if (roll >= 4) return "E";
  if (roll >= 3) return "NE";
  if (roll >= 1) return "N";
}

async function getScatterOptions(degrees = 0) {
  const template = "systems/mythic/templates/chat/scatter-dialog.hbs";
  const html = await renderTemplate(template, {dof: degrees});
  return new Promise(resolve => {
    const data = {
      title: game.i18n.localize("mythic.chat.scatter.title"),
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

async function onScatter(event) {
  const element = event.currentTarget;
  const options = await getScatterOptions(element.dataset.dof);
  let msg = "";

  let times = options.isZeroG ? 2 : 1;
  for (let i = 1; i <= times; i++) {
    const roll = await new Roll("1D10").roll({ async: true });
    if (i === 2) msg += " | ";
    let distance = parseInt(options.distance);
    let mod = 0;
    mod += Math.floor(parseFloat(options.dof));
    mod += Math.floor(distance / 100);
    if (distance > element.dataset.range) {
      if (distance > element.dataset.range * 2) { mod *= 5 }
      else { mod *= 2 }
    }
    mod -= calculateCharacteristicModifier(parseInt(element.dataset.wfm));
    const dice = await new Roll(`${mod > 1 ? mod : 1}D10`).roll({ async: true });
    msg += getScatterDirection(roll.total).split("").map(x => getArrow(x)).join("");
    msg += ` ${dice.total} m`;
  }

  await AudioHelper.play({src: "sounds/dice.wav", volume: 0.8, autoplay: true, loop: false}, true);
  element.classList.remove("scatter");
  element.innerHTML = msg;
}

function _processTestOptions(form) {
  return {
    dof: form.dof.value,
    distance: form.distance.value,
    isZeroG: form.isZeroG.checked
  };
}