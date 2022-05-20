export async function migrateWorld() {
  await migrateActors();
  await migrateItems();
  await migrateScenes();
  await migrateCompendia();
}

export async function migrateCauterize() {
  for (let item of game.items) {
    if (item.type === "weapon") {
      let updateData = {};
      let itemData = item.data.data;

      updateData["data.ammoList.STD.critsOn"] = 10;
      updateData["data.special.cauterize"] = {
        "has": itemData.special.cauterize.has,
        "needsInput": false
      };

      updateData["data.settings"] = {
        "firearmType": "firearms",
        "barrel": "xs",
        "bulletDiameter": 0.0,
        "caseLength": 0,
        "singleUse": false
      };

      if (!foundry.utils.isObjectEmpty(updateData)) {
        console.log(`Migrating Item entity ${item.name}...`);
        await item.update(updateData);
      }
    }
  }
}

function migrateActorData(actor) {
  let updateData = {};
  let actorData = actor.data;

  updateData["data.carryingCapacity.imposing"] = false;
  updateData["data.carryingCapacity.mod"] = 0;
  updateData["data.carryingCapacity.bar.tier"] = "carry";
  updateData["data.carryingCapacity.hearing"] = (
    parseInt(actorData.carryingCapacity.hearing) || 0
  );

  updateData["data.fatigue.enduring"] = actorData.fatigue.enduring ? 1 : 0

  updateData["data.skills.medXenophile"] = actorData.skills.medMgalekgolo;

  return updateData;
}

async function migrateActors() {
  // Update all Actors.
  for (let actor of game.actors.content) {
    const updateData = migrateActorData(actor.data);
    if (!foundry.utils.isObjectEmpty(updateData)) {
      console.log(`Migrating Actor entity ${actor.name}...`);
      await actor.update(updateData);
    }
  }
}

async function migrateCompendia() {
  for (let pack of game.packs) {
    if (pack.metadata.package !== "world") {
      const packType = pack.metadata.entity;
      if (["Actor", "Scene", "Item"].includes(packType)) {
        const wasLocked = pack.locked;
        await pack.configure({ locked: false });
        await pack.migrate();

        const documents = await pack.getDocuments();
        for (let doc of documents) {
          let updateData = {};
          switch(packType) {
            case "Actor":
              updateData = migrateActorData(doc.data);
              break;
            case "Scene":
              updateData = migrateSceneData(doc.data);
              break;
            case "Item":
              updateData = migrateItemData(doc.data)
              break;
          }

          if (!foundry.utils.isObjectEmpty(updateData)) {
            console.log(`Migrating ${packType} entity ${doc.name} in Compendium ${pack.collection}...`);
            await doc.update(updateData);

          }
        }
        await pack.configure({ locked: wasLocked });
      }
    }
  }
}

async function migrateItemData(item) {
  let updateData = {};
  let itemData = item.data;

  updateData["ammoGroup"] = "";
  updateData["magazineCapacity"] = itemData.magazine.max;

  updateData["data.special.flashbang"] = {"has": false};
  updateData["data.special.tearGas"] = {"has": false};
  updateData["data.special.cryo"] = {
    "has": false,
    "value": "1D5",
    "needsInput": true
  };
  updateData["data.special.smoke"] = {
    "has": false,
    "value": 0,
    "needsInput": true
  };
  updateData["data.special.cauterize"] = {
    "has": itemData.special.cauterize.has,
    "needsInput": false
  };

  const dice = itemData.attack.damageRoll.toLowerCase().split("d");
  updateData["ammoList.STD"] = {
    "attackBonus": 0,
    "diceQuantity": parseInt(dice[0]) || 1,
    "diceValue": parseInt(dice[1]) || 5,
    "baseDamage": itemData.attack.baseDamage,
    "strDamage": itemData.attack.strDamage,
    "piercing": itemData.attack.piercing,
    "strPiercing": itemData.attack.strPiercing,
    "target": itemData.attack.target,
    "currentMag": itemData.magazine.current,
    "critsOn": 10,
    "range": itemData.range,
    "special": {},
    "desc": ""
  };

  return updateData;
}

async function migrateItems() {
  // Update all Items.
  for (let item of game.items.content) {
    const updateData = migrateItemData(item.data);
    if (!foundry.utils.isObjectEmpty(updateData)) {
      console.log(`Migrating Item entity ${item.name}...`);
      await item.update(updateData);
    }
  }
}

function migrateSceneData(scene) {
  const tokens = scene.tokens.map(token => {
    const t = token.toJSON();
    if (!t.actorLink) {
      const actor = duplicate(t.actorData);
      actor.type = t.actor?.type;
      const update = migrateActorData(actor);
      mergeObject(t.actorData, update);
    }
    return t;
  });
  return { tokens };
}

async function migrateScenes() {
  // Update tokens that are already on a scene.
  for (let scene of game.scenes.contents) {
    const updateData = migrateSceneData(scene);
    if (!foundry.utils.isObjectEmpty(updateData)) {
      console.log(`Migrating Scene ${scene.name}...`);
      await scene.update(updateData);
    }
  }
}
