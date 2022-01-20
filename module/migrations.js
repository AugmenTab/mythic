export default async function migrateWorld() {
  await migrateActors();
  await migrateItems();
  await migrateScenes();
  await migrateCompendia();
}

function migrateActorData(actor) {
  let updateData = {};
  // TODO: Call to migrateItemData with all items attached to Actor.
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