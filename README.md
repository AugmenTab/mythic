# Mythic for Foundry

An unofficial system implementation for playing the fan-made [Halo: Mythic](https://www.reddit.com/r/HaloMythic/) tabletop roleplaying game on [Foundry Virtual Tabletop](https://foundryvtt.com/).

<!-- ![Foundry Version Supported](https://img.shields.io/endpoint?url=https%3A%2F%2Ffoundryshields.com%2Fversion%3Fstyle%3Dfor-the-badge%26url%3Dhttps%3A%2F%2Fraw.githubusercontent.com%2FAugmenTab%2Fmythic%2Fmain%2Fsystem.json) -->
![License](https://img.shields.io/github/license/AugmenTab/mythic?style=for-the-badge)
![Downloads](https://img.shields.io/github/downloads/AugmenTab/mythic/total?style=for-the-badge)
![Downloads - Latest Release](https://img.shields.io/github/downloads/AugmenTab/mythic/latest/total?style=for-the-badge)
![Code Size](https://img.shields.io/github/languages/code-size/AugmenTab/mythic?style=for-the-badge)
![Open Issues](https://img.shields.io/github/issues/AugmenTab/mythic?style=for-the-badge)
![Last Commit](https://img.shields.io/github/last-commit/AugmenTab/mythic?style=for-the-badge)

This system (once complete) provides character sheet support for various Actor and Item types, mechanical support for dice and rules necessary to play games of Mythic 6.0. Coming soon will be compendium content for the Bestiary, the Flood, and all in-game items.

You can find video guides on how to install and use Mythic for Foundry [on my YouTube channel](https://www.youtube.com/playlist?list=PLeJTbB--c7R3tkFoFiC145CT_ykJ-FdsP).

## Installation

The latest version of the Mythic for Foundry system can be installed using the manifest. On the Foundry VTT home page ("Configuration and Setup"), navigate to the "Game Systems" tab. Select "Install System," and provide the following link as the Manifest URL:
* [https://raw.githubusercontent.com/AugmenTab/mythic/main/system.json](https://raw.githubusercontent.com/AugmenTab/mythic/main/system.json)

Any current or prior version can also be installed manually. You must clone or extract the system files to your `Data/systems/mythic` folder. Archives of all editions can be found in the [Releases](https://github.com/AugmenTab/mythic/releases).

Always remember to back up your world files before updating.

## Features

The following is a list of features offered by Mythic for Foundry.

### Actors

All available Actor types in this system.

* **Named Character:** The primary Actor type for player characters, NPCs, and special Nemesis enemies. Nearly every aspect of this character sheet can be customized to fine-tune exacting stat requirements.
* **Bestiary Character**: The Actor type representing all unnamed mooks in the game, pulled from the Bestiary. These are made to be generic boilerplate templates for enemies that can be rolled from and adjusted quickly so as to represent a variety of similar enemies.
* **Flood**: An Actor type specifically for the Flood Bestiary, playing to their unique strengths and offers multiple avenues of adjustment for stats and swarm sizes.
* **Vehicle**: The Actor type for vehicles, permitting them to own items and weapons that can be swapped out to represent different configurations without requiring different sheets.

### Items

All available Item types in this system.

* **Ability:** All purchasable abilities, as well as abilities unique to particular races or soldier types, traits, and augmentations.
* **Armor:** All wearable protective armor suits available in the game.
* **Education:** All purchasable educations.
* **Equipment:** All other items that don't fall into any other category are represented by this item type.
* **Weapon:** Anything that requires a to-hit roll or deals damage can be made into a weapon, which can be rolled from the owner's sheet to make attacks and post informative messages about the item to chat. Weapons come in three main categories: Ranged, Melee, and Thrown.

### Compendia

All of the compendia that come included with this system. **Development on some Compendia will rely on as-of-yet unimplemented features or bug fixes, and thus won't be introduced until those dependencies are resolved.**

Available Compendium Packs:
* Item
  * Abilities
  * Armor Permutations - UNSC
  * Equipment - Covenant
  * Equipment - Forerunner
  * Equipment - UNSC
  * Melee Weapons - Banished
  * Melee Weapons - Covenant
  * Melee Weapons - Forerunner
  * Melee Weapons - UNSC
  * Ranged Weapons - Banished
  * Ranged Weapons - Covenant
  * Ranged Weapons - Forerunner
  * Ranged Weapons - UNSC

Planned Compendium Packs:
* Actor
  * *Bestiary - Banished*
  * *Bestiary - Covenant*
  * *Bestiary - Forerunner*
  * *Bestiary - UNSC*
  * *Vehicles - Banished*
  * *Vehicles - Covenant*
  * *Vehicles - Forerunner*
  * *Vehicles - UNSC*
* Item
  * *Armors - Banished*
  * *Armors - Covenant*
  * *Armors - Forerunner*
  * *Armors - UNSC*

## Legal

The software component of this system is distributed under the GNUv3 license.

Halo &copy; Microsoft Corporation. Mythic for Foundry was created under Microsoft's "Game Content Usage Rules." It is not endorsed by Microsoft and does not reflect the views or opinions of Microsoft or anyone officially involved in producing or managing Halo. As such, it does not contribute to the official narrative of the fictional universe, if applicable.

## Known Bugs

* Talents tabs do not apply active class when first clicking in from elsewhere.
* Experience Apply buttons are discarding first attempt to alter values, then work correctly afterward.
* Experience Purchase Price field does not focus coming off the Name field. It has to be focused a second time before the focus sticks.

## Planned Features and Stretch Goals

For a real-time record of what I'm working on, you can check the project's [Trello board](https://trello.com/b/y80KFteH/mythic-for-foundry).

* Fully functioning Medical page to handle Special Damage.
* Special ammo builder for weapons, with stats and cost automatically applied to the weapon.
* Compendia as laid out above.
* Tooltips for everything.
* A supporting API to handle weapon stat and price calculation using the [Mythic and 100DOS Community Development Tools](https://docs.google.com/spreadsheets/d/1e5gepoZg_IZvupVw-rCKNMz42xMxGlF--oJluaa9Ljo/edit?usp=sharing).
* Add called shots, Clear Target, and Precision Strike.
* Add dual wielding.
* Lock nav bar & inventory bar at the top of the Actor Sheet.
* Ability to add custom skills.
