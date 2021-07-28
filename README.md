# Mythic 4.x for Foundry

An unofficial system implementation for playing the fan-made [Halo: Mythic](https://www.reddit.com/r/HaloMythic/) tabletop roleplaying game on [Foundry Virtual Tabletop](https://foundryvtt.com/).

This system (once complete) provides character sheet support for various Actor and Item types, mechanical support for dice and rules necessary to play games of Mythic 4.0 and later. Coming soon will be compendium content for the Bestiary, the Flood, and all in-game items.

## Installation

Coming soon!

## Features

The following is a list of features offered by Mythic for Foundry.

### Actors

* Named Character: The primary Actor type for player characters, NPCs, and special Nemesis enemies. Nearly every aspect of this character sheet can be customized to fine-tune exacting stat requirements.
* Bestiary Character
* Flood Character
* Vehicle

### Items

* Ability: All purchasable abilities, as well as abilities unique to particular races or soldier types, traits, and augmentations.
* Armor
* Education: All purchasable educations.
* Equipment
* Weapon: Anything that requires a to-hit roll or deals damage can be made into a weapon, which can be rolled from the owner's sheet to make attacks and post informative messages about the item to chat. Weapons come in three main categories: Ranged, Melee, and Thrown.

### Compendiums

All of the following are planned compendiums. None are currently implemented.

* Actor
    * Bestiary - Covenant
    * Bestiary - Flood
    * Bestiary - Human
    * Bestiary - Promethean
    * Bestiary - Sentinel
    * Vehicles - Banished
    * Vehicles - Covenant
    * Vehicles - Forerunner
    * Vehicles - Human
* Item
    * Abilities
    * Armors
    * Educations
    * Equipment - Covenant
    * Equipment - Human
    * Weapons - Banished
    * Weapons - Covenant
    * Weapons - Forerunner
    * Weapons - Human
    * Weapons - Splatbooks
* JournalEntry
    * Ammunition
    * Armor Abilities
    * Armor Permutations
    * Armor Variants - Covenant
    * Armor Variants - MJOLNIR GEN1
    * Armor Variants - MJOLNIR GEN2
    * Modifications - Vehicles
    * Modifications - Weapons
    * Rules
    * Skills
* Macro
    * Calculate Damage from Falling Objects
    * Calculate Falling Damage
    * Calculate Scatter
    * Calculate Splatter Damage
    * Generate Encounter
    * Generate Environment
    * Generate Mission
    * Generate NPC
    * Generate Weather Conditions
    * Generate Planet

## Legal

The software component of this system is distributed under the GNUv3 license.

Halo &copy; Microsoft Corporation. Mythic for Foundry was created under Microsoft's "Game Content Usage Rules." It is not endorsed by Microsoft and does not reflect the views or opinions of Microsoft or anyone officially involved in producing or managing Halo. As such, it does not contribute to the official narrative of the fictional universe, if applicable.

## Known Bugs

* Talents tabs do not apply active class when first clicking in from elsewhere.
* Experience Apply buttons are discarding first attempt to alter values, then work correctly afterward.
* Weapons listed in Weapon Summary are not sorting.

## Planned Features and Stretch Goals

* Have attacks drain the weapon's magazine.
* Allow circumstance modifier dialog window to evaluate modifiers.
* Tooltips for everything.
* Languages and equipment tags 
* Special ammo builder for weapons, with stats and cost automatically applied to the weapon.
* Compendiums for all items, equipment, skills, abilities, and educations in the core book.
* Compendiums for extra splatbook content.
* Compendium of journal entries for rules.
* Compendium of helpful macros. Some ideas:
    * Calculators for falling damage and damage from getting hit by falling objects
    * Scatter
    * Calculator for splatter damage
* Ability to add custom skills.
* Fully functioning Medical page to handle injuries.
* Lock nav bar at the top of the Actor Sheet.
* Add hit locations for targeting vehicles.
* Add called shots, Clear Target, and Precision Strike.
* Add dual wielding.
* Weapon summary QOL improvements:
    * Evasion button should prompt a dialog window asking how many to roll (and circumstance modifier), then roll batched Evasions, with stacking penalty.
