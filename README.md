# mythic

An unofficial system implementation for playing the fan-made Halo: Mythic game on Foundry Virtual Tabletop.

## Known Bugs

* Weapon special rule input fields disappear when passed an empty string. **Workaround:** open the console (F12) in Foundry, then enter the following two lines in order:
    1. `myWeapon = game.items.entities.find(w => w.name === "<your weapon name>")`
    2. `myWeapon.data.data.special.<special rule in camelCase>.value = <special rule value>`

## Stretch Goals

* Change all instances of advancements settings to select dropdown.
* Tooltips for everything.
* Collapsible sections for sheet to save on vertical space when not being used (collapse skills, weapon special rules, etc).
* Special ammo builder for weapons, with stats and cost automatically applied to the weapon.
* Compendiums for all items, equipment, skills, abilities, and educations in the core book.
* Compendiums for extra splatbook content.
* Compendium of journal entries for rules.
* Compendium of helpful macros. Some ideas:
    * Calculators for falling damage and damage from getting hit by falling objects
    * Scatter
    * Calculator for splatter damage
* Ability to add custom skills, educations, etc.
