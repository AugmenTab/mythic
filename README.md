# mythic

An unofficial system implementation for playing the fan-made Halo: Mythic game on Foundry Virtual Tabletop.

## Known Bugs

* Flame special rule input field disappears when passed an empty string. **Workaround:** open the console (F12) in Foundry, then enter the following two lines in order:
    1. `myWeapon = game.items.entities.find(w => w.name === "<your weapon name>")`
    2. `myWeapon.data.data.special.flame.value = <Flame special rule value>`

## Stretch Goals

* Change characteristics advancements settings to select dropdown.
* Tooltips for everything.
* Collapsible sections for sheet to save on vertical space when not being used (collapse skills, weapon special rules, etc).
* Special ammo builder for weapons, with stats and cost automatically applied to the weapon.
* Compendiums for all items, equipment, skills, abilities, and educations in the core book.
* Compendiums for extra splatbook content.
* Compendium of journal entries for rules.
