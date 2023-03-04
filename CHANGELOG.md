# Changelog

## 0.3.0

## Changes

* Full migration to Foundry v10.
* Updated the system to Mythic 6.0, with the following adjustments:
  * Set the default critical failure threshold to only natural 100s.
  * Added a value field for editing the Electrified special rule for Weapons.
  * Introduction of new Long Barrel special rule for Weapons.
  * Updated values and logic handling melee reach, to-hit, and damage.
  * Updated the rules for calculating throwing distance.
  * Updated the rules for calculating carry, lift, and push weight.
  * Changed the "Double AGI Mod to Run/Charge" field to "AGI Bonus for Run/Charge."
    * This is to support the new rule for Hunters, where they were changed from doubling their AGI mod for figuring Charge and Run to instead gain a +3 bonus to their AGI mod when calculating those movement speeds. This field can take a number input, which will allow players to use either the new or old rules, as well as for use in like abilities, both for homebrew content and possible future official content.
  * Updated a number of system terms that have been changed for 5.1.
  * Fixed wound calculation to now be accurate for both Named Characters and Bestiary.
  * Updated rate of melee attacks.
* Re-introduced the ammunition management feature, with a new system setting offering the following three modes:
  * Magazines - weapons track full magazines, and reloaded weapons lose the held ammunition in the current magazine.
  * Ammo Pool - a count of total rounds are tracked, with magazines always topped off. This works like how the games handle ammunition, and is how single-loading weapons will work even in Magazines mode.
  * Self-Managed - no automation is applied here, allowing users to manage ammunition however they please.
* Added Parry as an alternative to Evasion by widening the available characteristics for Evasion tests to WFM and updating sheet copy.
* Added a field for circumstance modifiers on damage for attacks.
* Simplified initiative tie-breaking when sorting combatants.
* Changes the "Enduring?" select field to the "Fatigue Levels Ignored" number field.
* Factors encumbrance penalties into characteristics with three methods (as system settings):
  * Standard, which uses the rules in the book.
  * Simplified, which uses a simplified version of the rules.
  * Off, which does no encumbrance penalty automation.
* Scatter is now determined at the time the attack is made, so it can persist between server instances.
* Natural Armor is now supported.
* Effects for attacking at different ranges (bonuses and penalties to attack, damage, and pierce) are applied based on distance to target. This can be toggled on and off with a system setting.
* Education limit is now tracked and modifiable.
* Perceptive Range is now tracked and modifiable.
* Weapons have a Scope Magnification value, and display their effective minimum and maximum ranges with that magnification factored in.
* Attacks that deal Special Damage now indicate this by displaying a bullseye.
* Equipment Item Compendia are now available.
* Field added to melee weapons to indicate the number of extra attacks it grants on an Attack action.
* Field added to thrown weapons to indicate a flat range bonus granted by the weapon.

## Bug Fixes

* Weapon Item sheet tabs for Settings and Notes are now displaying properly.
* Fixed an instance where Armor that held values for Shields or Characteristics modifiers, but had those fields not checked as "has," were still applying those values to the Actor.
* Item postables now use the correct values.
* Inventory items now calculate and display weight values correctly.
* Fixes Actor data preparation failing at the time of Actor creation.
* Self-supporting items now only apply their benefit when they are equipped.
* Dice are now parsed in circumstance modifiers for Tests, as well as To-Hit and Damage values for Attacks.
* Various formatting issues in sheets and chat messages were corrected.

## 0.2.4

## Changes

* Removes incomplete magazine count feature. This will require a different, more involved re-implementation and this removal will prevent the current buggy implementation from breaking the update for those that want to use it.

## 0.2.3

## Changes

* Known languages can now be removed from a character simply by clicking on them.
* The following were given post-to-chat functionality:
  * Items
    * Ability Item
    * Armor Item
    * Education Item
    * Equipment Item
    * Weapon Item
  * Trainings
    * Known Languages
    * Faction Trainings
    * Equipment Trainings
* Scatter arrows are now rotated into the proper direction rather than using differing numbers of arrows.
* A value to track how many loaded magazines are carried for a weapon was added to the Special Ammo tab on the Weapon Item sheet. This value is automatically decremented with each reload, and blocks reloads when the character has run out of loaded magazines for the weapon.

## Bug Fixes

* Fixes the Explode and Explode Once alternate settings for Special Damage to use the correct weapon "Crits On" value.
* Fixes the broken Scatter link for weapon chat dialogs.

## 0.2.2

### Bug Fixes

* Fixes a bug where `textarea` elements were adding unnecessary whitespace.
* Exposes system setting to choose which edition of Mythic's rules for the Strong Back ability to use.

## 0.2.1

### Bug Fixes

* Empties and disables the Swarm Modifier field on the Flood Actor sheet when the unit does not swarm (the "Swarms?" checkbox is unchecked).
* Fixes a bug where the carrying capacity tier label for the bar at the top of the Inventory tab on the Named Character and Bestiary Enemy Actor sheets would break when the inventory total weight is 0 kg.
* Updates the Cauterize special rule on the Weapon Item sheet for the new 5.0 rules. This means removing the numerical value it carried that previously represented the die result required to score a critical hit for a given weapon.
* Adds a few fields to the Weapon Item that may be required in the future. These are on a new Settings tab, and have no bearing on play.

## 0.2.0

### Changes

* All relevant rules updated to support Mythic 5.0.
* Created and implemented:
    * "Bestiary Character" Actor type.
    * "Flood" Actor type.
* New settings created relating to the use of the Flood, and the new Flood Actor type.
* Added hit locations for vehicles.
* Increased minimum Critical Failure Threshold setting to 95.
* Added a text instance of the carry weight range the character is in under the Carry Weight Bar.
* Added a Recharge Shield button to replenish current Shield Integrity by the Recharge value, up to the Shield's maximum Integrity.
* Fixed various spacing issues on all sheets.
* Added the Cryo, Flashbang, Smoke Grenade, and Tear Gas special rules to the Weapon Item sheet.
* The Cryo, Flame, and Needle special rules now roll their damage in the attack chat message based on the number of hits the attacker landed. Each is also a link that will roll its typical damage to the chat on its own.
* Updated the Weapon Item sheet in preparation for a new data model that will be able to support Special Ammo down the line.

### Bug Fixes

* Fixed bug where carry weights in the Lift range were displaying incorrect percentages in the Carry Weight Bar.
* Fixed bug where Mythic characteristic contributions to carry weight were not doubled on soldier types that double STR/TOU on carry weight.
* Fixed bug where magazines would not deplete when the amount of shots fired would bring the current magazine value to 0.
* Fixed bug where combat turns and rounds could not be advanced.
* Fixed bug where penalties to characteristics imposed by armor would not apply to the wearer's characteristics.

## 0.1.1

### Changes

* Armor now calculates relevant values on its own.
* Fixed spacing issue on weapon summary fire modes.
* Magazine no longer depletes when attack is cancelled.

## 0.1.0

### Changes

* Created and implemented:
    * "Named Character" Actor type.
    * "Ability" Item type.
    * "Armor" Item type.
    * "Education" Item type.
    * "Equipment" Item type.
    * "Weapon" Item type.
* Modified Combat system to:
    * Accept custom initiative formula.
    * Advance time by 6 seconds per Round.
