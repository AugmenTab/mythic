# Changelog

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
