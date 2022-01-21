# Changelog

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
* The Cryo, Flame, and Needle special rules now roll their damage in the attack chat message based on the number of hits the attacker landed. Each is also a link that will roll its typical damage on its own.
* Updated the Weapon Item sheet in preparation for a new data model that will be able to support Special Ammo down the line.

### Bug Fixes

* Fixed bug where carry weights in the Lift range were displaying incorrect percentages in the Carry Weight Bar.
* Fixed bug where Mythic characteristic contributions to carry weight were not doubled on soldier types that double STR/TOU on carry weight.
* Fixed bug where magazines would not deplete when the amount of shots fired would bring the current magazine value to 0.

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
