# Changelog

## 0.2.0

### Changes

* All relevant rules updated to support Mythic 4.5.
* New settings created to allow users to selectively use rules from Mythic 4.0.
    * The Enduring Outlier Trait
    * The Strong Back Ability
* Created and implemented:
    * "Bestiary Character" Actor type.
    * "Flood" Actor type.
* New settings created relating to the use of the Flood, and the new Flood Actor type.
* Updated the Weapon Item sheet in preparation for a new data model that will be able to support Special Ammo down the line.
* Created data migration files to prevent breaking of existing Weapon items from previous versions.
* Added hit locations for vehicles.
* Increased minimum Critical Failure Threshold setting to 95 due to feedback that it was too cumbersome to use a slider with 100 options for a setting with such a small range.
* Added a text instance of the carry weight range the character is in under the Carry Weight Bar.
* Added a Recharge Shield button to replenish current Shield Integrity by the Recharge value, up to the Shield's maximum Integrity.
* Fixed bug where carry weights in the Lift range where displaying incorrect percentages in the Carry Weight Bar.
* Fixed various spacing issues on all sheets.

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
