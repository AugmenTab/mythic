# Changelog

## 0.2.0

### Changes

* All relevant rules updated to support Mythic 4.5.
* New settings created to allow users to selectively use rules from Mythic 4.0.
    * The Enduring Outlier Trait
    * The Strong Back Ability
* Created and implemented the Bestiary Character Actor type.
* Updated the Weapon Item sheet in preparation for the new data model.
* Created data migration files to prevent breaking of existing Weapon items from previous versions.
* Added hit locations for vehicles.
* Increased minimum Critical Failure Threshold setting to 95 due to feedback that it was too cumbersome to use a slider with 100 options for a setting with such a small range.

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
