# Changelog for Adaptive Performance Tweaks (> 1.18.x)

## Note

This change log includes the summarized changes.
For the full changelog, please go to the [Git Hub History][history] instead.

### 2022.05.28

- Improved version sorting for duplicated mods detection.

### 2022.05.21

- Added additional entities to the ignore list for better compatibility to fixing warning messages.
- Removed "Crafting Tweaks" from the client mods list, because it has optional server features.

### 2022.05.19

- Added custom spawn config file for individual settings
- Added default spawn config for Tinkers Construct

### 2022.04.18

- Added additional client side mods to the mods database.

### 2022.04.13

- Improved duplicated mods and client mods detection with exotic 1.18.2 mod names.

### 2022.04.11

- Added Alex's Mob's, Mekanism Additions, Quark and Untamed Wilds individual spawn configuration.
- Improved performance of mods module by ignore specific files and removing additional duplicates.
- Removed view-distance and simulation distance optimizations from player module >= 3.5.0 and referrer to [Dynamic View][dynamic-view], [Farsight][farsight] and [Better FPS] [better-fps-render-distance] instead.

### 2022.04.04

- Added warning and fixed performance issues with caused by other mods and/or specific server configuration which setting logging = ALL which enables all debug messages and cause a additional server load up to 10%.

### 2022.04.03

- Merged latest changes from 1.18.1 to 1.18.2
- Added mod specific configs for Minecraft and Aquaculture 2 to 1.18.2 to allow individual fine tuning for specific mob groups.
  You can easily add or remove specific mob over the mob list in the relevant config files.

### 2022.03.07

- Refactored code for version 1.18.2-40.0.12

### 2022.03.01

- Moved config files to separate directory. NOTE: Please make sure to check the new auto-generated configs.
- Improved entity and position manager to improve tracking of entities and player positions.
- Fixed issue with spawn rules which wrongly triggering "Player Limit" rule when it should not.

### 2022.02.26

- Fixed issue with **FTB Ultimine** mod and other mods which offers similar features.
- Added smaller performance improvements.

### 2022.02.15

- Added smaller performance optimization mostly for memory usage and unnecessary object instances.

### 2022.02.13

- Added game difficult level and game difficult factor to the core module.
- Added ignore dimension list to spawn module which includes "The End" by default.
- Fixed and improved logging messages.

### 2022.01.30

- Added Spawn module to the bundled version.

### 2022.01.29

- Optimized core module and increased event priority to make sure to capture all relevant events.
- Improved pre-filtering of entities
- Added better entity / spawn control logic and filtering
- Added debug command and additional output for all modules
- Added Spawner monitoring

### 2022.01.27

- Added experimental entity / spawn control module.
- Added debug manager for easy activate of the debug output over `/aptweaks debug <module> <enable|disable>`.
- Fixed smaller performance issue.

### 2022.01.26

- Added automatic clean-up every 30 secs for the item cache.
- Optimized kill command to work only inside a specific dimension.
- Preparation for entity / spawn control module
  - Adjusted view area calculation to consider additional factors
  - Added `/aptweaks entity <options> command`
  - Added entity monitoring
  - Added automatic clean-up every 30 secs for the entity / spawn cache.

### 2022.01.23

- Overworked modules and optimized cpu and memory usage
- Added delay for high to low changes to make sure that the sever has more time to catch up.
- Added version restriction to make sure optimized modules are only used with newer versions.
- Added CHANGELOG.md for better overview

### 2022.01.06

- Fixed validation bug.
- Added child player feature for are better player experience for child player
- Update README.md

### 2022.01.04

- Fixed: typos
- Added time between update delay for view distance and simulation distance changes to lower the visual impact for the player.

### 2022.01.03

- Updated versions and optimized packages.
- Bundled version is ready to ship after player module is approved.
- Added Player module to optimize view distance and simulation distance.

### 2022.01.01

- Added bundled version to allow easier transition from 1.16.5 and 17.1.1
- Initial check-in of next gen-version of Adaptive Performance Tweaks.

[history]: https://github.com/MarkusBordihn/BOs-Adaptive-Performance-Tweaks/commits/main

[better-fps-render-distance]: https://www.curseforge.com/minecraft/mc-mods/better-fps-render-distance
[dynamic-view]: https://www.curseforge.com/minecraft/mc-mods/dynamic-view/
[farsight]: https://www.curseforge.com/minecraft/mc-mods/farsight
