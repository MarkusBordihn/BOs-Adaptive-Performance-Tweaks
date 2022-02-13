# Changelog for Adaptive Performance Tweaks (> 1.18.x)

## Note

This change log includes the summarized changes.
For the full changelog, please go to the [Git Hub History][history] instead.

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
