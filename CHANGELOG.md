# Changelog for Adaptive Performance Tweaks (1.16.5)

## Note

This change log includes the summarized changes.
For the full changelog, please go to the [Git Hub History][history] instead.

### 2022.03.26

- Fix reported warning and errors from mana-and-artifice mod in the log.

### 2022.02.01

- Released updated 2.0.0 version, which will be the last major update for the 1.16.5 version with backports from the 1.18.1 version.
- Backported change to limited over-population from "unknown" mods.
  - Added new config fields under "Spawn Limitations" like spawnLimitationLimiter, spawnLimitationMaxMobsPerPlayer, spawnLimitationMaxMobsPerWorld
  - This change solves a lot of issue with "unsupported" mods, but not all if they are not using the normal ways of spawning mobs.
  - This additional optimization could be disabled over the config and the spawnLimitationLimiter.
- Updated references which are pointing to the old repro.

### 2022.01.30

- Updated 1.16.5 version and improved cache handler for a better garbage collection.
- Added additional checks to exclude minecart-chest or minecart-chest like containers (e.g. Lootr) from optimization.

### 2022.01.01

- Added note for the next-gen version for 1.18.1

### 2021.12.17

- Fixed java.util.concurrent.ConcurrentHashMap$MapEntry cannot be cast to net.minecraft.entity.monster.CreeperEntity
- Backported some of the changes from 1.17.1 to 1.16.5

### 2021.11.03

- Added support for "Twist" mod

### 2021.09.18

- Added support for "The Farmlanders" mod

### 2021.08.22

- Optimized Code and removed dead / outdated code

### 2021.08.21

- Fixed optimized player position issue, where player position is not updated and leads to ghost positions.

### 2021.07.02

- Fixed wrongly activated debug mode, which affects server performance by about 3%-10%

### 2021.06.15

- Added stricter type checks and added experimental support for "Untamed Wilds" mod.

### 2021.05.28

- Fixed issue where Fishing BobberEntities are removed and improved logging.
- Fixed caching issues and memory leak.

### 2021.05.27

- Improved player positions and consider additional factors

### 2021.05.26

- Added Item Entity clustering and automatic reload for spawn rules.

### 2021.05.25

- Added XP orb server side clustering and fixed smaller issues.

### 2021.05.24

- Added additional rules and improves to allow a less aggressive filtering or mobs.

### 2021.05.08

- Added debugging function and fix smaller issues.

### 2021.05.04

- Added support for "Mutant Beasts" mod
- Added safety check for Mine Colonies for minEntityCramming
- Fixed crash with null pointer error

### 2021.04.29

- Merged Game Rule changes from Tatara88

### 2021.04.27

- Added simple check to ignore custom entities without a real name

### 2021.04.19

- Optimized code and conditions
- Restructured Spawn Manager to optimize Player Position and consider additional use-cases.
- Removed less relevant commands like memory and cpu usage, because these are covered by other mods.
- Included mod support:
  - Alex's Mobs
  - Artifacts
  - Mowzie's Mobs
  - Statues
  - Supplementaries
  - Tinkers' Construct
  - The Twillight Forst

### 2021.04.04

- Fixed user reported issue #3

### 2021.04.03

- Improved documentation

### 2021.03.25

- Disabled experimental feature and adding switch to the common config.

### 2021.03.07

- Fixed issue with invisible dropped items.
- Fixed # and optimized item handling in general.

### 2021.03.06

- Added support for the "Dungeons" mod

### 2021.03.04

- Fixed map / set exception for clean up items by using are more optimized workflow.

### 2021.03.03

- Refactored mod spawn config and consider edge cases like more enderman in the end.

### 2021.03.01

- Improved internal calculations
- Added additional tweaks and ChunkManager, MonsterManager and GameRule Manager.
- Added new commands

### 2021.02.27

- First public release of the working developer versions.

[history]: https://github.com/MarkusBordihn/BOs-Adaptive-Performance-Tweaks/commits/1.16.5
