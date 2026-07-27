# Changelog for Adaptive Performance Tweaks 1.21.1

## Note

This change log includes the summarized changes. For the full changelog, please go to
the [GitHub History][history] instead.

### 12.8.0

- Fixed #91 by letting the login and teleport warmup decay back to the load-based distance.
- Fixed view and simulation distance never recovering while a player kept walking around.
- Fixed simulation distance never recovering from a warmup at NORMAL load and above.
- Changed `movementThrottleRecoverOnlyWhenStable` to wait for the movement threshold rather than a
  motionless player; only applies with `movementThrottleEnabled=false`.
- Changed movement throttling to a speed in blocks/s, replacing the distance-threshold settings with
  `movementThrottleSpeedBlocksPerSecond`/`movementSpeedBlocksPerSecond` (removed on load).
- Changed movement throttling to ignore vertical movement; falling and mining down no longer count.
- Added a second throttle stage for fast transit, applying max reduction at any load, via
  `movementThrottleFastSpeedBlocksPerSecond`/`movementFastSpeedBlocksPerSecond`.
- Increased code readability.

### 12.7.0

- Fixed login protection Invulnerable/Invisible flags persisting across sessions after a
  mid-protection disconnect. Thanks to `@Anscylla`.
- Fixed Fabric item/orb limits removing entities after they already joined the world.

### 12.6.0

- Fixed item merge losing the remainder when a drop only partially fits a nearby stack.
- Fixed arrow cleanup removing thrown tridents and pickup-able player arrows; protected projectiles
  now use a separate hard cap (tridents removed last).
- Fixed game rules not being restored on shutdown, persisting optimized values (randomTickSpeed,
  doFireTick, ...) into level.dat and corrupting the baseline on restart.
- Fixed /aptweaks feature not applying enable/disable transitions (stale view/simulation distance
  and game rules).
- Fixed monitoringIntervalSeconds=0 silencing monitoring instead of logging on load changes.
- Fixed dimensions.ignore in spawn presets falling back to global limits instead of exempting it.
- Fixed server load rising-check comparing against a two-cycles-old measurement.
- Fixed login protection clearing invisibility/invulnerability set by game modes or other mods.
- Added entity tag support (#namespace:tag) in spawn preset allow/deny lists.
- Added protection for named/enchanted/NBT items in world/type limits with a 2x hard cap.
- Added wiring for minOptimizationLoadLevel in simulation distance and game rules.
- Added validation for core load thresholds and damage reduction percentages.
- Improved AI throttling to skip only goal/navigation/sensing, keeping physics, item pickup and
  despawning intact.
- Improved game-rule movement warmup to work without an active distance feature.
- Improved config reload to log mod conflict warnings only once.
- Improved entity tracking report to write asynchronously and mob-farm cleanup to use live entity
  positions.
- Improved benchmark heap metric: GC-collapsed measurements now report "n/a (GC)" instead of a
  misleading 0KB delta.
- Removed unused NeoForge-style mixin entries from the Forge mods.toml.

### 12.5.0

- Fixed benchmark report not covering all features and adjustments.
- Added Exploration scenario and update benchmark scenarios.
- Improved simulation distance features and set default to AUTO with better load-based adjustments.

### 12.4.0

- Added player warmup for simulation distance and random tick speed on login, teleport and fast
  movement.
- Added config toggles for login and movement warmup independent of minOptimizationLoadLevel.
- Improved load-based simulation distance changes with step-by-step reductions.
- Improved warmup recovery to only restore values below normal server load.
- Improved reset handling and tests for restoring server default values.

### 12.3.0

- Added more detailed MSPT buckets and improved reporting.
- Added smaller performance optimizations and removed some redundant code.
- Added additional benchmark scenarios for better performance analysis and optimization.
- Improved benchmark reporting and save result to file.
- Improved entity report and store results outside of config folder.
- Improved memory usage and reduced string concatenation in maps.

### 12.2.0

- Fixed #89 compatibility for many popular mods with special entities.
- Fixed #88 and #84 by providing NeoForge version and fixed related issues.
- Fixed #49 Ars Nouveau mob spawn handling and compatibility.
- Fixed benchmark crashes with chunk optimization mods.
- Fixed features are not completely disabled and downgrading benchmark phase 2 results.
- Fixed the incorrect Forge "missing on client" log message on clients.
- Added entity exclusion metadata (mod, category, reason) and entity tracking reports.
- Added more third-party compatibility checks.
- Added stuck arrow cleanup.
- Added spawn-type-based handling for structures, chunk generation, reinforcements...
- Added movement-aware simulation distance throttling.
- Added more spawn presets for known mods and entities.
- Added the missing "Eco Stack Manager" integration for the 12.x feature set.
- Added unit and game tests for the new features and optimizations.
- Improved benchmark measurement accuracy and result presentation.
- Improved logging, server load summaries, and stats output while reducing noisy output.

### 12.1.0

- Fixed issue with server load measurement per world was not accurate enough.
- Fixed smaller memory leak in the server load measurement code.
- Fixed log spam issue with auto-detected mods.
- Fixed Forge Mixin issue with missing mixin configuration file.
- Added auto-benchmarking tools for better performance analysis and optimization.
- Improved stats command to provide more detailed information about server load and performance.
- Improved Fabric performance by optimizing existing code to Fabric entity pipeline.

### 12.0.0 🚀

Next major alpha release with refactored code and improved performance. Now available for Fabric,
Forge and NeoForge with a single codebase and better maintainability.

[history]: https://github.com/MarkusBordihn/BOs-Adaptive-Performance-Tweaks/commits/main
