![Adaptive Performance Tweaks][header]

# Adaptive Performance Tweaks (APTweaks)

> ⚠ **Alpha Version** — 12.x is currently in alpha. Please report issues on GitHub.
>
> 💾 **Back up your world and config files before installing 12.x.** As an alpha release,
> breaking changes between versions can occur. Restoring a backup may be necessary after updates.
>
> 🔄 **Upgrading from 11.x?** — **Remove all 11.x modules before installing 12.x.**
> This includes: `adaptive-performance-tweaks-core`, `-spawn`, `-gamerules`, `-items`, `-player`,
> and the bundle.
> Installing both versions simultaneously will cause conflicts.

Adaptive Performance Tweaks is a **single mod** for **Forge and Fabric** that automatically adjusts
server-side settings to maintain smooth TPS under load.
It replaces the entire 11.x module system with one unified mod.

## ✨ What's new in 12.x

### 🔌 Multi-loader support

Supports Fabric, Forge und NeoForge.

### 📦 All modules included

Spawn control, game rule tuning, item/XP orb optimization, player login protection, view/sim
distance adaptation, and server load monitoring are all bundled.

### ⚡ Pre-creation spawn check

The most significant performance improvement over 11.x: mobs are denied **before** a Minecraft
entity ID is allocated and before the entity enters the world. In 11.x, the mod cancelled the spawn
event after the entity had already joined the level — wasting entity ID space and triggering
unnecessary game logic. The 12.x architecture intercepts at the natural spawner level, meaning zero
overhead for denied spawns.

### 🧪 Automated game tests

Every build runs 38 game tests on a headless Fabric server, covering spawn limits, load scaling,
friendly chunk bypass, and all major features. This ensures regressions are caught before release.

## 📥 Installation

1. Remove any installed 11.x APTweaks modules (see warning above)
2. Place the `adaptive-performance-tweaks-12.x.x-forge.jar` **or** `...-fabric.jar` in your `mods/`
   folder
3. Start the server — default config files are generated automatically

Default limits are tuned for **~4 players**. For larger servers, increase `per_player_max` and
`per_world_max` in the spawn preset JSON files
(`config/adaptive_performance_tweaks/spawn_presets/`).

## 🧩 Modules

| Module              | Description                                                                           |
|---------------------|---------------------------------------------------------------------------------------|
| Spawn               | Per-entity-type limits with load-adaptive scaling and friendly chunk bypass           |
| Game Rules          | Automatic adjustment of randomTickSpeed, entityCramming, and related rules under load |
| Items               | Item entity merge and lifetime reduction under load                                   |
| Experience Orbs     | XP orb clustering to reduce entity count                                              |
| View Distance       | Adaptive view distance scaling based on server load                                   |
| Simulation Distance | Adaptive simulation distance scaling                                                  |
| Player Protection   | Configurable login invulnerability and invisibility period                            |
| Monitoring          | Periodic server load and spawn stats logging                                          |

## ⚙️ Configuration

All config files are generated in `config/adaptive_performance_tweaks/` on first start.
Spawn presets live in `config/adaptive_performance_tweaks/spawn_presets/` and can be added or
overridden per server. Datapacks can also provide presets via
`data/<namespace>/aptweaks/spawn_presets/`.

See the [wiki](https://github.com/MarkusBordihn/BOs-Adaptive-Performance-Tweaks/wiki) for
configuration details, module descriptions, and troubleshooting.

## 📜 Version overview

See [wiki/Versions.md](wiki/Versions.md) for a comparison of 12.x vs 11.x and the reasons behind
the architecture change.

[header]: https://github.com/MarkusBordihn/BOs-Adaptive-Performance-Tweaks/wiki/images/aptweaks-header-only.png
