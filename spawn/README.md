# Adaptive Performance Tweaks: Spawn

![Adaptive Performance Tweaks: Spawn][header]

[![Adaptive Performance Tweaks Downloads](http://cf.way2muchnoise.eu/full_573708_downloads.svg)](https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-spawn)
[![Adaptive Performance Tweaks Versions](http://cf.way2muchnoise.eu/versions/Minecraft_573708_all.svg)](https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-spawn)

Adaptive Performance Tweaks is a collection of Minecraft Forge server-side Mod which automatically adjust specific settings on the server to allow a more balanced TPS/FPS.
The goal of this mod is to allow a smoother experience on a server with several (=> 180) Mods.

## 👾Optimized Mob Spawn Calculations

Mob spawns are limited to a specific view area of the player which allows a better distribution and lowers the numbers of entities without a visible effect for the user.
_Example: If you are walking on the surface entities below a specific distance / which are currently not reachable will not be spawn until you are closer to them._

## 👻Spawn Control

The mod includes a basic spawn control to define the max. number of entity types per player and world with some predefined settings for specific mods.
This includes a deny and allow list to disable specific mobs completely or to exclude them from the optimization.

The spawn-rate is calculated on the following formula:

```math
Number Of players * Max Number Of Entity * Server Load * Game Difficulty
```

You can add your own custom basic spawn control over the `adaptive_performance_tweaks/spawn/CustomSpawn.toml` config file.

**Note: Playing in the game difficulty HARD could exceed the max number of hostile entity from the config file by max. 2x.**

You could also get an overview of all currently loaded monster over `/aptweaks monster`.

## 📦Spawner Optimization

Keeps track of the current number of loaded spawner and perform smaller optimization.
You could also get an overview of all currently loaded spawners over `/aptweaks spawner`.

## 🗄️Config file

With the config file you can disable / enable each of these individual features and adjust the relevant list of affected mobs.

### ✔️Supported Mods

This is a list of the currently supported mods with a custom spawn file for individual spawn settings.

- ✅ [Alex's Mobs][alexmobs]
- ✅ [Aquaculture 2][aquaculture]
- ✅ [Mekanism Additions][mekanismadditions]
- ✅ [Quark][quark]
- ✅ [Tinkers' Construct][tinkersconstruct]
- ✅ [Untamed Wilds][untamedwilds]

You can adjust the spawn settings over the `adaptive_performance_tweaks/spawn/*.toml` config files.

## 🚀Install additional Optimization

### 📦Bundled Version

If you want to install all modules together use the bundled version:

**➡️[Install the bundled version][bundled]**

### ⚙️Customized

If you want to pick and choose your optimization mod, take a look at the core page:

**➡️[Install additional optimization modules][core]**

### 🚩Issues

Please report issues over the **Issue** link.

## Version Status Overview 🛠️

| Version        | Status                |
| -------------- | --------------------- |
| Fabric Version | ❌ Not planned        |
| Forge 1.16.5   | ❌ Not planned        |
| Forge 1.17.1   | ❌ Not planned        |
| Forge 1.18.1   | ⚠️ Deprecated         |
| Forge 1.18.2   | ⚠️ Maintenance only   |
| Forge 1.19     | ⚠️ Deprecated         |
| Forge 1.19.1   | ⚠️ Deprecated         |
| Forge 1.19.2   | ✔️ Active development |

[header]: ../assets/aptweaks-header-only.png
[bundled]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks
[core]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-core
[gamerules]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-gamerules
[items]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-items
[player]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-player
[alexmobs]: https://www.curseforge.com/minecraft/mc-mods/alexs-mobs
[aquaculture]: https://www.curseforge.com/minecraft/mc-mods/aquaculture
[mekanismadditions]: https://www.curseforge.com/minecraft/mc-mods/mekanism-additions
[quark]: https://www.curseforge.com/minecraft/mc-mods/quark
[tinkersconstruct]: https://www.curseforge.com/minecraft/mc-mods/tinkers-construct
[untamedwilds]: https://www.curseforge.com/minecraft/mc-mods/untamedwilds
