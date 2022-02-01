[![Adaptive Performance Tweaks Downloads](http://cf.way2muchnoise.eu/full_adaptive-performance-tweaks_downloads.svg)](https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks)
[![Adaptive Performance Tweaks Versions](http://cf.way2muchnoise.eu/versions/Minecraft_adaptive-performance-tweaks_all.svg)](https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks)

**⚠️ Note: For version 1.18.1 and higher, please got to [BOs-Adaptive-Performance-Tweaks][adaptive_performance_tweaks] instead.**

**The 1.16.5 version is no longer actively developed and will be only receiving maintenance updates.**

# Adaptive Performance Tweaks 1.16.5 (legacy version)

![Adaptive Performance Tweaks][logo]

Adaptive Performance Tweaks is a Minecraft Forge server-side Mod which automatically adjust specific settings on the server to allow a more balanced TPS/FPS.
The goal of this mod is to allow a smoother experience on a server with several (=> 180) Mods.

**⚠️Please make sure to create regular backup of your world in case something goes wrong.**

## 🚀Features

### 🔭Optimized player view distance per World

The mod automatically increase or decrease the player view distance depending on the server load of the current world.
This means players different dimensions are not directly affected by the player view distance of other dimensions, if the server load on this dimension is normal.
Worlds / Dimensions without any players will be automatically set to a lower player view distance until players entering the dimension again.

### 😊Player Login Optimization

Sometimes it takes up to 30sec until the player is able to interact with the world after he is logged in.
This mod automatically protects the user for the time between login and to be able to interactive with the world to make sure that he is not attacked by mobs during this time.
Furthermore other smaller optimization will be done to allow a smoother login process.

### 👾Optimized Mob Spawn Calculations

Mob spawns are limited to a specific view area of the player which allows a better distribution and lowers the numbers of entities without a visible effect for the user.
_Example: If you are walking on the surface entities below a specific distance / which are currently not reachable mobs will not be spawn until you are closer to them._

### 👻Spawn Control

The mod includes a basic spawn control to define the max. number of entity types per player and world with some predefined settings for specific mods.
You could tune the spawn control for your specific setup over the config file.

This includes a deny and allow list to disable specific mobs completely or to exclude them from the optimization.

The spawn-rate is calculated on the following formula:

```math
Number Of players * Max Number Of Entity * Server Load * Game Difficulty
```

**Note: Playing in the game difficulty HARD could exceed the max number of hostile entity from the config file by max. 1.5x.**

You could also get an overview of all currently loaded monster over `/aptweaks monster`.

### 📦Spawner Optimization

Keeps track of the current number of loaded spawner and perform smaller optimization.
You could also get an overview of all currently loaded spawners over `/aptweaks spawner`.

### 🗑️Item garbage cleanup

Sometimes there are a lot of items lying around in the world which are not picked up by the user or not used at all.
In most cases these are clustered together but they could easily exceed the limit if you are mining and not picking up the items.
The mod automatically cleanup this items on a regular basis starting with the oldest one and adjust the lifespan based on the server load.
This helps dramatically to allow greater TNT explosions because most of the trashed items are removed after reaching a certain limit.

You could also get an overview of all current items over `/aptweaks items`.

### ✨Experience Orbs clustering

Experience Orbs could cause a lag from bigger monster farms or if they are not picked up.
The mod automatically cluster experience orbs in a specific range and combine the experience values.
This optimization happens on the server side without introducing any new custom items.

### 🔀randomTickSpeed and maxEntityCramming Optimization

Automatically adjust the randomTickSpeed and maxEntityCramming based on the current server load.

## 🗄️Setting file

With the setting file you can disable / enable each of these individual features.

### ✔️Supported Mods

This is a list of the currently supported mods within the config file for individual settings.

- ✅ [Alex's Mobs][alexmobs]
- ✅ [Aquaculture 2][aquaculture]
- ✅ [Artifacts][artifacts]
- ✅ [Dungeons Mod][dungeons_mod]
- ✅ [Ice and Fire: Dragons][iceandfire]
- ✅ [Mekanism Additions][mekanismadditions]
- ✅ [Mowzie's Mobs][mowziesmobs]
- ✅ [Mutant Beasts][mutantbeasts]
- ✅ [Quark][quark]
- ✅ [Savage & Ravage][savageandravage]
- ✅ [Statues][statues]
- ✅ [Supplementaries][supplementaries]
- ✅ [The Abyss: Chapter II][theabyss]
- ✅ [The Farlanders][thefarlanders]
- ✅ [The Twilight Forest][twilightforest]
- ✅ [Tinkers' Construct][tinkersconstruct]
- ✅ [Untamed Wilds][untamedwilds]
- ✅ [Whisperwoods][whisperwoods]

### ⛔ Unsupported Mods

The following mods are unsupported because of using outdated and/or unexpected ways (mixin) of spawning mobs.
You can test them and see it they are working but I will not offer any support on these.

- ❌ [BetterEnd (Forge)][betterend-forge-port]
- ❌ [Environmental][environmental]
- ❌ [Project: Vibrant Journeys][project-vibrant-journeys]
- ❌ [Rats][rats]
- ❌ [The Endergetic Expansion][endergetic]
- ❌ [Upgrade Aquatic][upgrade-aquatic]

## 🎱Commands

### 👁️‍🗨️Items Usage

`/aptweaks items` shows an overview about the number of currently loaded items.

`/aptweaks items optimize` runs manual Items Optimization, normally not needed.

### 👁️‍🗨️Entity Usage

`/aptweaks entity overview` shows an overview about the number of currently loaded entities.

`/aptweaks entity registry` shows all know entities from the registry.

### 👁️‍🗨️Monster Usage

`/aptweaks monster` shows an overview about the number of currently loaded monsters.

### 👁️‍🗨️Spawner Usage

`/aptweaks spawner` shows an overview about the number of currently loaded mob spawners.

### 👁️‍🗨️Kill Command

`/aptweaks kill entities` kill all entities except players.

`/aptweaks kill items` kill all item entities.

### 👁️‍🗨️Spawn rules

`/aptweaks spawnRules` shows the pre-calculated spawn rules.

### 👁️‍🗨️Special Spawn rules

`/aptweaks specialSpawnRules` shows the pre-calculated special spawn rules.

### 👁️‍🗨️Player Positions

`/aptweaks playerPositions` shows the current player position and meta data.

### 👁️‍🗨️Debug

The debug is helpful to understand an unexpected behave like to many or to less spawned mobs.

`/aptweaks debug true` enables the debugging which will logged into debug.log file.

`/aptweaks debug false` disables the debugging.

## 🙋FAQ

### Is this a server side / client side mod❓

The optimization are happening directly on the server side, so you don't need it on the client side.
At a later stage I will add additional client side modifications.

### Do you plan to support 12.x, 13.x, 14.x, 15.x or a Fabric/Rift version❓

Unfortunately not because of my time constrains and missing knowledge about the api / engine mechanics in these versions.

### Is this a Core Mod❓

**No this is no core mod.** Core mods directly modifying existing Minecraft or Forge implementations which affects all other mods implementations and game mechanics.
For this reason core mods could cause crashes and incompatibility with the existing implementations and other mods.
This mod is only using the existing API functionality and is not modifying any core implementation to be compatible with other mods and game mechanics as much as possible.

### Will it affect other mods❓

The spawn control will affect other mods, but for supported mods it should not be noticeable for the user.
In some cases it could be needed to adjust the settings to your preferences but the default should be fine for most casual players.

### Will it affect spawners❓

Most of the optimization are happing for the natural spawn, which mean that spawners should not be affected by most of the optimization.
I'm currently testing a way to better optimize the spawners as well, because they using most of the resources.

### What are the general performance gains❓

In general this is hard to say, because it's depends on a lot of factors. In my case it helps me to run a server with about 170 mods.
The mod will not help so much for extensive red stone constructions, custom spawner or other tasks which requires several ticks for the calculation.

## Will it work with Performant, Clumps, Dynamic view, FPS reducer, FerriteCore and Spark❓

You should try to use as less as possible performance mods with overlapping features.
Depending on your mod pack it could be that one works better than the other.
For this reason it's important to test different kind of combination for your specific case.
These recommendations are based on my experience and my knowledge about other mods.

### ❌Performant

**Performant** is a core mod, which means it is directly modifying existing Minecraft or Forge implementations which affects all other mods implementations and game mechanics. This means all modification from such core mods are applied and happen before the optimization of this mod are taking into effect.

Technically they should work together because they are performing different kind of optimization on different levels.
But I'm not recommending using both because the combined optimization overhead is too much.

Because core mods modifying the underlying integration it could be that they are producing crashes which could be only fixed from the core mod author itself and not my side.

### ⚠️Clumps

**Clumps** groups XP orbs together into a new single entity. The cluster feature of this mod whoever do this on the server side without introducing new entities or items.
I recommend not using both mods together, however if you prefer Clumps, please disable the experience orb feature over the config file.

### ⚠️Dynamic View

**Dynamic view** adjusts the view distance for all worlds to the same value. The view distance feature of this mod adjusts the view distance per world instead and current load instead.
I recommend not using both mods together, however if you prefer Dynamic view, please disable the view distances feature over the config file.

### ⚠️InControl

**InControl** controls the mob spawns and entity spawns, which could conflict with this spawn control of this mod.
Don't use both optimizations together.

### ✅FPS reducer

**FPS reducer** is a great client side mod and I'm not planning to implement any overlapping features, so you should definitely use it.

### ✅FerriteCore

**FerriteCore** is a great memory optimization mod for the server and client side and I recommend to use it together with this mod.

### ✅Spark

**Spark** is a great performance profiling mod and is not performing any optimization on it own.
But I still recommend this mod to make it easier to find lagging entities or to identify general performance issues.

## ⭐Recommended Server Mods

- [FTB Backups][ftb_backups] for regular automated backups.
- [FerriteCore][ferrite_core] This mod reduces the memory usage of Minecraft in a few different ways.
- [RandomPatches (Forge)][random_patches] Collection of bug fixes and quality of life improvements for Minecraft
- [Spark][spark] Performance profiling plugin/mod for Minecraft clients, servers and proxies.

## ⭐Recommended Client Mods

- [FPS Reducer][fps_reducer] Reduces unnecessary GPU and CPU load when you are not operating the Minecraft for a certain period of time.
- [FerriteCore][ferrite_core] This mod reduces the memory usage of Minecraft in a few different ways.
- [RandomPatches (Forge)][random_patches] Collection of bug fixes and quality of life improvements for Minecraft
- [Spark][spark] Performance profiling plugin/mod for Minecraft clients, servers and proxies.

[adaptive_performance_tweaks]: https://github.com/MarkusBordihn/BOs-Adaptive-Performance-Tweaks

[alexmobs]: https://www.curseforge.com/minecraft/mc-mods/alexs-mobs
[aquaculture]: https://www.curseforge.com/minecraft/mc-mods/aquaculture
[artifacts]: https://www.curseforge.com/minecraft/mc-mods/artifacts
[betterend-forge-port]: https://www.curseforge.com/minecraft/mc-mods/betterend-forge-port
[dungeons_mod]: https://www.curseforge.com/minecraft/mc-mods/dungeons-mod
[endergetic]: https://www.curseforge.com/minecraft/mc-mods/endergetic
[environmental]: https://www.curseforge.com/minecraft/mc-mods/environmental
[ferrite_core]: https://www.curseforge.com/minecraft/mc-mods/ferritecore
[fps_reducer]: https://www.curseforge.com/minecraft/mc-mods/fps-reducer
[ftb_backups]: https://www.curseforge.com/minecraft/mc-mods/ftb-backups
[iceandfire]: https://www.curseforge.com/minecraft/mc-mods/ice-and-fire-dragons
[mekanismadditions]: https://www.curseforge.com/minecraft/mc-mods/mekanism-additions
[mowziesmobs]: https://www.curseforge.com/minecraft/mc-mods/mowzies-mobs
[mutantbeasts]: https://www.curseforge.com/minecraft/mc-mods/mutant-beasts
[project-vibrant-journeys]: https://www.curseforge.com/minecraft/mc-mods/project-vibrant-journeys
[quark]: https://www.curseforge.com/minecraft/mc-mods/quark
[random_patches]: https://www.curseforge.com/minecraft/mc-mods/randompatches-forge
[rats]: https://www.curseforge.com/minecraft/mc-mods/rats
[savageandravage]: https://www.curseforge.com/minecraft/mc-mods/savage-and-ravage
[spark]: https://www.curseforge.com/minecraft/mc-mods/spark
[statues]: https://www.curseforge.com/minecraft/mc-mods/statues
[supplementaries]: https://www.curseforge.com/minecraft/mc-mods/supplementaries
[theabyss]: https://www.curseforge.com/minecraft/mc-mods/the-abyss-chapter-ii
[thefarlanders]: https://www.curseforge.com/minecraft/mc-mods/farlanders
[tinkersconstruct]: https://www.curseforge.com/minecraft/mc-mods/tinkers-construct
[twilightforest]: https://www.curseforge.com/minecraft/mc-mods/the-twilight-forest
[untamedwilds]: https://www.curseforge.com/minecraft/mc-mods/untamedwilds
[upgrade-aquatic]: https://www.curseforge.com/minecraft/mc-mods/upgrade-aquatic
[whisperwoods]: https://www.curseforge.com/minecraft/mc-mods/whisperwoods

[logo]: src/main/resources/logo.png
