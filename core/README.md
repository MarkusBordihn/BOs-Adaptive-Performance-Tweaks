[![Adaptive Performance Tweaks Downloads](http://cf.way2muchnoise.eu/full_561087_downloads.svg)](https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-core)
[![Adaptive Performance Tweaks Versions](http://cf.way2muchnoise.eu/versions/Minecraft_561087_all.svg)](https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-core)

![Adaptive Performance Tweaks: Core][header]

Adaptive Performance Tweaks (APTweaks) is a collection of Minecraft Forge server-side Mod which automatically adjust specific settings on the server to allow a more balanced TPS/FPS.
The goal of this mod is to allow a smoother experience on a server with several (=> 180) Mods.

If you want to install all modules together use the bundled version:

**➡️[Install the bundled version][bundled]**

## How does it work ❓

Take a look at the additional modules and install all modules which are relevant for your use-case.

## ⚙️ Modules

To allow a better use and mix of the needed features you need, the project is split into several separated modules with a shared core mod.

### 🔩 Core

The core is required for all other adaptive performance tweaks modules and provides a shared api, monitoring functionally, events and commands. It should be automatically installed, if you selected one of the other modules.

**➡️[Core Dependency][core]**

### 🔀 Game Rule Optimizations

Automatically adjust the random tick speed, entity cramming, raids, patrol, trader and insomnia settings based on the current server load.

**➡️[Install Game Rule Optimization][gamerules]**

### ✨🗑️ Item clustering (fallen blocks, experience orbs, ...) and garbage cleanup

Sometimes there are a lot of items lying around in the world which are not picked up by the user or not used at all.
In most cases these are clustered together but they could easily exceed the limit if you are mining and not picking up the items.
The mod automatically try to cluster them further and cleanup items on a regular basis starting with the oldest one and adjust the lifespan based on the server load.
This helps dramatically to allow greater TNT explosions because most of the trashed items are removed after reaching a certain limit.

Experience Orbs could cause a lag from bigger monster farms or if they are not picked up.
They will be automatically clustered in a specific range and combine the experience values.
These optimization happens on the server side without introducing any new custom items.

**➡️[Install Item Optimization][items]**

### 😊 Player Login Optimization and Protection

Sometimes it takes up to 30sec until the player is able to interact with the world after he is logged in.
This mod automatically protects the user for the time between login and to be able to interactive with the world to make sure that he is not attacked by mobs during this time.
Furthermore other smaller optimization will be done to allow a smoother login process.

**➡️[Install Player Optimization][player]**

### 👻Spawn Control and 📦Spawner Optimization

Mob spawns are limited to a specific view area of the player which allows a better distribution and lowers the numbers of entities without a visible effect for the user.
_Example: If you are walking on the surface entities below a specific distance / which are currently not reachable will not be spawn until you are closer to them._

**➡️[Install Spawn Optimization][spawn]**

### 👾Automatic Server Bundle (one mod pack for client/server) and mod cleanup

This mod automatically disable mods which are incompatible or not needed on the server side.
There is no longer the need to have a separated "server" and "client" mod pack only for the mods.
Furthermore it's automatically clean up duplicated files, by removing older versions.
This is helpful to avoid duplication issues and manual deleting / sorting out of older versions.

**➡️[Install Mods Optimization][mods]**

## 🙋 FAQ

### Is this a server side / client side mod❓

The optimization are happening directly on the server side, so you don't need it on the client side.

### Is this a Core Mod❓

**No this is no core mod.** Core mods directly modifying existing Minecraft or Forge implementations which affects all other mods implementations and game mechanics.
For this reason core mods could cause crashes and incompatibility with the existing implementations and other mods.
This mod is only using the existing API functionality and is not modifying any core implementation to be compatible with other mods and game mechanics as much as possible.

### Will it affect other mods❓

In some cases it could be needed to adjust the settings to your preferences but the default should be fine for most casual players.

### What are the general performance gains❓

In general this is hard to say, because it's depends on a lot of factors. In my case it helps me to run a server with about 170 mods.
The mod will not help so much for extensive red stone constructions, custom spawner or other tasks which requires several ticks for the calculation.

### Will it work with Performant, Clumps, Dynamic view, FPS reducer, FerriteCore and Spark❓

You should try to use as less as possible performance mods with overlapping features.
Depending on your mod pack it could be that one works better than the other.
For this reason it's important to test different kind of combination for your specific case.

### Will it work with xyz❓

I'm not able to test all possible mod combination, so you need to test this on your on own to find out.

### 🚩Issues

Please report issues over the **Issue** link.

## Version Status Overview 🛠️

| Version        | Status                |
| -------------- | --------------------- |
| Fabric Version | ❌ Not planned        |
| Forge 1.16.5   | ❌ Not planned        |
| Forge 1.17.1   | ❌ Not planned        |
| Forge 1.18.1   | ⚠️ Maintenance only   |
| Forge 1.18.2   | ⚠️ Maintenance only   |
| Forge 1.19     | ✔️ Active development |

[header]: ../assets/aptweaks-header.png

[bundled]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks
[core]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-core
[gamerules]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-gamerules
[items]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-items
[mods]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-mods
[player]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-player
[spawn]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-spawn
