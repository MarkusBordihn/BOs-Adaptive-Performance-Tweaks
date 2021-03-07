# Adaptive Performance Tweaks

![Adaptive Performance Tweaks][logo]

Adaptive Performance Tweaks is a Minecraft Forge Mod which automatically adjust specific settings on the server and client side to allow a balanced TPS/FPS.
The goal of this mod is to allow a smooth experience on a server with several (=> 100) Mods.

**WARNING: Please make sure to create regular backup of your world in case something goes wrong.**

## Features

### Optimized player view distance per World

The mod automatically increase or decrease the player view distance depending on the server load of the current world.
This means players different dimensions are not directly affected by the player view distance of other dimensions, if the server load on this dimension is normal.
Worlds / Dimensions without any players will be automatically set to a lower player view distance until players entering the dimension again.

### Player Login Optimization

Sometimes it takes up to 30sec until the player is able to interact with the world after he is logged in.
This mod automatically protects the user for the time between login and to be able to interactive with the world to make sure that he is not attacked by mobs during this time.
Furthermore other smaller optimization will be done to allow a smoother login process.

### Optimized Mob Spawn Calculations

Mob spawns are limited to a specific view area of the player which allows a better distribution and lowers the numbers of entities without a visible effect for the user.
_Example: If you are walking on the surface entities below a specific distance / which are currently not reachable mobs will not be spawn until you are closer to them._

### Spawn Control

The mod includes a basic spawn control to define the max. number of entity types per player and world with some predefined settings for specific mods.
You could tune the spawn control for your specific setup over the config file.

This includes a deny and allow list to disable specific mobs completely or to exclude them from the optimization.

The spawn-rate is calculated on the following formula:

```math
Number Of players * Max Number Of Entity * Server Load * Game Difficulty
```

**Note: Playing in the game difficulty HARD could exceed the max number of hostile entity from the config file by max. 1.5x.**

You could also get an overview of all currently loaded monster over `/aptweaks monster`.

### Spawner Optimization

Keeps track of the current number of loaded spawner and perform smaller optimization.
You could also get an overview of all currently loaded spawners over `/aptweaks spawner`.

### Item garbage cleanup

Sometimes there are a lot of items lying around in the world which are not picked up by the user or not used at all.
In most cases these are clustered together but they could easily exceed the limit if you are mining and not picking up the items.
The mod automatically cleanup this items on a regular basis starting with the oldest one and adjust the lifespan based on the server load.
This helps dramatically to allow greater TNT explosions because most of the trashed items are removed after reaching a certain limit.

You could also get an overview of all current items over `/aptweaks items`.

### randomTickSpeed and maxEntityCramming Optimization

Automatically adjust the randomTickSpeed and maxEntityCramming based on the current server load.

## Setting file

With the setting file you can disable / enable each of these individual features.

### Supported Mods

This is a list of the currently supported mods within the config file for individual settings.

- [Aquaculture 2][aquaculture]
- [Dungeons Mod][dungeons_mod]
- [Ice and Fire: Dragons][iceandfire]
- [Mekanism Additions][mekanismadditions]
- [Quark][quark]
- [Savage & Ravage][savageandravage]
- [The Abyss: Chapter II][theabyss]

## Commands

### Items Usage

`/aptweaks items` shows an overview about the number of currently loaded items.
`/aptweaks items optimize` runs manual Items Optimization, normally not needed.

### Monster Usage

`/aptweaks monster` shows an overview about the number of currently loaded monsters.

### Spawner Usage

`/aptweaks spawner` shows an overview about the number of currently loaded mob spawners.

### Debug

`/aptweaks debug true` enables the debugging which will logged into debug.log file.
`/aptweaks debug false` disables the debugging.

### Memory Usage

`/aptweaks memory` shows the current memory usage

### Threads Usage

`/aptweaks threads` shows the current threads (cpu) usage.

## FAQ

### Is this a server side / client side mod ?

At the moment most of the optimization are server side, so you don't need it on the client side.
At a later stage I will add additional client side modifications.

### Do you plan to support 12.x, 13.x, 14.x, 15.x or a Fabric/Rift version ?

Unfortunately not because of my time constrains and missing knowledge about the api / engine mechanics in these versions.

### Is this a Core Mod ?

**No this is no core mod.** Core mods directly modifying existing Minecraft or Forge implementations which affects all other mods implementations and game mechanics.
For this reason core mods could cause crashes and incompatibility with the existing implementations and other mods.
This mod is only using the existing API functionality and is not modifying any core implementation to be compatible with other mods and game mechanics as much as possible.

### Will it affect other mods ?

The spawn control will affect other mods, but for supported mods it should not be noticeable for the user.
In some cases it could be needed to adjust the settings to your preferences but the default should be fine for most casual players.

### What are the general performance gains ?

In general this is hard to say, because it's depends on a lot of factors. In my case it helps me to run a server with about 170 mods.
The mod will not help so much for extensive red stone constructions, custom spawner or other tasks which requires several ticks for the calculation.

## Recommended Server Mods

- [FTB Backups][ftb_backups] for regular automated backups.
- [Clumps][clumps] groups XP orbs together into a single entity.
- [RandomPatches (Forge)][random_patches] Collection of bug fixes and quality of life improvements for Minecraft

[logo]: logo.png
[aquaculture]: https://www.curseforge.com/minecraft/mc-mods/aquaculture
[clumps]: https://www.curseforge.com/minecraft/mc-mods/clumps
[dungeons_mod]: https://www.curseforge.com/minecraft/mc-mods/dungeons-mod
[ftb_backups]: https://www.curseforge.com/minecraft/mc-mods/ftb-backups
[iceandfire]: https://www.curseforge.com/minecraft/mc-mods/ice-and-fire-dragons
[mekanismadditions]: https://www.curseforge.com/minecraft/mc-mods/mekanism-additions
[quark]: https://www.curseforge.com/minecraft/mc-mods/quark
[random_patches]: https://www.curseforge.com/minecraft/mc-mods/randompatches-forge
[savageandravage]: https://www.curseforge.com/minecraft/mc-mods/savage-and-ravage
[theabyss]: https://www.curseforge.com/minecraft/mc-mods/the-abyss-chapter-ii
