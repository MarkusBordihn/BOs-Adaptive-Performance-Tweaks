# Adaptive Performance Tweaks

![Adaptive Performance Tweaks][logo]

The Adaptive Performance Tweaks is a Minecraft Forge Mod which automatically adjust specific settings on the server and client side to allow a better TPS/FPS.
The goal of this mod is to allow a smooth experience on a server with several (=> 100) Mods.

**WARNING: This version is an early developer build, please make sure to create regular backup of your world in case something goes wrong.**

## Features

### Optimized player view distance per World

The mod automatically increase or decrease the player view distance depending on the server load of the current world.

This means players in different dimensions are not directly affected by player view distance if the server load on this worlds is normal.

Worlds without any players will be automatically set to a lower player view.

### Player Login Optimization

Sometimes it takes up to 30sec until the player is able to interact with the world after he is logged in. This mod automatically protects the user for the time between login and interactive of the world to make sure that he is not attacked by mobs during this time.

Furthermore other smaller optimization will be done to allow a smoother login process.

### Optimized Mob Spawn Calculations

Mob spawns are limited to a specific view area of the player which allows a better distribution and lowers the numbers of entities without a visible effect for the user.
Example: If you are walking on the surface entities below a specific distance / which are currently not reachable will not be spawn until you are closer to them.

### Spawn Control

The mod includes a basic spawn control to define the max. number of entity types per player and world with some predefined settings for specific mods.
You can tune the spawn control for your setup over the config file.

This includes a deny and allow list to disable specific mobs completely or to exclude them from the optimization.

The spawn-rate is calculated on the following formula:

```math
Number Of players * Max Number Of Entity * Server Load * Game Difficulty
```

Note: Playing with difficulty HARD could exceed the max number of hostile entity from the config file by 1.5x;

### Spawner Optimization

Keeps track of the current number of loaded spawner and perform smaller optimization.
You could also get a overview of all currently loaded spawners over `/aptweaks spawner`.

### Item garbage cleanup

Sometimes there are a lot of items lying around in the world which are not picked up by the user or not used at all.
In most cases these are clustered together but they could easily exceed the limit if you are mining and not picking up the items.
The mod automatically cleanup this items on a regular basis starting with the oldest one and adjust the lifespan based on the server load.
This helps dramatically to allow greater TNT explosions because most of the trashed items are removed after reaching a certain limit.

### Setting file

With the setting file you can disable / enable each of these features.

## Commands

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

### Do you plan to support 12.x, 13.x, 14.x, 15.x or a Fabric version ?

Unfortunately not because of my time constrains and missing knowledge about the api / engine mechanics in these versions.

### Will it affect other mods ?

The spawn control will affect other mods, but for supported mods it should not be noticeable for the user.
In some cases it could be needed to adjust the settings to your preferences but the default should be fine for most casual players.

### What are the general performance gains ?

In general this is hard to say, because it's depends on a lot of factors. In my case it helps me to run a server with about 170 mods.
The mod will not help so much for extensive red stone constructions, custom spawner or other tasks which requires several ticks for the calculation.

[logo]: logo.png
