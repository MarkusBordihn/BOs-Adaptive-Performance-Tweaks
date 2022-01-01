# Adaptive Performance Tweaks: Items

![Adaptive Performance Tweaks: Items][logo]

Adaptive Performance Tweaks is a collection of Minecraft Forge server-side Mod which automatically adjust specific settings on the server to allow a more balanced TPS/FPS.
The goal of this mod is to allow a smoother experience on a server with several (=> 180) Mods.

### Why is this splitted into several modules ❓

The **APTweaks** is separated into several modules to support all individual needs and to allow the usage of other mods with the same or similar functionality.

It's your mod-pack / server and your rules, pick and mix the optimization mods you want to use.😉

## ✨ Item clustering (fallen blocks, experience orbs, ...) and garbage cleanup 🗑️

Sometimes there are a lot of items lying around in the world which are not picked up by the user or not used at all.
In most cases these are clustered together but they could easily exceed the limit if you are mining and not picking up the items.
The mod automatically try to cluster them further and cleanup items on a regular basis starting with the oldest one and adjust the lifespan based on the server load.
This helps dramatically to allow greater TNT explosions because most of the trashed items are removed after reaching a certain limit.

Experience Orbs could cause a lag from bigger monster farms or if they are not picked up.
They will be automatically clustered in a specific range and combine the experience values.
These optimization happens on the server side without introducing any new custom items.

## How does it work ❓

Take a look at the additional modules and install all modules which are relevant for your use-case.

## Additional Modules

### 🔩 Core

This mod is required for all other modules and provides a core monitoring functionally and additional events.

➡️[Get Core dependency][core]

### 🔀 Game Rule Optimizations

Automatically adjust the random tick speed, entity cramming, raids, patrol, trader and insomnia based on the current server load.

### 😊 Player Login Optimization (wip)

Sometimes it takes up to 30sec until the player is able to interact with the world after he is logged in.
This mod automatically protects the user for the time between login and to be able to interactive with the world to make sure that he is not attacked by mobs during this time.
Furthermore other smaller optimization will be done to allow a smoother login process.

[logo]: src/main/resources/logo.png
[core]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-core
[gamerules]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-gamerules
