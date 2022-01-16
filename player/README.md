[![Adaptive Performance Tweaks Downloads](http://cf.way2muchnoise.eu/full_563963_downloads.svg)](https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-player)
[![Adaptive Performance Tweaks MC Versions](http://cf.way2muchnoise.eu/versions/Minecraft_563963_all.svg)](https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-player)

![Adaptive Performance Tweaks: Player][header]

Adaptive Performance Tweaks is a collection of Minecraft Forge server-side Mod which automatically adjust specific settings on the server to allow a more balanced TPS/FPS.
The goal of this mod is to allow a smoother experience on a server with several (=> 180) Mods.

## 🛡️ Player Login Optimization and Protection

Sometimes it takes up to 30sec until the player is able to interact with the world after he is logged in.
This mod automatically protects the user for the time between login and to be able to interactive with the world to make sure that he is not attacked by mobs during this time.
Furthermore other smaller optimization will be done to allow a smoother login process.

## 🛡️🐨 Child Player Protection

Changes the game-mode of your Child automatically to a more peaceful / easier mode so that they are more enjoying the game even if you are playing in another mode like hardcore.
Just add the child player name's to the configuration to automatically activate this mode for them.

## 🔭 Optimized player view distance

The mod automatically increase or decrease the player view distance depending on the server load of the current world.
This means players different dimensions are not directly affected by the player view distance of other dimensions, if the server load on this dimension is normal.
Worlds / Dimensions without any players will be automatically set to a lower player view distance until players entering the dimension again.

You could adjust the view distance over `/aptweaks setViewDistance <view distance>`.

## ⚙️ Optimized player simulation distance

The mod automatically increase or decrease the player simulation distance depending on the server load of the current world.

You could adjust the view distance over `/aptweaks setSimulationDistance <simulation distance>`.

## Install additional Optimization

### Bundled Version

If you want to install all modules together use the bundled version:

**➡️[Install the bundled version][bundled]**

### Customized

If you want to pick and choose your optimization mod, take a look at the core page:

**➡️[Install separate modules][core]**

### Issues

Please report issues over the **Issue** link.

[header]: ../assets/aptweaks-header.png
[bundled]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks
[core]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-core
[gamerules]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-gamerules
[items]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-items
[player]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-player
