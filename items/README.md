[![Adaptive Performance Tweaks Downloads](http://cf.way2muchnoise.eu/full_561439_downloads.svg)](https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-gamerules)
[![Adaptive Performance Tweaks MC Versions](http://cf.way2muchnoise.eu/versions/Minecraft_561439_all.svg)](https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-gamerules)

![Adaptive Performance Tweaks: Game Rules][header]

Adaptive Performance Tweaks is a collection of Minecraft Forge server-side Mod which automatically adjust specific settings on the server to allow a more balanced TPS/FPS.
The goal of this mod is to allow a smoother experience on a server with several (=> 180) Mods.
Adaptive Performance Tweaks is a collection of Minecraft Forge server-side Mod which automatically adjust specific settings on the server to allow a more balanced TPS/FPS.
The goal of this mod is to allow a smoother experience on a server with several (=> 180) Mods.

## ✨🗑️ Item clustering (fallen blocks, experience orbs, ...) and garbage cleanup

Sometimes there are a lot of items lying around in the world which are not picked up by the user or not used at all.
In most cases these are clustered together but they could easily exceed the limit if you are mining and not picking up the items.
The mod automatically try to cluster them further and cleanup items on a regular basis starting with the oldest one and adjust the lifespan based on the server load.
This helps dramatically to allow greater TNT explosions because most of the trashed items are removed after reaching a certain limit.

Experience Orbs could cause a lag from bigger monster farms or if they are not picked up.
They will be automatically clustered in a specific range and combine the experience values.
These optimization happens on the server side without introducing any new custom items.

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

[core]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-core
[bundled]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks
[gamerules]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-gamerules
[items]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-items
