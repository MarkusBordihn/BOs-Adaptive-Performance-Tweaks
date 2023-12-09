[![Adaptive Performance Tweaks Downloads](http://cf.way2muchnoise.eu/full_561439_downloads.svg)](https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-gamerules)
[![Adaptive Performance Tweaks Versions](http://cf.way2muchnoise.eu/versions/Minecraft_561439_all.svg)](https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-gamerules)

# Adaptive Performance Tweaks: Items

![Adaptive Performance Tweaks: Items][header]

Adaptive Performance Tweaks (APTweaks) is a collection of Minecraft Forge server-side Mod which
automatically adjust specific settings on the server to allow a more balanced TPS/FPS.
The goal of this mod is to allow a smoother experience on a server with several (=> 180) Mods.

### ✨ Features

- Optimize server performance effortlessly with server-side optimization; client installation is
  entirely optional.
- Customize your experience by selecting the specific optimization modules or features that suit
  your needs.
- Achieve compatibility with other mods seamlessly, as our optimization approach avoids core/Mixin
  modding.
- Enjoy precise control over optimizations through fine-tuning via the user-friendly config files.
- Dive deeper into the intricacies of your server's performance with our built-in debug mode,
  providing comprehensive information about the current state and any applied changes.

**⚠️Please make sure to create regular backup of your world in case something goes wrong.**

## ✨🗑️ Item clustering (fallen blocks, experience orbs, ...) and garbage cleanup

Sometimes there are a lot of items lying around in the world which are not picked up by the user or
not used at all.
In most cases these are clustered together, but they could easily exceed the limit if you are mining
and not picking up the items.
The mod automatically try to cluster them further and cleanup items on a regular basis starting with
the oldest one and adjust the lifespan based on the server load.
This helps dramatically to allow greater TNT explosions because most of the trashed items are
removed after reaching a certain limit.

Experience Orbs could cause a lag from bigger monster farms or if they are not picked up.
They will be automatically clustered in a specific range and combine the experience values.
These optimization happens on the server side without introducing any new custom items.

### Bundled Modules

If you want to install all modules together use the bundled version:

**➡️[Install the bundled version][bundled]**

## ℹ️ Explore Further Details

For in-depth information, explore our wiki available
at https://github.com/MarkusBordihn/BOs-Adaptive-Performance-Tweaks/wiki.
Delve into comprehensive documentation to gain a deeper understanding of the nuances and
functionalities of our project.

## 🚩 Report Issues and Share Feature Requests

Encountering any issues? Navigate to our troubleshooting guide
at https://github.com/MarkusBordihn/BOs-Adaptive-Performance-Tweaks/wiki/Troubleshooting for
comprehensive assistance.

To report issues or propose new features, utilize the Issues link above.
Your feedback is invaluable, and we appreciate your contribution to enhancing the performance and
functionality of
our project.
Thank you for helping us make it even better!

[header]: ../assets/aptweaks-header.png

[bundled]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks
