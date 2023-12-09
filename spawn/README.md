[![Adaptive Performance Tweaks Downloads](http://cf.way2muchnoise.eu/full_573708_downloads.svg)](https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-spawn)
[![Adaptive Performance Tweaks Versions](http://cf.way2muchnoise.eu/versions/Minecraft_573708_all.svg)](https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-spawn)

# Adaptive Performance Tweaks: Spawn

![Adaptive Performance Tweaks: Spawn][header]

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

## 👾Optimized Mob Spawn Calculations

Mob spawns are limited to a specific view area of the player which allows a better distribution and
lowers the numbers of entities without a visible effect for the user.
_Example: If you are walking on the surface entities below a specific distance / which are currently
not reachable will not be spawn until you are closer to them._

## 👻Spawn Control

The mod includes a basic spawn control to define the max. number of entity types per player and
world with some predefined settings for specific mods.
This includes a denied and allow list to disable specific mobs completely or to exclude them from
the
optimization.

The spawn-rate is calculated on the following formula:

```math
Number Of players * Max Number Of Entity * Server Load * Game Difficulty
```

You can add your own custom basic spawn control over
the `adaptive_performance_tweaks/spawn/CustomSpawn.toml` config file.

**Note: Playing in the game difficulty HARD could exceed the max number of hostile entity from the
config file by max. 2x.**

You could also get an overview of all currently loaded monster over `/aptweaks monster`.

## 📦Spawner Optimization

Keeps track of the current number of loaded spawner and perform smaller optimization.
You could also get an overview of all currently loaded spawners over `/aptweaks spawner`.

## 🗄️Config file

With the config file you can disable / enable each of these individual features and adjust the
relevant list of affected mobs.

### ✔️Supported Mods

This is a list of the currently supported mods with a custom spawn file for individual spawn
settings.

- ✅ Alex's Mobs
- ✅ Aquaculture 2
- ✅ Born in Chaos
- ✅ Fish of Thieves
- ✅ Friends and Foes
- ✅ Gothic
- ✅ Internal Expansion
- ✅ Mekanism Additions
- ✅ Panthalassa
- ✅ Quark
- ✅ Tinkers' Construct
- ✅ Untamed Wilds
- ✅ Untitled Duck
- ...

You can adjust the spawn settings over the `adaptive_performance_tweaks/spawn/*.toml` config files.

The list of supported mods will be extended over time.

The following list includes mods were additional compatibility / checks were added, to ensure
compatibility with the spawn optimizations.

- ✅ Bigger Reactors
- ✅ Botania
- ✅ Create
- ✅ Industrial Foregoing
- ✅ Mekanism
- ✅ Pipez
- ✅ Pokecube AIO
- ✅ Refined Storage
- ✅ Ultimate Car
- ✅ Viescraft
- ✅ XNet
- ...

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

[header]: ../assets/aptweaks-header-only.png

[bundled]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks