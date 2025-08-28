# 👾 Adaptive Performance Tweaks: Spawn

[![APTweaks: Spawn Versions](http://cf.way2muchnoise.eu/versions/Minecraft_573708_all.svg)][mod_page]

[![Download on CurseForge](http://cf.way2muchnoise.eu/title/573708.svg)][mod_page]
[![CurseForge Downloads](http://cf.way2muchnoise.eu/full_573708_downloads.svg)][mod_page]

[![Download on Modrinth](https://img.shields.io/badge/dynamic/json?labelColor=black&color=grey&label=&query=title&url=https://api.modrinth.com/v2/project/217PsbJI&style=flat&logo=modrinth)][modrinth_page]
[![Modrinth Downloads](https://img.shields.io/badge/dynamic/json?labelColor=black&color=grey&label=&suffix=%20downloads&query=downloads&url=https://api.modrinth.com/v2/project/217PsbJI&style=flat&logo=modrinth)][modrinth_page]

[![Report an Issue](https://img.shields.io/badge/Report%20Issue%20%2F%20Bug%20%2F%20Crash-grey?style=flat&logo=github)][issues]
[![Open Issues](https://img.shields.io/github/issues/MarkusBordihn/BOs-Adaptive-Performance-Tweaks?style=flat&logo=Github&color=red)][issues_open]
[![Closed Issues](https://img.shields.io/github/issues-closed/MarkusBordihn/BOs-Adaptive-Performance-Tweaks?style=flat&logo=Github)][issues_closed]

![Adaptive Performance Tweaks: Spawn][header]

Server-side Forge module that optimizes mob spawning: smarter distribution, spawn limits, and
spawner tweaks to keep entity counts under control.
Works standalone on the server; clients are optional.

## Status ✅

Stable LTS (Long‑Term Support, Classic Forge).
Actively maintained and recommended for new Forge modpacks.
After 3+ years of development, this Classic line is highly optimized and battle‑tested.
Critical fixes and compatibility updates will be provided when needed. 🛠️

Looking for the next‑gen rewrite for Fabric, Forge, and NeoForge?
➡️ Check out the new [Eco Stack Manager][eco-stack-manager].

If your pack runs fine on Classic, no action is required. 👍

## Requirements 🧩

Requires [Adaptive Performance Tweaks: Core][core_mod]

## Features ✨

- Server-side only; clients do not need to install the mod.
- Pick only the modules you want; highly configurable via config files.
- No core/mixin patches → broad mod compatibility.
- Built-in debug info to see what the optimizer is doing.

⚠️ Always keep regular backups of your world.

## Spawn optimizations 👾

- Optimized mob spawn calculations around the player for better distribution and fewer entities.
- Basic spawn control: per-player/world caps, allow/deny lists, and presets for popular mods.
- Adaptive spawn rate considers players, caps, server load, and game difficulty.
- Spawner optimization: track loaded spawners and apply lightweight tweaks.
- Configurable via adaptive_performance_tweaks/spawn/*.toml.

## Bundled modules 📦

Want everything at once? Install the bundled version:

➡️ [Install the bundled version][bundled]

## Learn more 📚

- Wiki: https://github.com/MarkusBordihn/BOs-Adaptive-Performance-Tweaks/wiki
-

Troubleshooting: https://github.com/MarkusBordihn/BOs-Adaptive-Performance-Tweaks/wiki/Troubleshooting

- Issues: Report via GitHub Issues or the issue/comment feature on CurseForge/Modrinth where you got
  the mod.

[header]: https://github.com/MarkusBordihn/BOs-Adaptive-Performance-Tweaks/wiki/images/aptweaks-header-only.png

[eco-stack-manager]: https://www.curseforge.com/minecraft/mc-mods/eco-stack-manager

[bundled]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks

[mod_page]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-spawn

[modrinth_page]: https://modrinth.com/mod/217PsbJI

[issues]: https://github.com/MarkusBordihn/BOs-Adaptive-Performance-Tweaks/issues

[issues_open]: https://github.com/MarkusBordihn/BOs-Adaptive-Performance-Tweaks/issues

[issues_closed]: https://github.com/MarkusBordihn/BOs-Adaptive-Performance-Tweaks/issues?q=is%3Aissue+is%3Aclosed

[core_mod]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-core
