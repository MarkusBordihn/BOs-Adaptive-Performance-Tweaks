# 🎛️ Adaptive Performance Tweaks: Game Rules

[![APTweaks: Game Rules Versions](http://cf.way2muchnoise.eu/versions/Minecraft_561137_all.svg)][mod_page]

[![Download on CurseForge](http://cf.way2muchnoise.eu/title/561137.svg)][mod_page]
[![CurseForge Downloads](http://cf.way2muchnoise.eu/full_561137_downloads.svg)][mod_page]

[![Download on Modrinth](https://img.shields.io/badge/dynamic/json?labelColor=black&color=grey&label=&query=title&url=https://api.modrinth.com/v2/project/Rd89yO7d&style=flat&logo=modrinth)][modrinth_page]
[![Modrinth Downloads](https://img.shields.io/badge/dynamic/json?labelColor=black&color=grey&label=&suffix=%20downloads&query=downloads&url=https://api.modrinth.com/v2/project/Rd89yO7d&style=flat&logo=modrinth)][modrinth_page]

[![Report an Issue](https://img.shields.io/badge/Report%20Issue%20%2F%20Bug%20%2F%20Crash-grey?style=flat&logo=github)][issues]
[![Open Issues](https://img.shields.io/github/issues/MarkusBordihn/BOs-Adaptive-Performance-Tweaks?style=flat&logo=Github&color=red)][issues_open]
[![Closed Issues](https://img.shields.io/github/issues-closed/MarkusBordihn/BOs-Adaptive-Performance-Tweaks?style=flat&logo=Github)][issues_closed]

![Adaptive Performance Tweaks: Game Rules][header]

Server-side Forge module that keeps TPS/FPS stable by dynamically adjusting selected game rules
based on server load.
Works standalone on the server; clients are optional.

## Status ✅

Stable LTS (Long‑Term Support, Classic Forge).
Actively maintained and recommended for new Forge modpacks.
After 3+ years of development, this Classic line is highly optimized and battle‑tested.
Critical fixes and compatibility updates will be provided when needed. 🛠️

If your pack runs fine on Classic Forge, no action is required. 👍

## Requirements 🧩

Requires [Adaptive Performance Tweaks: Core][core_mod]

## Features ✨

- Server-side only; clients do not need to install the mod.
- Pick only the modules you want; highly configurable via config files.
- No core/mixin patches → broad mod compatibility.
- Built-in debug info to see what the optimizer is doing.

⚠️ Always keep regular backups of your world.

## Game rule optimizations 🎛️

- Adjusts randomTickSpeed dynamically to the current server load.
- Tunes entity cramming to reduce lag spikes.
- Controls raids, patrols, wandering trader, and insomnia under high load.

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

[mod_page]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-gamerules

[modrinth_page]: https://modrinth.com/mod/Rd89yO7d

[issues]: https://github.com/MarkusBordihn/BOs-Adaptive-Performance-Tweaks/issues

[issues_open]: https://github.com/MarkusBordihn/BOs-Adaptive-Performance-Tweaks/issues

[issues_closed]: https://github.com/MarkusBordihn/BOs-Adaptive-Performance-Tweaks/issues?q=is%3Aissue+is%3Aclosed

[core_mod]: https://www.curseforge.com/minecraft/mc-mods/adaptive-performance-tweaks-core
