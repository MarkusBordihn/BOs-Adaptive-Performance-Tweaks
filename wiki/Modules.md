# Modules ⚙️

Enhance your gameplay with our specialized modules, each designed to optimize your Minecraft
experience. These modules are tailored to specific needs, all sharing a common core for seamless
integration.

## Core 🔩

The essential backbone of all adaptive performance tweaks modules, the core provides a unified API,
monitoring functions, events, and commands.
It's required and automatically installed when you choose any of the other modules.

## Game Rules 🔀

Fine-tune your gaming experience with automatic adjustments to random tick speed, entity cramming,
raids, patrols, trader interactions, and insomnia settings.
These optimizations respond dynamically to the server load, ensuring a balanced gameplay
environment.

### Game Rule Optimizations 🔀

Automatically adjust game rules like the random tick speed, entity cramming, raids, patrol, trader
and insomnia based on the current server load.

| Supported Game Rule        |
|----------------------------|
| blockExplosionDropDecay    |
| disableElytraMovementCheck |
| disableRaids               |
| doInsomnia                 |
| doPatrolSpawning           |
| doTraderSpawning           |
| doVinesSpread              |
| doWardenSpawning           |
| maxEntityCramming          |
| mobExplosionDropDecay      |
| randomTickSpeed            |
| tntExplosionDropDecay      |

## Item Clustering and Garbage Cleanup ✨🗑️

Say goodbye to cluttered worlds! Our mod intelligently clusters fallen blocks and experience orbs,
optimizing server performance. Items are automatically cleaned up based on server load, enhancing
gameplay without compromising server stability.

Experience orbs are intelligently managed, preventing lag caused by large monster farms or neglected
items. They are clustered within a specific range and combined for efficiency. All optimizations
occur server-side, eliminating the need for custom items.

## Player 👨‍👩‍👧‍👦

Bid farewell to long waiting times! Reduce player login delays and safeguard players during the
transition from login to active gameplay.
This mod ensures a seamless login experience and protects players from unexpected mob attacks.

### Child Player Protection 🛡️🐨

Playing together with less experience player or child's could be a challenge to find the right
balance for all players.
Just add the child player names to the configuration and additional define how much hurt damage
should be decreased and if any attack damage should be increased for them.
This allows to more enjoying the game even if you are playing in hardcore mode or with less
experience players or child's.

## Spawn Control and Spawner Optimization 👻📦

Enhance your world's dynamics with optimized mob spawns. Entities are limited within the player's
view area, ensuring a balanced distribution without compromising player experience. Entities beyond
a specific distance won't spawn until you approach, maintaining a fluid gameplay experience.

### Optimized Mob Spawn Calculations 👾

Mob spawns are limited to a specific view area of the player which allows a better distribution and
lowers the numbers of entities without a visible effect for the user.
_Example: If you are walking on the surface entities below a specific distance / which are currently
not reachable will not be spawn until you are closer to them._

### Spawn Control 👻

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

### Spawner Optimization 📦

Keeps track of the current number of loaded spawner and perform smaller optimization.
You could also get an overview of all currently loaded spawners over `/aptweaks spawner`.

### Config file 🗄️

With the config file you can disable / enable each of these individual features and adjust the
relevant list of affected mobs.

## Automatic Server Bundle and Mod Cleanup 👾

Simplify your modding experience! Automatically disable incompatible or unnecessary mods on the
server side.
Say goodbye to the hassle of managing separate server and client mod packs.
Our mod also cleans up duplicate files, eliminating confusion and ensuring seamless gameplay.

Note: This mod is renaming / moving your mod files, for this reason it is important that you have
regular backups in place.
I will do my best to implement corresponding safety features to avoid possible issues, but I'm not
able to cover all use cases.

### Automatic Server Bundle (one mod pack for client/server) 👾

This mod automatically disable mods which are incompatible or not needed on the server side.
There is no longer the need to have a separated "server" and "client" mod pack only for the mods.

### Automatic Mod Cleanup 👾

This mod automatically clean up duplicated files, by removing older versions.
This is helpful to avoid duplication issues and manual deleting of older versions.

### Total Start Time Logging ⏱️

Measure and logs the total start time of the client and server inside the log.
This makes it ideal to optimize the loading time of mods packs without using a manual stop clock.

### How to use the mod ?

Just add the mod to your mod pack and during the start on a Minecraft Client or a Minecraft Server
it will automatically optimize the mods in the mod folder.

If the mod is used on a server all client related mods will be automatically renamed to "
???.client".
The mod is mostly helpful if it is used on the server and the client side.

### Example Mod Pack

If you want to see this mod in action, please take a look at
the [Reference Mod Pack][example-mod-pack].

[example-mod-pack]: https://www.curseforge.com/minecraft/modpacks/bos-adventure-world
