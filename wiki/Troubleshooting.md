# Troubleshooting

On our troubleshooting page, we want to help you with any problems you face, just like detectives
solving a mystery!
To do this, it's super important to share as many details about the issue as possible.

Imagine you're telling a cool story: the more details you give, the better we can understand and
help you.
If you don't provide enough details, it's like missing pieces of a puzzle.
We might have to keep asking questions which can take a lot of time and might feel a bit like going
in circles.
So, remember, sharing all the information helps us solve the problem faster and makes everything
easier for everyone involved! 🕵️‍♂️🔍

## Create a debug.log

If you're encountering issues with the mod, creating a debug log can greatly assist us in
identifying and solving the problem. Follow these simple steps to generate a debug log:

1. Load the world where the problem occurs.
2. Ensure you have operator privileges to use debug commands.
3. Depending on the module you're having issues with,
   run `/aptweaks debug core true`, `/aptweaks debug spawn true`, or `/aptweaks debug items` true.
4. Explore the world and attempt to reproduce the issue.
5. Once done, run `/aptweaks debug core false`, `/aptweaks debug spawn true`,
   or `/aptweaks debug items true` to disable the relevant module's debug mode.
6. Exit the world and close the game.
7. Locate and share the debug.log file from your logs folder. This file contains entries related to
   the issue like:

```
[26Mai2023 20:09:09.567] [Server thread/DEBUG] [APTweaks(Core)/]: [Left] Entity minecraft:bat (Bat['Bat'/717, l='ServerLevel[Test World 1.18.2]', x=-246.35, y=25.68, z=-614.77]) leaved minecraft:overworld.
[26Mai2023 20:09:10.564] [Server thread/DEBUG] [APTweaks(Core)/]: [Joined] Entity minecraft:bat (Bat['Bat'/816, l='ServerLevel[Test World 1.18.2]', x=-314.50, y=-34.00, z=-645.50]) joined minecraft:overworld.
[26Mai2023 20:09:10.614] [Server thread/DEBUG] [APTweaks(Core)/]: [Joined] Entity minecraft:bat (Bat['Bat'/818, l='ServerLevel[Test World 1.18.2]', x=-232.50, y=20.00, z=-705.50]) joined minecraft:overworld.
[26Mai2023 20:09:10.614] [Server thread/DEBUG] [APTweaks(Core)/]: [Joined] Entity minecraft:bat (Bat['Bat'/819, l='ServerLevel[Test World 1.18.2]', x=-231.50, y=20.00, z=-703.50]) joined minecraft:overworld.
[26Mai2023 20:09:10.614] [Server thread/DEBUG] [APTweaks(Core)/]: [Joined] Entity minecraft:bat (Bat['Bat'/820, l='ServerLevel[Test World 1.18.2]', x=-228.50, y=20.00, z=-713.50]) joined minecraft:overworld.
[26Mai2023 20:09:10.614] [Server thread/DEBUG] [APTweaks(Core)/]: [Joined] Entity minecraft:bat (Bat['Bat'/821, l='ServerLevel[Test World 1.18.2]', x=-232.50, y=20.00, z=-704.50]) joined minecraft:overworld.
[26Mai2023 20:09:13.717] [Server thread/DEBUG] [APTweaks(Core)/]: ? Removed 110 entries during the verification
```

Within the log, it's designed to be user-friendly, allowing you to quickly identify if any mobs or
items are blocked or spawned, along with the precise reasons for these occurrences.
Additionally, it provides clarity on whether specific monsters and items were permitted to spawn or
not.

It's worth noting that certain mods deviate from the standard spawn events and essentially 'bypass'
them.
This can lead to overpopulation issues, as these mods replace the potential spawns of 'blocked' mobs
without any limitations.

## No Mobs are spawning after removing Adaptive Performance Tweaks: Spawn Module ?

If you have removed Adaptive Performance Tweaks: Spawn Module and no mobs are spawning anymore, this
could have several reasons.
The most common reason is that other mods modified the chunk data and modified / removed the spawn
entries from the chunk data for optimization reasons.

The Adaptive Performance Tweaks: Spawn module is only blocking the spawn events itself and is not
modifying the chunk data.
This means if you remove Adaptive Performance Tweaks: Spawn module, the spawn events are not blocked
anymore, but the chunk data could be still modified by other mods.

Note: For this reason it is important to disable all kind of optimization mods before you using any
kind of pre-chunk-generator.
After the pre-chunk-generator has finished, you can re-enable the optimization mods again.

We try to mitigate this issue by allow the spawn events every x chunks to ensure that each chunk has
at least one unblocked spawn event.
But we can't cover all use cases and control all mods behaviour, so it could happen that the
relevant spawn chunk data are modified.

In the case that you have removed Adaptive Performance Tweaks and no mobs are spawning anymore, you
could try the following steps:

- Make sure that `/gamerule doMobSpawning` for natural spawning is enabled.
- Temporary increase the view distance
- Switch between peaceful, hardcore and normal mode which will trigger a recalculation of the spawn
  events.
- Edit the world and use the 'Optimize World' option to regenerate / cleanup the chunks.

### Regenerate Chunks

If the above steps are not working, you could try to regenerate the affected chunks.

- First create a new backup of your world and only work on the backup.
- In the case you use a pre-chunk-generator check their documentation if they are offering an option
  to refresh / regenerate the chunks.
- Use a world editor like [MCA Selector](https://github.com/Querz/mcaselector) to regenerate
  untouched chunks.

Most world editors have an option to automatically select all chunks which are not modified by the
player or only used for 1-2 minutes.
Please keep in mind to only select the chunks which are not modified by the player, otherwise you
will lose all changes in the selected chunks.

## How to report a bug

If you believe you've discovered a bug, first, see if someone else has already reported it on
our [GitHub issue tracker](https://github.com/MarkusBordihn/BOs-Adaptive-Performance-Tweaks/issues).
If not, please create a new issue, but remember, the more details you provide, the better!
Describe the problem and what you were doing when it happened.
The more information you share, the quicker we can fix it. Thank you for helping us make our system
even better!
