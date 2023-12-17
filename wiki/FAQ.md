# 🙋 FAQ

## Is this a server side / client side mod❓

The optimization are happening directly on the server side, so you don't need it on the client side.

Only the mods optimization module should be installed on the client and server side to get the best
results.

## Is this a Core Mod❓

**No, this is no core mod.** Core mods directly modifying existing Minecraft or Forge
implementations
which affects all other mods implementations and game mechanics.
For this reason core mods could cause crashes and incompatibility with the existing implementations
and other mods.
This mod is only using the existing API functionality and is not modifying any core implementation
to be compatible with other mods and game mechanics as much as possible.

## Will it affect other mods❓

In some cases it could be needed to adjust the settings to your preferences but the default should
be fine for most casual players.

## What are the general performance gains❓

In general this is hard to say, because it's depends on a lot of factors. In my case it helps me to
run a server with about 170 mods.
The mod will not help so much for extensive red stone constructions, custom spawner or other tasks
which requires several ticks for the calculation.

## Will it work with pre-chunk generation mods like Chunk Pre-generator❓

In general, you should disable/remove any optimization mods (including this mod) during the
pre-chunk
generation process, because it could cause unknown side effects.
After the pre-chunk generation processes you can safely re-enable the optimization mods again.

## Will it work with Performant, Clumps, Dynamic view, FPS reducer, FerriteCore and Spark❓

You should try to use as less as possible performance mods with overlapping features.
Depending on your mod pack it could be that one works better than the other.
For this reason it's important to test different kind of combination for your specific case.

## Will it work with xyz❓

I'm not able to test all possible mod combination, so you need to test this on your on own to find
out.

## Help something is not working as expected❓

If you have any issues, please check the [Troubleshooting](Troubleshooting.md) page first.
