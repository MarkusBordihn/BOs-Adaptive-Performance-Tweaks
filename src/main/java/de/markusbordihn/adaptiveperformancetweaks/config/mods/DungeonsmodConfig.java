/**
 * Copyright 2021 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.adaptiveperformancetweaks.config.mods;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import net.minecraftforge.fml.ModList;

public class DungeonsmodConfig extends SpawnConfigModSupport {

  protected DungeonsmodConfig() {

  }

  private static Set<String> hostileMobList = new HashSet<>(Arrays.asList(
    // @formatter:off
    "dungeonsmod:anthermite",
    "dungeonsmod:crow",
    "dungeonsmod:deadhound",
    "dungeonsmod:guard",
    "dungeonsmod:haunter",
    "dungeonsmod:leech",
    "dungeonsmod:lordskeleton",
    "dungeonsmod:mimic",
    "dungeonsmod:notehead",
    "dungeonsmod:pice",
    "dungeonsmod:piranha",
    "dungeonsmod:pitcher",
    "dungeonsmod:rogue",
    "dungeonsmod:scientist",
    "dungeonsmod:slimewolf",
    "dungeonsmod:slimond",
    "dungeonsmod:voidmaster",
    "dungeonsmod:whirlwind",
    "dungeonsmod:winterhunter"
  // @formatter:on
  ));

  private static Set<String> bossMobList = new HashSet<>(Arrays.asList(
    // @formatter:off
    "dungeonsmod:crawler",
    "dungeonsmod:deserted",
    "dungeonsmod:ironslime",
    "dungeonsmod:king",
    "dungeonsmod:kraken",
    "dungeonsmod:sun"
  // @formatter:on
  ));

  public static void addSpawnRates(Map<String, Integer> spawnConfigPerPlayer,
      Map<String, Integer> spawnConfigPerWorld) {
    if (Boolean.FALSE.equals(COMMON.modDungeonsmodEnabled.get())
        || !ModList.get().isLoaded("dungeonsmod")) {
      return;
    }
    log.info("\u2713 Enable spawn rate control for Dungeons Mod");
    for (String entity : hostileMobList) {
      spawnConfigPerPlayer.put(entity, COMMON.modDungeonsmodMaxHostileMobsPerPlayer.get());
      spawnConfigPerWorld.put(entity, COMMON.modDungeonsmodMaxHostileMobsPerWorld.get());
    }
    for (String entity : bossMobList) {
      spawnConfigPerPlayer.put(entity, COMMON.modDungeonsmodMaxBossesPerPlayer.get());
      spawnConfigPerWorld.put(entity, COMMON.modDungeonsmodMaxBossesPerWorld.get());
    }
  }

}
