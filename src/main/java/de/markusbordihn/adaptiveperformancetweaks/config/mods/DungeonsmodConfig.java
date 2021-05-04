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
import java.util.Set;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import net.minecraftforge.fml.ModList;

public class DungeonsmodConfig extends SpawnConfigModSupport {

  private static final String NAME = Constants.ICEANDFIRE_NAME;
  private static final String MOD_ID = Constants.ICEANDFIRE_MOD;

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

  public static void addSpawnRates() {
    if (Boolean.FALSE.equals(COMMON.modDungeonsmodEnabled.get())
        || !ModList.get().isLoaded(MOD_ID)) {
      return;
    }
    addSpawnRatesForHostileMobs(NAME, hostileMobList,
        COMMON.modDungeonsmodMaxHostileMobsPerPlayer.get(),
        COMMON.modDungeonsmodMaxHostileMobsPerWorld.get());
    addSpawnRatesForBossMobs(NAME, bossMobList, COMMON.modDungeonsmodMaxBossesPerPlayer.get(),
        COMMON.modDungeonsmodMaxBossesPerWorld.get());

    // Only one rogue per player is enough.
    addSpawnRateForMob("dungeonsmod:rogue", 1, 8);

    if (Boolean.TRUE.equals(COMMON.modDungeonsmodOptimizeWhirlwind.get())) {
      addSpawnRateForMob("dungeonsmod:whirlwind", 1, 2);
    }
  }

}
