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

import net.minecraftforge.fml.ModList;

import de.markusbordihn.adaptiveperformancetweaks.Constants;

public class Whisperwoods extends SpawnConfigModSupport {

  private static final String NAME = Constants.WHISPERWOODS_NAME;
  private static final String MOD_ID = Constants.WHISPERWOODS_MOD;

  private static Set<String> passiveMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
    "whisperwoods:moth"
  // @formatter:on
  ));

  private static Set<String> hostileMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
    "whisperwoods:hidebehind",
    "whisperwoods:wisp",
    "whisperwoods:zotzpyre"
  // @formatter:on
  ));

  private static Set<String> bossMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
    "whisperwoods:hirschgeist"
  // @formatter:on
  ));

  public static void addSpawnRates() {
    if (Boolean.FALSE.equals(COMMON.modWhisperwoodsEnabled.get()) || !ModList.get().isLoaded(MOD_ID)) {
      return;
    }
    addSpawnRatesForPassiveMobs(NAME, passiveMobList,
        COMMON.modWhisperwoodsMaxPassiveMobsPerPlayer.get(),
        COMMON.modWhisperwoodsMaxPassiveMobsPerWorld.get());
    addSpawnRatesForHostileMobs(NAME, hostileMobList,
        COMMON.modWhisperwoodsMaxHostileMobsPerPlayer.get(),
        COMMON.modWhisperwoodsMaxHostileMobsPerWorld.get());
    addSpawnRatesForBossMobs(NAME, bossMobList,
        COMMON.modWhisperwoodsMaxBossesPerPlayer.get(),
        COMMON.modWhisperwoodsMaxBossesPerWorld.get());
  }

}
