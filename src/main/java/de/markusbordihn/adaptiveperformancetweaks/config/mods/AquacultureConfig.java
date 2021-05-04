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

public class AquacultureConfig extends SpawnConfigModSupport {

  private static final String NAME = Constants.AQUACULTURE_NAME;
  private static final String MOD_ID = Constants.AQUACULTURE_MOD;

  private static Set<String> passiveMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
    "aquaculture:acacia_fish_mount",
    "aquaculture:arapaima",
    "aquaculture:arrau_turtle",
    "aquaculture:atlantic_cod",
    "aquaculture:atlantic_halibut",
    "aquaculture:atlantic_herring",
    "aquaculture:bayad",
    "aquaculture:birch_fish_mount",
    "aquaculture:blackfish",
    "aquaculture:bluegill",
    "aquaculture:bobber",
    "aquaculture:boulti",
    "aquaculture:box_turtle",
    "aquaculture:brown_shrooma",
    "aquaculture:brown_trout",
    "aquaculture:capitaine",
    "aquaculture:carp",
    "aquaculture:catfish",
    "aquaculture:dark_oak_fish_mount",
    "aquaculture:gar",
    "aquaculture:jellyfish",
    "aquaculture:jungle_fish_mount",
    "aquaculture:minnow",
    "aquaculture:muskellunge",
    "aquaculture:oak_fish_mount",
    "aquaculture:pacific_halibut",
    "aquaculture:perch",
    "aquaculture:pink_salmon",
    "aquaculture:piranha",
    "aquaculture:pollock",
    "aquaculture:rainbow_trout",
    "aquaculture:red_grouper",
    "aquaculture:red_shrooma",
    "aquaculture:smallmouth_bass",
    "aquaculture:spruce_fish_mount",
    "aquaculture:starshell_turtle",
    "aquaculture:synodontis",
    "aquaculture:tambaqui",
    "aquaculture:tuna",
    "aquaculture:water_arrow"
  // @formatter:on
  ));

  public static void addSpawnRates() {
    if (Boolean.FALSE.equals(COMMON.modAquacultureEnabled.get()) || !ModList.get().isLoaded(MOD_ID)) {
      return;
    }
    addSpawnRatesForPassiveMobs(NAME, passiveMobList, COMMON.modAquacultureMaxFishPerPlayer.get(),
        COMMON.modAquacultureMaxFishPerWorld.get());
  }

}
