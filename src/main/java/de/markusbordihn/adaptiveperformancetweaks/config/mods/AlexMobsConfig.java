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

public class AlexMobsConfig extends SpawnConfigModSupport {

  private static final String NAME = Constants.ALEXSMOBS_NAME;
  private static final String MOD_ID = Constants.ALEXSMOBS_MOD;

  private static Set<String> passiveMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
    "alexsmobs:blobfish",
    "alexsmobs:endergrade",
    "alexsmobs:fly",
    "alexsmobs:gazelle",
    "alexsmobs:hummingbird",
    "alexsmobs:mungus",
    "alexsmobs:seal",
    "alexsmobs:spectre",
    "alexsmobs:stradpole",
    "alexsmobs:sunbird"
  // @formatter:on
  ));

  private static Set<String> neutralMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
   "alexsmobs:cachalot_whale",
   "alexsmobs:capuchin_monkey",
   "alexsmobs:cockroach",
   "alexsmobs:crow",
   "alexsmobs:elephant",
   "alexsmobs:emu",
   "alexsmobs:gorilla",
   "alexsmobs:kangaroo",
   "alexsmobs:lobster",
   "alexsmobs:mantis_shrimp",
   "alexsmobs:moose",
   "alexsmobs:orca",
   "alexsmobs:platypus",
   "alexsmobs:raccoon",
   "alexsmobs:roadrunner",
   "alexsmobs:shoebill",
   "alexsmobs:snow_leopard",
   "alexsmobs:tasmanian_devil",
   "alexsmobs:warped_toad"
  // @formatter:on
  ));

  private static Set<String> hostileMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
   "alexsmobs:alligator_snapping_turtle",
   "alexsmobs:bone_serpent",
   "alexsmobs:centipede_head",
   "alexsmobs:crimson_mosquito",
   "alexsmobs:crocodile",
   "alexsmobs:dropbear",
   "alexsmobs:enderiophage",
   "alexsmobs:grizzly_bear",
   "alexsmobs:guster",
   "alexsmobs:hammerhead_shark",
   "alexsmobs:komodo_dragon",
   "alexsmobs:leafcutter_ant",
   "alexsmobs:mimicube",
   "alexsmobs:rattlesnake",
   "alexsmobs:soul_vulture",
   "alexsmobs:straddler",
   "alexsmobs:warped_mosco"
  // @formatter:on
  ));

  public static void addSpawnRates() {
    if (Boolean.FALSE.equals(COMMON.modAlexmobsEnabled.get()) || !ModList.get().isLoaded(MOD_ID)) {
      return;
    }
    addSpawnRatesForPassiveMobs(NAME, passiveMobList, COMMON.modAlexmobsMaxPassiveMobsPerPlayer.get(),
        COMMON.modAlexmobsMaxPassiveMobsPerWorld.get());
    addSpawnRatesForNeutralMobs(NAME, neutralMobList, COMMON.modAlexmobsMaxNeutralMobsPerPlayer.get(),
        COMMON.modAlexmobsMaxNeutralMobsPerWorld.get());
    addSpawnRatesForHostileMobs(NAME, hostileMobList, COMMON.modAlexmobsMaxHostileMobsPerPlayer.get(),
        COMMON.modAlexmobsMaxHostileMobsPerWorld.get());

    // Limiting resource intensive mobs
    addSpawnRateForMob("alexsmobs:cachalot_whale", 1, 1);
    addSpawnRateForMob("alexsmobs:centipede_head", 1, 2);
    addSpawnRateForMob("alexsmobs:hammerhead_shark", 1, 1);
    addSpawnRateForMob("alexsmobs:orca", 1, 1);
  }

}
