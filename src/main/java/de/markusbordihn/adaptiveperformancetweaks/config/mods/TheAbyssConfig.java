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

public class TheAbyssConfig extends SpawnConfigModSupport {

  private static final String NAME = Constants.THEABYSS_NAME;
  private static final String MOD_ID = Constants.THEABYSS_MOD;

  private static Set<String> passiveMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
    "theabyss:eot_light_fish",
    "theabyss:eot_light_fish_2",
    "theabyss:eot_light_fish_3",
    "theabyss:eot_light_fish_4",
    "theabyss:fire_fly",
    "theabyss:jungle_light_fish",
    "theabyss:jungle_light_fish_green",
    "theabyss:light_fish",
    "theabyss:soul_lizard",
    "theabyss:soul_lizard_jungle"
  // @formatter:on
  ));

  private static Set<String> hostileMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
    "theabyss:abyss_elder",
    "theabyss:abyss_forest_zombie",
    "theabyss:abyss_forest_zombie_lv_l_3",
    "theabyss:abyss_forest_zombie_lv_l_4",
    "theabyss:abyss_guard_lv_l_1",
    "theabyss:abyss_guard_lv_l_4",
    "theabyss:abyss_guard_protect",
    "theabyss:abyss_raptor",
    "theabyss:abyss_spider_lv_l_2",
    "theabyss:abyss_spider_lv_l_3",
    "theabyss:abysscorruptedcow",
    "theabyss:abysscreeper",
    "theabyss:abyssguard",
    "theabyss:abyssspider",
    "theabyss:endspider",
    "theabyss:ice_knight",
    "theabyss:ice_skeleton",
    "theabyss:nether_scorpion_lv_l_4",
    "theabyss:netherscorpion",
    "theabyss:night_hunter",
    "theabyss:phantom_abyss_entity",
    "theabyss:phantom_abyss_entity_lv_l_3",
    "theabyss:slime_spider",
    "theabyss:soul_guard_lv_l_4",
    "theabyss:soulguard",
    "theabyss:the_abyss_dire_wolf",
    "theabyss:the_abyss_wolf",
    "theabyss:the_lurker",
    "theabyss:the_roka",
    "theabyss:the_roka_end_boss"
  // @formatter:on
  ));

  public static void addSpawnRates() {
    if (Boolean.FALSE.equals(COMMON.modTheabyssEnabled.get()) || !ModList.get().isLoaded(MOD_ID)) {
      return;
    }
    addSpawnRatesForPassiveMobs(NAME, passiveMobList,
        COMMON.modTheabyssMaxPassiveMobsPerPlayer.get(),
        COMMON.modTheabyssMaxPassiveMobsPerWorld.get());
    addSpawnRatesForHostileMobs(NAME, hostileMobList,
        COMMON.modTheabyssMaxHostileMobsPerPlayer.get(),
        COMMON.modTheabyssMaxHostileMobsPerWorld.get());
  }
}
