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

public class TwilightForestConfig extends SpawnConfigModSupport {

  private static final String NAME = Constants.TWILIGHTFOREST_NAME;
  private static final String MOD_ID = Constants.TWILIGHTFOREST_MOD;

  private static Set<String> passiveMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
    "twilightforest:bighorn_sheep",
    "twilightforest:bunny",
    "twilightforest:deer",
    "twilightforest:fire_beetle",
    "twilightforest:loyal_zombie",
    "twilightforest:penguin",
    "twilightforest:raven",
    "twilightforest:squirrel",
    "twilightforest:tiny_bird",
    "twilightforest:wild_boar"
  // @formatter:on
  ));

  private static Set<String> hostileMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
    "twilightforest:adherent",
    "twilightforest:armored_giant",
    "twilightforest:blockchain_goblin",
    "twilightforest:death_tome",
    "twilightforest:giant_miner",
    "twilightforest:goblin_knight_lower",
    "twilightforest:goblin_knight_upper",
    "twilightforest:harbinger_cube",
    "twilightforest:hedge_spider",
    "twilightforest:helmet_crab",
    "twilightforest:hostile_wolf",
    "twilightforest:king_spider",
    "twilightforest:kobold",
    "twilightforest:maze_slime",
    "twilightforest:mini_ghast",
    "twilightforest:minotaur",
    "twilightforest:mist_wolf",
    "twilightforest:mosquito_swarm",
    "twilightforest:pinch_beetle",
    "twilightforest:redcap",
    "twilightforest:redcap_sapper",
    "twilightforest:roving_cube",
    "twilightforest:skeleton_druid",
    "twilightforest:slime_beetle",
    "twilightforest:snow_guardian",
    "twilightforest:swarm_spider",
    "twilightforest:tower_broodling",
    "twilightforest:tower_ghast",
    "twilightforest:tower_golem",
    "twilightforest:tower_termite",
    "twilightforest:troll",
    "twilightforest:winter_wolf",
    "twilightforest:wraith",
    "twilightforest:yeti"
  // @formatter:on
  ));

  private static Set<String> bossMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
    "twilightforest:hydra",
    "twilightforest:hydra_mortar",
    "twilightforest:knight_phantom",
    "twilightforest:lich",
    "twilightforest:minoshroom",
    "twilightforest:naga",
    "twilightforest:quest_ram",
    "twilightforest:snow_queen",
    "twilightforest:ur_ghast",
    "twilightforest:yeti_alpha"
  // @formatter:on
  ));

  public static void addSpawnRates() {
    if (Boolean.FALSE.equals(COMMON.modTwilightForestEnabled.get()) || !ModList.get().isLoaded(MOD_ID)) {
      return;
    }
    addSpawnRatesForPassiveMobs(NAME, passiveMobList,
        COMMON.modTwilightForestMaxPassiveMobsPerPlayer.get(),
        COMMON.modTwilightForestMaxPassiveMobsPerWorld.get());
    addSpawnRatesForHostileMobs(NAME, hostileMobList,
        COMMON.modTwilightForestMaxHostileMobsPerPlayer.get(),
        COMMON.modTwilightForestMaxHostileMobsPerWorld.get());
    addSpawnRatesForBossMobs(NAME, bossMobList,
        COMMON.modTwilightForestMaxBossesPerPlayer.get(),
        COMMON.modTwilightForestMaxBossesPerWorld.get());
  }

}
