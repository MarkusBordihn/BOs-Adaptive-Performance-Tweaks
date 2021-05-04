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

public class IceAndFireConfig extends SpawnConfigModSupport {

  private static final String NAME = Constants.ICEANDFIRE_NAME;
  private static final String MOD_ID = Constants.ICEANDFIRE_MOD;

  private static Set<String> passiveMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
    "iceandfire:pixie",
    "iceandfire:pixie_charge",
    "iceandfire:hippocampus"
  // @formatter:on
  ));

  private static Set<String> neutralMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
    "iceandfire:hippogryph",
    "iceandfire:hippogryph_egg",
    "iceandfire:amphithere",
    "iceandfire:amphithere_arrow"
  // @formatter:on
  ));

  private static Set<String> hostileMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
    "iceandfire:chain_tie",
    "iceandfire:cockatrice",
    "iceandfire:cockatrice_egg",
    "iceandfire:deathworm",
    "iceandfire:deathworm_egg",
    "iceandfire:ghost",
    "iceandfire:ghost_sword",
    "iceandfire:mob_skull",
    "iceandfire:multipart",
    "iceandfire:myrmex_egg",
    "iceandfire:myrmex_queen",
    "iceandfire:myrmex_royal",
    "iceandfire:myrmex_sentinel",
    "iceandfire:myrmex_soldier",
    "iceandfire:myrmex_swarmer",
    "iceandfire:myrmex_worker",
    "iceandfire:sea_serpent",
    "iceandfire:sea_serpent_arrow",
    "iceandfire:sea_serpent_bubbles",
    "iceandfire:siren",
    "iceandfire:stone_statue",
    "iceandfire:stymphalian_arrow",
    "iceandfire:stymphalian_bird",
    "iceandfire:stymphalian_feather",
    "iceandfire:tide_trident",
    "iceandfire:troll"
  // @formatter:on
  ));

  private static Set<String> bossMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
    "iceandfire:cyclops",
    "iceandfire:cyclops_multipart",
    "iceandfire:cylcops_multipart", // Typing error inside the mod.
    "iceandfire:dragon_arrow",
    "iceandfire:dragon_egg",
    "iceandfire:dragon_multipart",
    "iceandfire:dragon_skull",
    "iceandfire:dread_beast",
    "iceandfire:dread_ghoul",
    "iceandfire:dread_horse",
    "iceandfire:dread_knight",
    "iceandfire:dread_lich",
    "iceandfire:dread_lich_skull",
    "iceandfire:dread_scuttler",
    "iceandfire:dread_thrall",
    "iceandfire:fire_dragon",
    "iceandfire:fire_dragon_charge",
    "iceandfire:gorgon",
    "iceandfire:hydra",
    "iceandfire:hydra_arrow",
    "iceandfire:hydra_breath",
    "iceandfire:hydra_multipart",
    "iceandfire:ice_dragon",
    "iceandfire:ice_dragon_charge",
    "iceandfire:lightning_dragon",
    "iceandfire:lightning_dragon_charge"
  // @formatter:on
  ));

  public static void addSpawnRates() {
    if (Boolean.FALSE.equals(COMMON.modIceandfireEnabled.get()) || !ModList.get().isLoaded(MOD_ID)) {
      return;
    }
    addSpawnRatesForPassiveMobs(NAME, passiveMobList,
        COMMON.modIceandfireMaxPassiveMobsPerPlayer.get(),
        COMMON.modIceandfireMaxPassiveMobsPerWorld.get());
    addSpawnRatesForNeutralMobs(NAME, neutralMobList,
        COMMON.modIceandfireMaxNeutralMobsPerPlayer.get(),
        COMMON.modIceandfireMaxNeutralMobsPerWorld.get());
    addSpawnRatesForHostileMobs(NAME, hostileMobList,
        COMMON.modIceandfireMaxHostileMobsPerPlayer.get(),
        COMMON.modIceandfireMaxHostileMobsPerWorld.get());
    addSpawnRatesForBossMobs(NAME, bossMobList, COMMON.modIceandfireMaxBossesPerPlayer.get(),
        COMMON.modIceandfireMaxBossesPerWorld.get());
  }

}
