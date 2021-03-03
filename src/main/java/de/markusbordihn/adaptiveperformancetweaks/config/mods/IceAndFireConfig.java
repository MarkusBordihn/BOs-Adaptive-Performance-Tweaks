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

public class IceAndFireConfig extends SpawnConfigModSupport {

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

  public static void addSpawnRates(Map<String, Integer> spawnConfigPerPlayer,
      Map<String, Integer> spawnConfigPerWorld) {
    if (Boolean.FALSE.equals(COMMON.modIceandfireEnabled.get())
        || !ModList.get().isLoaded("iceandfire")) {
      return;
    }
    log.info("\u2713 Enable spawn rate control for Iceandfire");
    for (String entity : passiveMobList) {
      spawnConfigPerPlayer.put(entity, COMMON.modIceandfireMaxPassiveMobsPerPlayer.get());
      spawnConfigPerWorld.put(entity, COMMON.modIceandfireMaxPassiveMobsPerWorld.get());
    }
    for (String entity : neutralMobList) {
      spawnConfigPerPlayer.put(entity, COMMON.modIceandfireMaxNeutralMobsPerPlayer.get());
      spawnConfigPerWorld.put(entity, COMMON.modIceandfireMaxNeutralMobsPerWorld.get());
    }
    for (String entity : hostileMobList) {
      spawnConfigPerPlayer.put(entity, COMMON.modIceandfireMaxHostileMobsPerPlayer.get());
      spawnConfigPerWorld.put(entity, COMMON.modIceandfireMaxHostileMobsPerWorld.get());
    }
    for (String entity : bossMobList) {
      spawnConfigPerPlayer.put(entity, COMMON.modIceandfireMaxBossesPerPlayer.get());
      spawnConfigPerWorld.put(entity, COMMON.modIceandfireMaxBossesPerWorld.get());
    }
  }

}
