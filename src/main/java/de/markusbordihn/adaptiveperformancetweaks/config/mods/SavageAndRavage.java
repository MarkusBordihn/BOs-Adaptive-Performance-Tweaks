package de.markusbordihn.adaptiveperformancetweaks.config.mods;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import net.minecraftforge.fml.ModList;

public class SavageAndRavage extends SpawnConfigModSupport {

  private static Set<String> hostileMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
    "savageandravage:executioner",
    "savageandravage:griefer",
    "savageandravage:iceologer",
    "savageandravage:skeleton_villager"
  // @formatter:on
  ));

  public static void addSpawnRates(Map<String, Integer> spawnConfigPerPlayer,
      Map<String, Integer> spawnConfigPerWorld) {
    if (Boolean.FALSE.equals(COMMON.modSavageandravageEnabled.get())
        || !ModList.get().isLoaded("savageandravage")) {
      return;
    }
    log.info("\u2713 Enable spawn rate control for Savage and Ravage");
    for (String entity : hostileMobList) {
      spawnConfigPerPlayer.put(entity, COMMON.modSavageandravageMaxHostileMobsPerPlayer.get());
      spawnConfigPerWorld.put(entity, COMMON.modSavageandravageMaxHostileMobsPerWorld.get());
    }
  }
}
