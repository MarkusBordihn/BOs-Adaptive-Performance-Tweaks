package de.markusbordihn.adaptiveperformancetweaks.config.mods;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import net.minecraftforge.fml.ModList;

public class QuarkConfig extends SpawnConfigModSupport {
  private static Set<String> passiveMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
    "quark:crab",
    "quark:frog",
    "quark:stoneling"
  // @formatter:on
  ));

  private static Set<String> neutralMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
    "quark:toretoise"
  // @formatter:on
  ));

  private static Set<String> hostileMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
    "quark:forgotten",
    "quark:foxhound",
    "quark:wraith"
  // @formatter:on
  ));

  public static void addSpawnRates(Map<String, Integer> spawnConfigPerPlayer,
      Map<String, Integer> spawnConfigPerWorld) {
    if (Boolean.FALSE.equals(COMMON.modQuarkEnabled.get()) || !ModList.get().isLoaded("quark")) {
      return;
    }
    log.info("\u2713 Enable spawn rate control for Quark");
    for (String entity : passiveMobList) {
      spawnConfigPerPlayer.put(entity, COMMON.modQuarkMaxPassiveMobsPerPlayer.get());
      spawnConfigPerWorld.put(entity, COMMON.modQuarkMaxPassiveMobsPerWorld.get());
    }
    for (String entity : neutralMobList) {
      spawnConfigPerPlayer.put(entity, COMMON.modQuarkMaxNeutralMobsPerPlayer.get());
      spawnConfigPerWorld.put(entity, COMMON.modQuarkMaxNeutralMobsPerWorld.get());
    }
    for (String entity : hostileMobList) {
      spawnConfigPerPlayer.put(entity, COMMON.modQuarkMaxHostileMobsPerPlayer.get());
      spawnConfigPerWorld.put(entity, COMMON.modQuarkMaxHostileMobsPerWorld.get());
    }
  }

}
