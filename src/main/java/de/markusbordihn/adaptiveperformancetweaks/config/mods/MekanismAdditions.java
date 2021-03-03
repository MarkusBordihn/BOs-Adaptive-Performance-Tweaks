package de.markusbordihn.adaptiveperformancetweaks.config.mods;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import net.minecraftforge.fml.ModList;

public class MekanismAdditions extends SpawnConfigModSupport {

  private static Set<String> hostileMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
    "mekanismadditions:baby_creeper",
    "mekanismadditions:baby_enderman",
    "mekanismadditions:baby_skeleton",
    "mekanismadditions:baby_stray",
    "mekanismadditions:baby_wither_skeleton"
  // @formatter:on
  ));

  public static void addSpawnRates(Map<String, Integer> spawnConfigPerPlayer,
      Map<String, Integer> spawnConfigPerWorld) {
    if (Boolean.FALSE.equals(COMMON.modMekanismadditionsEnabled.get())
        || !ModList.get().isLoaded("mekanismadditions")) {
      return;
    }
    log.info("\u2713 Enable spawn rate control for Mekanism Additions");
    for (String entity : hostileMobList) {
      spawnConfigPerPlayer.put(entity, COMMON.modMekanismadditionsMaxHostileMobsPerPlayer.get());
      spawnConfigPerWorld.put(entity, COMMON.modMekanismadditionsMaxHostileMobsPerWorld.get());
    }
  }
}
