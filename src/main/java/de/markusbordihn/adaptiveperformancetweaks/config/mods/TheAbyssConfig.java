package de.markusbordihn.adaptiveperformancetweaks.config.mods;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import net.minecraftforge.fml.ModList;

public class TheAbyssConfig extends SpawnConfigModSupport {

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

  public static void addSpawnRates(Map<String, Integer> spawnConfigPerPlayer,
      Map<String, Integer> spawnConfigPerWorld) {
    if (Boolean.FALSE.equals(COMMON.modTheabyssEnabled.get())
        || !ModList.get().isLoaded("theabyss")) {
      return;
    }
    log.info("\u2713 Enable spawn rate control for The Abyss");
    for (String entity : passiveMobList) {
      spawnConfigPerPlayer.put(entity, COMMON.modTheabyssMaxPassiveMobsPerPlayer.get());
      spawnConfigPerWorld.put(entity, COMMON.modTheabyssMaxPassiveMobsPerWorld.get());
    }
    for (String entity : hostileMobList) {
      spawnConfigPerPlayer.put(entity, COMMON.modTheabyssMaxHostileMobsPerPlayer.get());
      spawnConfigPerWorld.put(entity, COMMON.modTheabyssMaxHostileMobsPerWorld.get());
    }
  }
}
