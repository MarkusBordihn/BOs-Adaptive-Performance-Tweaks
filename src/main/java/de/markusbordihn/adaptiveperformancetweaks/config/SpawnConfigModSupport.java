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

package de.markusbordihn.adaptiveperformancetweaks.config;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class SpawnConfigModSupport {

  SpawnConfigModSupport() {
  }

  private static Set<String> aquacultureFishList = new HashSet<>(Arrays.asList(
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

  private static Set<String> iceandfirePassiveMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
    "iceandfire:pixie",
    "iceandfire:pixie_charge",
    "iceandfire:hippocampus"
  // @formatter:on
  ));

  private static Set<String> iceandfireNeutralMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
    "iceandfire:hippogryph",
    "iceandfire:hippogryph_egg",
    "iceandfire:amphithere",
    "iceandfire:amphithere_arrow"
  // @formatter:on
  ));

  private static Set<String> iceandfireHostileMobList = new HashSet<>(Arrays.asList(
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

  private static Set<String> iceandfireBossList = new HashSet<>(Arrays.asList(
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

  private static Set<String> mekanismadditionsHostileMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
    "mekanismadditions:baby_creeper",
    "mekanismadditions:baby_enderman",
    "mekanismadditions:baby_skeleton",
    "mekanismadditions:baby_stray",
    "mekanismadditions:baby_wither_skeleton"
  // @formatter:on
  ));

  private static Set<String> quarkPassiveMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
    "quark:crab",
    "quark:frog",
    "quark:stoneling"
  // @formatter:on
  ));

  private static Set<String> quarkNeutralMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
    "quark:toretoise"
  // @formatter:on
  ));

  private static Set<String> quarkHostileMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
    "quark:forgotten",
    "quark:foxhound",
    "quark:wraith"
  // @formatter:on
  ));

  private static Set<String> savageandravageHostileMobList = new HashSet<>(Arrays.asList(
  // @formatter:off
    "savageandravage:executioner",
    "savageandravage:griefer",
    "savageandravage:iceologer",
    "savageandravage:skeleton_villager"
  // @formatter:on
  ));

  private static Set<String> theabyssPassiveMobList = new HashSet<>(Arrays.asList(
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

  private static Set<String> theabyssHostileMobList = new HashSet<>(Arrays.asList(
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

  public static Set<String> getAquacultureFishList() {
    return aquacultureFishList;
  }

  public static Set<String> getIceandfirePassiveMobList() {
    return iceandfirePassiveMobList;
  }

  public static Set<String> getIceandfireNeutralMobList() {
    return iceandfireNeutralMobList;
  }

  public static Set<String> getIceandfireHostileMobList() {
    return iceandfireHostileMobList;
  }

  public static Set<String> getIceandfireBossList() {
    return iceandfireBossList;
  }

  public static Set<String> getMekanismadditionsHostileMobList() {
    return mekanismadditionsHostileMobList;
  }

  public static Set<String> getQuarkPassiveMobList() {
    return quarkPassiveMobList;
  }

  public static Set<String> getQuarkNeutralMobList() {
    return quarkNeutralMobList;
  }

  public static Set<String> getQuarkHostileMobList() {
    return quarkHostileMobList;
  }

  public static Set<String> getSavageandravageHostileMobList() {
    return savageandravageHostileMobList;
  }

  public static Set<String> getTheabyssPassiveMobList() {
    return theabyssPassiveMobList;
  }

  public static Set<String> getTheabyssHostileMobList() {
    return theabyssHostileMobList;
  }

}
