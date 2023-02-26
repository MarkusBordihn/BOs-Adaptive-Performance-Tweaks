/**
 * Copyright 2022 Markus Bordihn
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

package de.markusbordihn.minecraft.adaptiveperformancetweaksmods.data;

public class TestData {

  public static String[] modList = {"adaptive_performance_tweaks_1.19.2-2.0.1.jar",
      "adaptive_performance_tweaks_core_1.19.2-2.2.0.jar",
      "adaptive_performance_tweaks_gamerules_1.19.2-2.2.0.jar",
      "adaptive_performance_tweaks_items_1.19.2-2.2.0.jar",
      "adaptive_performance_tweaks_player_1.19.2-2.2.0.jar",
      "adaptive_performance_tweaks_spawn_1.19.2-2.2.0.jar", "AdHooks-1.19.2-7.0.0.1-build.0050.jar",
      "advancednetherite-1.10.2-1.19.2.jar", "appleskin-forge-mc1.19.2-2.4.0.jar",
      "Aquaculture-1.19.2-2.3.3.jar", "architectury-3.7.33.jar", "artifacts-1.19.2-4.0.3.jar",
      "awesomedungeon-2.0.5.jar", "AwesomeDungeonEnd-Forge-1.19.2-1.0.0.jar",
      "AwesomeDungeonNether-Forge-1.19.2-1.0.0.jar", "AwesomeDungeonOcean-Forge-1.19.2-2.0.4.jar",
      "balm-2.4.3+0.jar", "bedspreads-forge-1.19.2-5.1.0.5.jar",
      "BetterDungeons-Forge-1.19.2-1.0.1.jar", "BetterStrongholds-Forge-1.19.2-1.0.2.jar",
      "BetterThanMending-1.7.1.jar", "Beyond-Earth-1.19.2-6.0d.jar",
      "Beyond-Earth-Giselle-Addon-1.19.2-1.13.jar", "biggerreactors-1.19.2-0.6.0-beta.0.3.jar",
      "blockui-1.19.2-0.0.43-ALPHA.jar", "blue_skies-1.19.2-1.3.4.jar", "bonsaitrees3-3.0.4.jar",
      "bos_api_1.19.2-0.1.0.jar", "Botania-1.19.2-429.jar", "Buddycards-1.19.2-3.0.6.jar",
      "buildersaddition-1.19.2-20220123a.jar", "burgermod-2.6.0-1.19.2.jar",
      "carryon-1.19.2-1.17.0.7.jar", "castle_in_the_sky-1.19.2-0.4.0.jar",
      "cloth-config-6.2.57-forge.jar", "cobbled_paths-forge-0.1.6.jar", "collective-1.19.2-4.7.jar",
      "comforts-forge-1.19.2-5.0.0.2.jar", "connectivity-1.19.2-2.6.jar",
      "cookietology-1.19.2-1.1.0.jar", "copper-mod-1.9.jar", "CosmeticArmorReworked-1.19.2-v1a.jar",
      "create-mc1.19.2_v0.4f.jar", "createaddition-1.19.2-20220125a.jar",
      "createfa-1.19.2_v1.0.8.jar", "critical_version_enforcer_1.19.2-1.0.0.jar",
      "Croptopia-1.19.2-FORGE-1.8.3.jar", "curios-forge-1.19.2-5.0.6.2.jar",
      "curious-armor-stands-1.19.2-4.0.0.jar", "Decorative Blocks-forge-1.19.2-2.0.4.jar",
      "dimdungeons-1.13.4-forge-1.19.2.jar", "domum_ornamentum-1.19.2-1.0.49-ALPHA-universal.jar",
      "DungeonsArise-1.19.2-2.1.49d-beta.jar", "EasyMagic-v3.2.1-1.19.2-Forge.jar",
      "easy_mob_farm_1.19.2-0.3.2.jar", "easy_piglins-1.19.2-1.0.0.jar",
      "easy_villagers-1.19.2-1.0.2.jar", "emotecraft-for-MC1.19.2-2.1-forge.jar",
      "endrem_forge-5.0.1-R-1.19.2.jar", "EnigmaticLegacy-2.17.3.jar",
      "expandability-5.0.0-forge.jar", "ExtraDisks-1.19.2-2.0.2.jar",
      "FarmersDelight-1.19.2-1.0.3.jar", "FastLeafDecay-27.2.jar",
      "feature_nbt_deadlock_be_gone_forge-1.0.0+1.19.2.jar", "ferritecore-4.1.2-forge.jar",
      "fire_extinguisher_1.19.2-1.6.1.jar", "flywheel-forge-1.19.2-0.6.1.jar",
      "forbidden_arcanus-1.19.2-2.0.3.jar", "ForgeEndertech-1.19.2-8.0.1.0-build.0059.jar",
      "framework-0.2.3-1.19.2.jar", "ftb-chunks-forge-1801.3.4-build.127.jar",
      "ftb-essentials-1801.1.6-build.37.jar", "ftb-library-forge-1801.3.5-build.109.jar",
      "ftb-teams-forge-1801.2.5-build.48.jar", "furniture-7.0.0-pre28-1.19.2.jar",
      "glow_sticks_1.19.2-1.2.1.jar", "Gobber2-Forge-1.19.2-2.6.20.jar",
      "goblintraders-1.7.0-1.19.2.jar", "guardvillagers-1.19.2.4.0.jar", "gun-mod-1.1.3-1.19.2.jar",
      "gunswithoutroses-1.19.2-1.0.11.jar", "HammerLib-1.19.2-18.1.11.jar",
      "healingcampfire_1.19.2-3.4.jar", "hexerei-0.1.14.jar", "humancompanions-1.19.2-1.4.4.jar",
      "HunterIllager-1.19.2-3.1.6.jar", "Iceberg-1.19.2-1.0.40.jar", "InsaneLib-1.4.3-mc1.19.2.jar",
      "Jade-1.19.2-4.4.1.jar", "jei-1.19.2-9.4.1.168.jar", "jepp-1.19.2-1.0.0.jar",
      "JustEnoughProfessions-1.19.2-1.2.2.jar", "JustEnoughResources-1.19.2-0.14.0.157.jar",
      "justmobheads_1.19.2-5.3.jar", "libnonymous-2.0.5.jar",
      "LibraryFerret-Forge-1.19.2-1.0.3.jar", "LibX-1.19.2-3.1.18.jar", "lobby_1.19.2-2.2.0.jar",
      "lootr-1.19.2-0.1.15.50.jar", "lucky-block-forge-1.19.2-11.0.jar", "malum-1.19.2-1.2.2.jar",
      "Mantle-1.19.2-1.8.37.jar", "material_elements_1.19.2-3.0.1.jar",
      "material_elements_armor_tools_weapons_1.19.2-3.0.2.jar",
      "material_elements_decorative_1.19.2-3.0.1.jar", "mcjtylib-1.19.2-6.0.10.jar",
      "mcw-bridges-2.0.2-mc1.19.2.jar", "mcw-doors-1.0.6-mc1.19.2.jar",
      "mcw-fences-1.0.3-mc1.19.2.jar", "mcw-lights-1.0.3-mc1.19.2.jar",
      "mcw-roofs-2.1.1-mc1.19.2.jar", "mcw-trapdoors-1.0.5-mc1.19.2.jar",
      "mcw-windows-2.0.3-mc1.19.2.jar", "Measurements-1.19.2-1.2.jar",
      "minecolonies-1.19.2-1.0.428-RELEASE.jar", "MmmMmmMmmMmm-1.19.2-1.5.1.jar",
      "moveboats_1.19.2-1.9.jar", "movingelevators-1.3.5-mc1.19.2.jar",
      "multi-piston-1.19.2-1.2.2-ALPHA.jar", "mutil-1.19.2-4.4.0.jar",
      "MythicBotany-1.19.2-2.0.2.jar", "neoncraft2-2.1.jar",
      "netherportalfix-forge-1.19.2-9.0.0.jar", "nohostilesaroundcampfire_1.19.2-3.9.jar",
      "no_null_processors_forge-2.0.2+1.19.2.jar", "Patchouli-1.19.2-65. jar",
      "phosphophyllite-1.19.2-0.6.0-beta.0.3.jar", "PickUpNotifier-v3.1.0-1.19.2-Forge.jar",
      "Ping-1.19.2-1.8.0.jar", "PizzaCraft-1.19.2-5.0.0.jar", "player_companions_1.19.2-0.8.0.jar",
      "polymorph-forge-1.19.2-0.40.jar", "potionsmaster-0.5.2-1.19.2-39.0.19.jar",
      "primalmagick-2.0.5.jar", "ProgressiveBosses-3.5.4-mc1.19.2.jar",
      "Public GUI Announcement-forge-1.19.2-3.2.0.0.jar", "PuzzlesLib-v3.2.1-1.19.2-Forge.jar",
      "quartz-1.19.2-0.0.0-beta.0.2.jar", "recast_1.19.2-1.6.jar", "refinedpipes-0.6.1.jar",
      "refinedstorage-1.10.1.jar", "refinedstorageaddons-0.8.1.jar",
      "repurposed_structures_forge-4.2.11+1.19.2.jar", "rftoolsbase-1.19.2-3.0.6.jar",
      "rftoolsbuilder-1.19.2-4.0.7.jar", "rftoolspower-1.19.2-4.0.3.jar",
      "rftoolsstorage-1.19.2-3.0.6.jar", "rftoolsutility-1.19.2-4.0.10.jar",
      "scuba_gear-1.19.2-1.0.4.jar", "selene-1.19.2-1.13.2.jar", "ServerTabInfo-1.19.2-1.3.4.jar",
      "shieldmechanics-1.19.2-2.9.jar", "Shrink-1.19.2-1.3.0.jar",
      "simplylight-1.19.2-1.4.0-build.26.jar", "sit-1.19.2-1.3.1.jar", "SkyVillages 1.0.11.18.jar",
      "SoL-Carrot-1.19.2-1.12.0.jar", "SolarFluxReborn-1.19.2-18.1.3.jar",
      "sophisticatedbackpacks-1.19.2-3.15.15.550.jar", "spark-1.8.6-forge.jar",
      "stackrefill_1.19.2-2.1.jar", "Statues-1.19.2-0.1.8.jar", "stoneholm-1.4.1.jar",
      "StorageDrawers-1.19.2-10.1.1.jar", "structure_gel-1.19.2-2.1.3.jar",
      "structurize-1.19.2-1.0.342-RELEASE.jar", "supermartijn642configlib-1.0.9-mc1.19.2.jar",
      "supermartijn642corelib-1.0.16b-mc1.19.2.jar", "supplementaries-1.19.2-1.2.4.jar",
      "swingthroughgrass-1.19.2-1.8.0.jar", "TConstruct-1.19.2-3.4.2.60.jar",
      "tetra-1.19.2-4.8.1.jar", "the-conjurer-1.19.2-1.1.1.jar", "The_Undergarden-1.19.2-0.6.3.jar",
      "token_coins_1.19.2-2.3.1.jar", "Uppers-0.4.0.jar", "valhelsia_core-1.19.2-0.3.2.jar",
      "valhelsia_structures-1.19.2-0.1.1.jar", "villagerdeathmessages_1.19.2-2.2.jar",
      "VisualWorkbench-v3.1.0-1.19.2-Forge.jar", "WaterStrainer-1.19.2-12.1.0.jar",
      "waystones-forge-1.19.2-9.0.4.jar", "witchesbrew-1.19.2-1.0.jar", "worldedit-mod-7.2.9.jar",
      "Xaeros_Minimap_22.3.0_Forge_1.19.2.jar", "YungsApi-1.19.2-Forge-26.jar",
      "YungsBridges-Forge-1.19.2-1.0.jar", "YungsExtras-Forge-1.19.2-1.0.jar",
      "rubidium-extra-0.4.16+mc1.19.2-build.57.jar", "[1.19.2] SecurityCraft v1.9.0.2-beta1.jar",
      "Xaeros_Minimap_22.4.0_Forge_1.19.2.jar"};

  public static String[] clientModList = {"BetterAdvancements-1.19.2-0.1.2.122.jar",
      "BetterF3-1.2.2-Forge-1.19.2.jar", "Controlling-forge-1.19.2-9.0+15.jar",
      "Ding-1.19.2-1.3.0.jar", "EnchantmentDescriptions-Forge-1.19.2-9.0.13.jar",
      "EquipmentCompare-1.19.2-1.2.12.jar", "FpsReducer2-forge-1.19.2-2.0.jar",
      "GameMenuModOption-1.19.2-Forge-1.14.jar", "GameMenuRemoveGFARB-1.19.2-Forge-1.4.jar",
      "ItemsDontBreak-1.19.2-0.5.0.jar", "LegendaryTooltips-1.19.2-1.2.4.jar",
      "bettersigns-1.0.jar", "InventoryProfilesNext-forge-1.19.2-1.3.5.jar",
      "chat_heads-0.6.0-forge-1.19.2.jar", "notenoughanimations-forge-1.5.0-mc1.19.2.jar",
      "eatinganimation-1.19.2-2.0.1.jar", "farsight-1.19.2-1.6.jar", "itemzoom-1.19.2-2.5.0.jar"};

  public static String[] modListDuplicates = {"adaptive_performance_tweaks_items_1.19.2-2.2.0.jar",
      "adaptive_performance_tweaks_items_1.19.2-2.3.0.jar",
      "adaptive_performance_tweaks_items_1.19.2-2.2.1.jar",
      "adaptive_performance_tweaks_items_1.19.2-2.2.0.jar",
      "adaptive_performance_tweaks_items_1.19.2-2.3.4.jar",
      "adaptive_performance_tweaks_items_1.19.2-2.2.0.jar",
      "adaptive_performance_tweaks_items_1:.18:.1:-2:.2:.0:.jar",
      "adaptive_performance_tweaks_items_1+.18+.1+-2+.2+.0+.jar",
      "adaptive_performance_tweaks_items_1.19.2-2.2.9.jar",
      "adaptive_performance_tweaks_items_1.19.2-2.2.9a.jar",
      "adaptive_performance_tweaks_items_1.19.2-2.2.9b.jar",
      "adaptive_performance_tweaks_items_1.19.2-2.2.9c.jar",
      "adaptive_performance_tweaks_items_1.19.2-2.2.10.jar"};

  public static String[] modListDuplicatesSpecific01 =
      {"ProgressiveBosses-3.5.16-mc1.19.2.jar", "ProgressiveBosses-3.5.11-mc1.19.2.jar"};

  public static String[] modListDuplicatesSpecific02 =
      {"createaddition-1.19.2-20220709a.jar", "createaddition-1.19.2-20220517a.jar"};

  public static String[] modListDuplicatesSpecific03 =
      {"mcw-trapdoors-1.0.6-mc1.19.2.jar", "mcw-trapdoors-1.0.5-mc1.19.2.jar"};

  public static String[] modListDuplicatesSpecific04 =
      {"Patchouli-1.19.2-71.1.jar", "Patchouli-1.19.2-70.jar"};

  public static String[] modListDuplicatesSpecific05 =
      {"pneumaticcraft-repressurized-1.19.2-3.4.1-131.jar",
          "pneumaticcraft-repressurized-1.19.2-3.4.0-124.jar"};
}
