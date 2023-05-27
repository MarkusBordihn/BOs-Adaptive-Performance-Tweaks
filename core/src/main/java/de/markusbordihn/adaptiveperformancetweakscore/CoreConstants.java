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

package de.markusbordihn.adaptiveperformancetweakscore;

import net.minecraftforge.fml.ModList;

public final class CoreConstants {

  protected CoreConstants() {}

  // Config Prefix
  public static final String CONFIG_ID = "adaptive_performance_tweaks";
  public static final String CONFIG_ID_PREFIX = CONFIG_ID + "/";

  // General Mod definitions
  public static final String ISSUE_REPORT =
      "https://github.com/MarkusBordihn/BOs-Adaptive-Performance-Tweaks/issues";

  // List of Mod IDs for easier maintenance and access

  public static final String ALEXSMOBS_MOD = "alexsmobs";
  public static final String ALEXSMOBS_NAME = "Alex's Mobs";
  public static final boolean ALEXSMOBS_LOADED = ModList.get().isLoaded(ALEXSMOBS_MOD);

  public static final String ADHOOKS_MOD = "adhooks";
  public static final String ADHOOKS_NAME = "Advanced Hook Launchers";
  public static final boolean ADHOOKS_LOADED = ModList.get().isLoaded(ADHOOKS_MOD);

  public static final String ARTIFACTS_MOD = "artifacts";
  public static final String ARTIFACTS_NAME = "Artifacts";
  public static final boolean ARTIFACTS_LOADED = ModList.get().isLoaded(ARTIFACTS_MOD);

  public static final String ARS_NOUVEAU_MOD = "ars_nouveau";
  public static final String ARS_NOUVEAU_NAME = "Ars Nouveau";
  public static final boolean ARS_NOUVEAU_LOADED = ModList.get().isLoaded(ARS_NOUVEAU_MOD);

  public static final String APPLIED_ENERGISTICS_2_MOD = "ae2";
  public static final String APPLIED_ENERGISTICS_2_NAME = "Applied Energistics 2";
  public static final boolean APPLIED_ENERGISTICS_2_LOADED =
      ModList.get().isLoaded(APPLIED_ENERGISTICS_2_MOD);

  public static final String AQUACULTURE_MOD = "aquaculture";
  public static final String AQUACULTURE_NAME = "Aquaculture 2";
  public static final boolean AQUACULTURE_LOADED = ModList.get().isLoaded(AQUACULTURE_MOD);

  public static final String BIGGER_REACTORS_MOD = "biggerreactors";
  public static final String BIGGER_REACTORS_NAME = "Bigger Reactors";
  public static final boolean BIGGER_REACTORS_LOADED = ModList.get().isLoaded(BIGGER_REACTORS_MOD);

  public static final String BORN_IN_CHAOS_MOD = "born_in_chaos_v1";
  public static final String BORN_IN_CHAOS_NAME = "Born in Chaos";
  public static final boolean BORN_IN_CHAOS_LOADED = ModList.get().isLoaded(BORN_IN_CHAOS_MOD);

  public static final String BOTANIA_MOD = "botania";
  public static final String BOTANIA_NAME = "Botania";
  public static final boolean BOTANIA_LOADED = ModList.get().isLoaded(BOTANIA_MOD);

  public static final String COFH_CORE_MOD = "cofh_core";
  public static final String COFH_CORE_NAME = "CoFH Core";
  public static final boolean COFH_CORE_LOADED = ModList.get().isLoaded(COFH_CORE_MOD);

  public static final String CLUMPS_MOD = "clumps";
  public static final String CLUMPS_NAME = "Clumps";
  public static final boolean CLUMPS_LOADED = ModList.get().isLoaded(CLUMPS_MOD);

  public static final String CREATE_MOD = "create";
  public static final String CREATE_NAME = "Create";
  public static final boolean CREATE_LOADED = ModList.get().isLoaded(CREATE_MOD);

  public static final String DYNVIEW_MOD = "dynview";
  public static final String DYNVIEW_NAME = "Dynview";
  public static final boolean DYNVIEW_LOADED = ModList.get().isLoaded(DYNVIEW_MOD);

  public static final String EASY_NPC_MOD = "easy_npc";
  public static final String EASY_NPC_NAME = "Easy NPC";
  public static final boolean EASY_NPC_LOADED = ModList.get().isLoaded(EASY_NPC_MOD);

  public static final String FISH_OF_THIEVES_MOD = "fishofthieves";
  public static final String FISH_OF_THIEVES_NAME = "Fish's Undead Rising";
  public static final boolean FISH_OF_THIEVES_LOADED = ModList.get().isLoaded(FISH_OF_THIEVES_MOD);

  public static final String FRIENDS_AND_FOES_MOD = "friendsandfoes";
  public static final String FRIENDS_AND_FOES_NAME = "Friends and Foes";
  public static final boolean FRIENDS_AND_FOES_LOADED =
      ModList.get().isLoaded(FRIENDS_AND_FOES_MOD);

  public static final String FTB_ULTIMINE_MOD = "ftbultimine";
  public static final String FTB_ULTIMINE_NAME = "FTB Ultimine";
  public static final boolean FTB_ULTIMINE_LOADED = ModList.get().isLoaded(FTB_ULTIMINE_MOD);

  public static final String FLUX_NETWORKS_MOD = "fluxnetworks";
  public static final String FLUX_NETWORKS_NAME = "Flux Networks";
  public static final boolean FLUX_NETWORKS_LOADED = ModList.get().isLoaded(FLUX_NETWORKS_MOD);

  public static final String GET_IT_TOGETHER_MOD = "getittogetherdrops";
  public static final String GET_IT_TOGETHER_NAME = "Get It Together, Drops!";
  public static final boolean GET_IT_TOGETHER_LOADED = ModList.get().isLoaded(GET_IT_TOGETHER_MOD);

  public static final String GUARD_VILLAGERS_MOD = "guardvillagers";
  public static final String GUARD_VILLAGERS_NAME = "Guard Villagers";
  public static final boolean GUARD_VILLAGERS_LOADED = ModList.get().isLoaded(GUARD_VILLAGERS_MOD);

  public static final String GOTHIC_MOD = "gothic";
  public static final String GOTHIC_NAME = "Gothic RPG";
  public static final boolean GOTHIC_LOADED = ModList.get().isLoaded(GOTHIC_MOD);

  public static final String HUMAN_COMPANIONS_MOD = "humancompanions";
  public static final String HUMAN_COMPANIONS_NAME = "Human Companions";
  public static final boolean HUMAN_COMPANIONS_LOADED =
      ModList.get().isLoaded(HUMAN_COMPANIONS_MOD);

  public static final String INDUSTRIAL_FOREGOING_MOD = "industrialforegoing";
  public static final String INDUSTRIAL_FOREGOING_NAME = "Industrial Foregoing";
  public static final boolean INDUSTRIAL_FOREGOING_LOADED =
      ModList.get().isLoaded(INDUSTRIAL_FOREGOING_MOD);

  public static final String INFERNAL_EXPANSION_MOD = "infernalexp";
  public static final String INFERNAL_EXPANSION_NAME = "Infernal Expansion";
  public static final boolean INFERNAL_EXPANSION_LOADED =
      ModList.get().isLoaded(INFERNAL_EXPANSION_MOD);

  public static final String IMMERSIVE_ENGINEERING_MOD = "immersiveengineering";
  public static final String IMMERSIVE_ENGINEERING_NAME = "Immersive Engineering";
  public static final boolean IMMERSIVE_ENGINEERING_LOADED =
      ModList.get().isLoaded(IMMERSIVE_ENGINEERING_MOD);

  public static final String INCONTROL_MOD = "incontrol";
  public static final String INCONTROL_NAME = "InControl";
  public static final boolean INCONTROL_LOADED = ModList.get().isLoaded(INCONTROL_MOD);

  public static final String LOGIN_PROTECTION_MOD = "logprot";
  public static final String LOGIN_PROTECTION_NAME = "Login Protection";
  public static final boolean LOGIN_PROTECTION_LOADED =
      ModList.get().isLoaded(LOGIN_PROTECTION_MOD);

  public static final String LOOTR_MOD = "lootr";
  public static final String LOOTR_NAME = "Lootr";
  public static final boolean LOOTR_LOADED = ModList.get().isLoaded(LOOTR_MOD);

  public static final String MANA_AND_ARTIFICE_MOD = "mana-and-artifice";
  public static final String MANA_AND_ARTIFICE_NAME = "Mana and Artifice";
  public static final boolean MANA_AND_ARTIFICE_LOADED =
      ModList.get().isLoaded(MANA_AND_ARTIFICE_MOD);

  public static final String MEKANISM_MOD = "mekanism";
  public static final String MEKANISM_FILTER = MEKANISM_MOD + ":";
  public static final String MEKANISM_NAME = "Mekanism";
  public static final boolean MEKANISM_LOADED = ModList.get().isLoaded(MEKANISM_MOD);

  public static final String MEKANISMADDITIONS_MOD = "mekanismadditions";
  public static final String MEKANISMADDITIONS_NAME = "Mekanism Additions";
  public static final boolean MEKANISMADDITIONS_LOADED =
      ModList.get().isLoaded(MEKANISMADDITIONS_MOD);

  public static final String MODULAR_ROUTERS_MOD = "modularrouters";
  public static final String MODULAR_ROUTERS_NAME = "Modular Routers";
  public static final boolean MODULAR_ROUTERS_LOADED = ModList.get().isLoaded(MODULAR_ROUTERS_MOD);

  public static final String MINECOLONIES_MOD = "minecolonies";
  public static final String MINECOLONIES_NAME = "MineColonies";
  public static final boolean MINECOLONIES_LOADED = ModList.get().isLoaded(MINECOLONIES_MOD);

  public static final String PANTHALASSA_MOD = "panthalassa";
  public static final String PANTHALASSA_NAME = "Panthalassa";
  public static final boolean PANTHALASSA_LOADED = ModList.get().isLoaded(PANTHALASSA_MOD);

  public static final String PERFORMANT_MOD = "performant";
  public static final String PERFORMANT_NAME = "Performant";
  public static final boolean PERFORMANT_LOADED = ModList.get().isLoaded(PERFORMANT_MOD);

  public static final String PIPEZ_MOD = "pipez";
  public static final String PIPEZ_NAME = "Pipez";
  public static final boolean PIPEZ_LOADED = ModList.get().isLoaded(PIPEZ_MOD);

  public static final String POKECUBE_AIO_MOD = "pokecube";
  public static final String POKECUBE_AIO_NAME = "Pokecube AIO";
  public static final boolean POKECUBE_AIO_LOADED = ModList.get().isLoaded(POKECUBE_AIO_MOD);

  public static final String RUBIDIUM_MOD = "rubidium";
  public static final String RUBIDIUM_NAME = "Rubidium";
  public static final boolean RUBIDIUM_LOADED = ModList.get().isLoaded(RUBIDIUM_MOD);

  public static final String REFINED_STORAGE_MOD = "refinedstorage";
  public static final String REFINED_STORAGE_NAME = "Refined Storage";
  public static final boolean REFINED_STORAGE_LOADED = ModList.get().isLoaded(REFINED_STORAGE_MOD);

  public static final String STORAGE_DRAWERS_MOD = "storagedrawers";
  public static final String STORAGE_DRAWERS_NAME = "Storage Drawers";
  public static final boolean STORAGE_DRAWERS_LOADED = ModList.get().isLoaded(STORAGE_DRAWERS_MOD);

  public static final String SUPPLEMENTARIES_MOD = "supplementaries";
  public static final String SUPPLEMENTARIES_NAME = "Supplementaries";
  public static final boolean SUPPLEMENTARIES_LOADED = ModList.get().isLoaded(SUPPLEMENTARIES_MOD);

  public static final String TCONSTRUCT_MOD = "tconstruct";
  public static final String TCONSTRUCT_NAME = "Tinkers Construct";
  public static final boolean TCONSTRUCT_LOADED = ModList.get().isLoaded(TCONSTRUCT_MOD);

  public static final String RATS_MOD = "rats";
  public static final String RATS_NAME = "Rats";
  public static final boolean RATS_LOADED = ModList.get().isLoaded(RATS_MOD);

  public static final String SODIUM_MOD = "sodium";
  public static final String SODIUM_NAME = "Sodium";
  public static final boolean SODIUM_LOADED = ModList.get().isLoaded(SODIUM_MOD);

  public static final String QUARK_MOD = "quark";
  public static final String QUARK_NAME = "Quark";
  public static final boolean QUARK_LOADED = ModList.get().isLoaded(QUARK_MOD);

  public static final String UNTAMED_WILDS_MOD = "untamedwilds";
  public static final String UNTAMED_WILDS_NAME = "Untamed Wilds";
  public static final boolean UNTAMED_WILDS_LOADED = ModList.get().isLoaded(UNTAMED_WILDS_MOD);

  public static final String UNTITLED_DUCK_MOD = "untitledduckmod";
  public static final String UNTITLED_DUCK_NAME = "Untitled Duck Mod";
  public static final boolean UNTITLED_DUCK_LOADED = ModList.get().isLoaded(UNTITLED_DUCK_MOD);

  public static final String ULTIMATE_CAR_MOD = "car";
  public static final String ULTIMATE_CAR_NAME = "Ultimate Car Mod";
  public static final boolean ULTIMATE_CAR_LOADED = ModList.get().isLoaded(ULTIMATE_CAR_MOD);

  public static final String VIESCRAFT_MACHINES_MOD = "viescraftmachines";
  public static final String VIESCRAFT_MACHINES_NAME = "ViesCraft Machines";
  public static final boolean VIESCRAFT_MACHINES_LOADED =
      ModList.get().isLoaded(VIESCRAFT_MACHINES_MOD);

  public static final String WHISPERWOODS_MOD = "whisperwoods";
  public static final String WHISPERWOODS_NAME = "Whisperwoods";
  public static final boolean WHISPERWOODS_LOADED = ModList.get().isLoaded(WHISPERWOODS_MOD);

  public static final String XNET_MOD = "xnet";
  public static final String XNET_NAME = "XNet";
  public static final boolean XNET_LOADED = ModList.get().isLoaded(XNET_MOD);
}
