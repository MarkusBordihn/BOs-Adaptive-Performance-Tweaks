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

package de.markusbordihn.minecraft.adaptiveperformancetweaksmods.config;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class ModsDatabase {

  protected ModsDatabase() {}

  /**
   * List of client side mods, which are confirmed to be client-side only!
   */
  public static final List<String> clientSideModsList = Arrays.asList(
  // @formatter:off
    "3dskinlayers-forge-1.4.3-mc1.19.2.jar",
    "BetterAdvancements-1.19-0.1.2.122.jar",
    "BetterF3-1.2.2-Forge-1.18.jar",
    "BetterModsButton-v3.2.1-1.19-Forge.jar",
    "BetterThirdPerson-Forge-1.19.6.0.jar",
    "BetterTitleScreen-1.19.12.0.jar",
    "Blur-1.1.0-1.jar",
    "ChunkAnimator-1.19.3.3.jar",
    "Controlling-forge-1.19-9.0+15.jar",
    "Ding-1.19.3.0.jar",
    "EnchantmentDescriptions-Forge-1.19-10.0.2.jar",
    "EquipmentCompare-1.19-1.2.12.jar",
    "FPS-Monitor-1.19-1.2.1.jar",
    "Fallingleaves-1.19-1.3.1.jar",
    "FpsReducer2-forge-1.19-2.0.jar",
    "GameMenuModOption-1.19-Forge-1.14.jar",
    "GameMenuRemoveGFARB-1.19-Forge-1.4.jar",
    "Highlighter-1.19-1.1.2.jar",
    "IKnowWhatImDoing-5.0.0-forge.jar",
    "ImprovedMountHUD-v3.1.0-1.19-Forge.jar",
    "InvMove-1.19-0.7.0-Forge.jar",
    "InvMoveCompats-1.19-0.1.0-Forge.jar",
    "InventoryHud_[1.19].forge-3.4.4.jar",
    "InventoryProfilesNext-forge-1.19-1.3.5.jar",
    "ItemPhysicLite_v1.4.8_mc1.19.jar",
    "ItemsDontBreak-1.19-0.5.0.jar",
    "LegendaryTooltips-1.19-1.2.4.jar",
    "MouseTweaks-forge-mc1.19-2.21.jar",
    "MyServerIsCompatible-1.19.0.jar",
    "Neat 1.8-30.jar",
    "NekosEnchantedBooks-1.19-1.6.1.jar",
    "Notes-1.19-1.2.4.jar",
    "ReAuth-1.19-Forge-4.0.0.jar",
    "TipTheScales-forge-1.19-5.0.5.jar",
    "ToastControl-1.19-6.0.2.jar",
    "auudio_forge_1.0.3_MC_1.19.jar",
    "beautifiedchatclient_1.19-1.1.jar",
    "better-loading-screen-1.4.0.jar",
    "betteranimalmodels-1.19-5.6.0-forge.jar",
    "betterbiomeblend-1.19-1.3.2-forge.jar",
    "betterfpsdist-1.19-1.4.jar",
    "bettersigns-1.0.jar",
    "catalogue-1.6.0-1.19.jar",
    "chat_heads-0.6.0-forge-1.19.jar",
    "clear-skies-forge-mc119-2.0.94.jar",
    "clienttweaks-forge-1.19-7.1.0.jar",
    "drawerfps-1.19-2.3.jar",
    "drippyloadingscreen_forge_1.5.1_MC_1.18-1.19.jar",
    "eatinganimation-1.19-2.0.1.jar",
    "entityculling-forge-mc1.19-1.5.0.jar",
    "extremeSoundMuffler-3.24_Forge-1.19.jar",
    "fancymenu_forge_2.6.4_MC_1.18.2.jar",
    "farsight-1.19.6.jar",
    "flickerfix-2.0.0.jar",
    "fm_audio_extension_forge_1.1.0_MC_1.19.jar",
    "firstperson-forge-2.1.0-mc1.19.jar",
    "guiclock_1.19-3.1.jar",
    "guicompass_1.19-2.2.jar",
    "guifollowers_1.19-1.9.jar",
    "hiddenrecipebook_1.19-2.4.jar",
    "itemzoom-1.19-2.5.0.jar",
    "justzoom_forge_1.0.1_MC_1.18-1.19.jar",
    "loadmyresources_1.0.1_MC_1.19.jar",
    "modernworldcreation_forge_1.0.0_MC_1.19.jar",
    "namepain-1.4.1 forge-1.18.x.jar",
    "notenoughanimations-forge-1.5.0-mc1.19.2.jar",
    "oculus-mc1.19.2-1.2.4.jar",
    "replanter-forge-1.3.jar",
    "rubidium-0.5.2a.jar",
    "screenscale-1.19-2.0.jar",
    "spyglass_improvements-1.2+mc1.19.x+forge.jar",
    "uiinputundo-1.2.jar",
    "waveycapes-forge-1.2.0-mc1.19.jar"
  // @formatter:on
  );

  /**
   * Aggregated list of client side modes, without version numbers to make them compatible across
   * versions. It could be that in the future this needs to be splitted to the different version
   * numbers.
   */
  public static final Set<String> clientSideMods =
      new HashSet<>(clientSideModsList.stream().map(ModsDatabase::stripeVersionNumbers).toList());

  public static String stripeVersionNumbers(String name) {
    // Remove version strings with RegExp in several steps.
    // This is needed, because there are two many variants to cover everything with a single check.
    return name.replaceAll("(?i)(-)?(ALPHA|BETA|RELEASE)(-)?", "")
        .replaceAll("(forge|FORGE|Forge)", "").replaceAll("(-(mc)?[^A-Za-z_]+)[a-z]?.jar$", ".jar")
        .replaceAll("(_(mc)?[^A-Za-z_]+)[a-z]?.jar$", ".jar")
        .replaceAll("(.[^A-Za-z_]+)[a-z]?.jar$", ".jar").replaceAll("(?i)(mc[^A-Z_]+)", "")
        .replaceAll("(-[^A-Za-z_]+)", "-").replaceAll("(_[^A-Za-z_]+)", "_")
        .replaceAll("(v[^A-Za-z_-]+)", "").replaceAll("(1.19|1.19.1)", "").replaceAll("-{2,}", "-")
        .replaceAll("_{2,}", "_").replace("[]", "").replace(" .jar", ".jar")
        .replaceAll("(-)+jar$", ".jar").replaceAll("(_)+jar$", ".jar")
        .replaceAll("[a-z]{1}jar$", ".jar");
  }

}
