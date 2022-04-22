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
    "3dskinlayers-forge-1.4.3-mc1.18.2.jar",
    "BetterAdvancements-1.18.1-0.1.2.122.jar",
    "BetterF3-1.2.2-Forge-1.18.jar",
    "BetterModsButton-v3.2.1-1.18.2-Forge.jar",
    "BetterThirdPerson-Forge-1.18-1.6.0.jar",
    "BetterTitleScreen-1.18-1.12.0.jar",
    "Blur-1.1.0-1.jar",
    "ChunkAnimator-1.18-1.3.3.jar",
    "Controlling-forge-1.18.1-9.0+15.jar",
    "Ding-1.18-1.3.0.jar",
    "EnchantmentDescriptions-Forge-1.18.2-10.0.2.jar",
    "EquipmentCompare-1.18.1-1.2.12.jar",
    "FPS-Monitor-1.18.2-1.2.1.jar",
    "Fallingleaves-1.18.1-1.3.1.jar",
    "FpsReducer2-forge-1.18.2-2.0.jar",
    "GameMenuModOption-1.18-Forge-1.14.jar",
    "GameMenuRemoveGFARB-1.18-Forge-1.4.jar",
    "Highlighter-1.18.1-1.1.2.jar",
    "IKnowWhatImDoing-5.0.0-forge.jar",
    "ImprovedMountHUD-v3.1.0-1.18.2-Forge.jar",
    "InvMove-1.18-0.7.0-Forge.jar",
    "InvMoveCompats-1.18-0.1.0-Forge.jar",
    "InventoryHud_[1.18].forge-3.4.4.jar",
    "InventoryProfilesNext-forge-1.18.2-1.3.5.jar",
    "ItemPhysicLite_v1.4.8_mc1.18.jar",
    "ItemsDontBreak-1.18-0.5.0.jar",
    "LegendaryTooltips-1.18.1-1.2.4.jar",
    "MouseTweaks-forge-mc1.18-2.21.jar",
    "MyServerIsCompatible-1.18-1.0.jar",
    "Neat 1.8-30.jar",
    "NekosEnchantedBooks-1.18.2-1.6.1.jar",
    "Notes-1.18.1-1.2.4.jar",
    "ReAuth-1.18-Forge-4.0.0.jar",
    "TipTheScales-forge-1.18.2-5.0.5.jar",
    "ToastControl-1.18.2-6.0.2.jar",
    "beautifiedchatclient_1.18.1-1.1.jar",
    "better-loading-screen-1.4.0.jar",
    "betterbiomeblend-1.18.1-1.3.2-forge.jar",
    "betterfpsdist-1.18.1-1.4.jar",
    "bettersigns-1.0.jar",
    "catalogue-1.6.0-1.18.1.jar",
    "chat_heads-0.6.0-forge-1.18.1.jar",
    "clienttweaks-forge-1.18.1-7.1.0.jar",
    "craftingtweaks-forge-1.18.1-14.0.2.jar",
    "drippyloadingscreen_forge_1.5.1_MC_1.18-1.18.1.jar",
    "drawerfps-1.18.2-2.3.jar",
    "flickerfix-2.0.0.jar",
    "eatinganimation-1.18.1-2.0.1.jar",
    "entityculling-forge-mc1.18-1.5.0.jar",
    "extremeSoundMuffler-3.24_Forge-1.18.1.jar",
    "fancymenu_forge_2.6.4_MC_1.18.2.jar",
    "farsight-1.18-1.6.jar",
    "guiclock_1.18.2-3.1.jar",
    "guicompass_1.18.2-2.2.jar",
    "guifollowers_1.18.2-1.9.jar",
    "hiddenrecipebook_1.18.2-2.4.jar",
    "itemzoom-1.18.1-2.5.0.jar",
    "namepain-1.4.1 forge-1.18.x.jar",
    "notenoughanimations-forge-1.5.0-mc1.18.2.jar",
    "oculus-mc1.18.2-1.2.2b.jar",
    "replanter-forge-1.3.jar",
    "rubidium-0.5.2a.jar",
    "screenscale-1.18.2-2.0.jar"
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
        .replaceAll("(-(mc)?[^A-Za-z_]+)[a-z]?.jar$", ".jar")
        .replaceAll("(_(mc)?[^A-Za-z_]+)[a-z]?.jar$", ".jar")
        .replaceAll("(.[^A-Za-z_]+)[a-z]?.jar$", ".jar").replaceAll("(?i)(mc[^A-Z_]+)", "")
        .replaceAll("(-[^A-Za-z_]+)", "-").replaceAll("(_[^A-Za-z_]+)", "_")
        .replaceAll("(v[^A-Za-z_-]+)", "").replaceAll("(1.18.1|1.18.2)", "").replace("--", "-")
        .replace("__", "_").replace("[]", "").replace(" .jar", ".jar")
        .replaceAll("(-)+jar$", ".jar").replaceAll("(_)+jar$", ".jar")
        .replaceAll("[a-z]{1}jar$", ".jar");
  }

}
