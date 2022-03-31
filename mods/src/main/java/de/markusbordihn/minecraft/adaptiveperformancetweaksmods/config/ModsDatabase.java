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
import java.util.Set;

public class ModsDatabase {

  protected ModsDatabase() {}

  public static final Set<String> clientSideMods = new HashSet<>(Arrays.asList(
  // Remove version strings with RegExp in two steps:
  // 1. ReplaceAll (-(mc)?[^A-Za-z_]+).jar with ".jar"
  // 2. ReplaceAll (-[^A-Za-z_]+) with "-"
  // 3. Replace -jar" with .jar"
  // @formatter:off
    "BetterAdvancements.jar",
    "BetterF3-Forge.jar",
    "Controlling-forge.jar",
    "Ding.jar",
    "EnchantmentDescriptions-Forge.jar",
    "EquipmentCompare.jar",
    "FpsReducer2-forge.jar",
    "GameMenuModOption-Forge.jar",
    "GameMenuRemoveGFARB-Forge.jar",
    "ItemsDontBreak.jar",
    "LegendaryTooltips.jar",
    "bettersigns.jar",
    "chat_heads-forge.jar",
    "configured.jar",
    "eatinganimation.jar",
    "farsight.jar",
    "itemzoom.jar",
    "notenoughanimations-forge.jar"
  // @formatter:on
  ));

}
