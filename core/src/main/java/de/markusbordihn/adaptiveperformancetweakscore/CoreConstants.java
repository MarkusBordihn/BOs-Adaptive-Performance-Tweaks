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

  public static final String ADHOOKS_MOD = "adhooks";
  public static final String ADHOOKS_NAME = "Advanced Hook Launchers";
  public static final boolean ADHOOKS_LOADED = ModList.get().isLoaded(ADHOOKS_MOD);

  public static final String CLUMPS_MOD = "clumps";
  public static final String CLUMPS_NAME = "Clumps";
  public static final boolean CLUMPS_LOADED = ModList.get().isLoaded(CLUMPS_MOD);

  public static final String DYNVIEW_MOD = "dynview";
  public static final String DYNVIEW_NAME = "Dynview";
  public static final boolean DYNVIEW_LOADED = ModList.get().isLoaded(DYNVIEW_MOD);

  public static final String FTB_ULTIMINE_MOD = "ftbultimine";
  public static final String FTB_ULTIMINE_NAME = "FTB Ultimine";
  public static final boolean FTB_ULTIMINE_LOADED = ModList.get().isLoaded(FTB_ULTIMINE_MOD);

  public static final String INCONTROL_MOD = "incontrol";
  public static final String INCONTROL_NAME = "InControl";
  public static final boolean INCONTROL_LOADED = ModList.get().isLoaded(INCONTROL_MOD);

  public static final String LOOTR_MOD = "lootr:";
  public static final String LOOTR_NAME = "Lootr";
  public static final boolean LOOTR_LOADED = ModList.get().isLoaded(LOOTR_MOD);

  public static final String MINECOLONIES_MOD = "minecolonies";
  public static final String MINECOLONIES_NAME = "MineColonies";
  public static final boolean MINECOLONIES_LOADED = ModList.get().isLoaded(MINECOLONIES_MOD);

  public static final String RUBIDIUM_MOD = "rubidium";
  public static final String RUBIDIUM_NAME = "Rubidium";
  public static final boolean RUBIDIUM_LOADED = ModList.get().isLoaded(RUBIDIUM_MOD);

  public static final String PERFORMANT_MOD = "performant";
  public static final String PERFORMANT_NAME = "Performant";
  public static final boolean PERFORMANT_LOADED = ModList.get().isLoaded(PERFORMANT_MOD);

  public static final String MANA_AND_ARTIFICE_MOD = "mana-and-artifice";
  public static final String MANA_AND_ARTIFICE_NAME = "Mana and Artifice";
  public static final boolean MANA_AND_ARTIFICE_LOADED =
      ModList.get().isLoaded(MANA_AND_ARTIFICE_MOD);

  public static final String RATS_MOD = "rats";
  public static final String RATS_NAME = "Rats";

  public static final String SODIUM_MOD = "sodium";
  public static final String SODIUM_NAME = "Sodium";
  public static final boolean SODIUM_LOADED = ModList.get().isLoaded(SODIUM_MOD);
}
