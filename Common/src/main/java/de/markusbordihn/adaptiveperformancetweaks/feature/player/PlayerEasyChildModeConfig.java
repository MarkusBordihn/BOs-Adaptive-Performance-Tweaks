/*
 * Copyright 2024 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.adaptiveperformancetweaks.feature.player;

import de.markusbordihn.adaptiveperformancetweaks.core.config.Config;
import de.markusbordihn.adaptiveperformancetweaks.core.config.CoreConfig;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import java.io.File;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;

public final class PlayerEasyChildModeConfig extends Config {

  public static final String CONFIG_FILE_NAME = "player_easy_child_mode.cfg";
  private static final String CONFIG_FILE_HEADER =
    """
       Player Easy Child Mode Configuration
      
       List player names that should receive reduced hurt damage and increased attack damage.
       Percentages: 0 = disabled, 100 = full reduction/increase.
      """;

  public static Set<String> childPlayerNames = new HashSet<>();
  public static int childPlayerHurtDamageReduction = 50;
  public static int childPlayerAttackDamageIncrease = 50;

  private PlayerEasyChildModeConfig() {
  }

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodified = new Properties();
    unmodified.putAll(properties);

    CoreConfig.applyFeatureState(
      FeatureToggle.PLAYER_EASY_CHILD_MODE,
      parseConfigValue(properties, "enabled",
        FeatureToggle.PLAYER_EASY_CHILD_MODE.getDefaultState()));

    childPlayerNames = parseConfigValue(properties, "childPlayerNames", childPlayerNames);
    childPlayerHurtDamageReduction = parseConfigValue(properties, "childPlayerHurtDamageReduction",
      childPlayerHurtDamageReduction);
    childPlayerAttackDamageIncrease = parseConfigValue(properties,
      "childPlayerAttackDamageIncrease",
      childPlayerAttackDamageIncrease);

    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodified);

    log.debug(
      "Child mode config: players={}, hurtReduction={}%, attackIncrease={}%",
      childPlayerNames,
      childPlayerHurtDamageReduction,
      childPlayerAttackDamageIncrease);
  }
}
