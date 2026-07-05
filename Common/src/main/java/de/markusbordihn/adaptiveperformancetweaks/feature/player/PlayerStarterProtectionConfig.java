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
import java.util.Properties;

public final class PlayerStarterProtectionConfig extends Config {

  public static final String CONFIG_FILE_NAME = "player_starter_protection.cfg";
  private static final String CONFIG_FILE_HEADER =
    """
       Player Starter Protection Configuration
      
       Automatically applies damage reduction and attack bonuses to new players
       based on their experience level -- no name list required.
      
       A player is considered a "starter" as long as their level is below
       starterMaxExperienceLevel. The protection is removed automatically when
       they level up past the threshold.
      
       starterMaxExperienceLevel  -- XP level below which a player counts as starter (default: 20)
       starterHurtDamageReduction -- % of incoming damage absorbed for starters  (default: 25)
       starterAttackDamageIncrease -- % by which starter attack damage is boosted (default: 25)
       Percentages: 0 = disabled, 100 = full reduction/increase.
      """;

  public static int starterMaxExperienceLevel = 20;
  public static int starterHurtDamageReduction = 25;
  public static int starterAttackDamageIncrease = 25;

  private PlayerStarterProtectionConfig() {
  }

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodified = new Properties();
    unmodified.putAll(properties);

    CoreConfig.applyFeatureState(
      FeatureToggle.PLAYER_STARTER_PROTECTION,
      parseConfigValue(properties, "enabled",
        FeatureToggle.PLAYER_STARTER_PROTECTION.getDefaultState()));

    starterMaxExperienceLevel =
      parseConfigValue(properties, "starterMaxExperienceLevel", starterMaxExperienceLevel);
    starterHurtDamageReduction = Math.max(0, Math.min(100,
      parseConfigValue(properties, "starterHurtDamageReduction", starterHurtDamageReduction)));
    starterAttackDamageIncrease = Math.max(0,
      parseConfigValue(properties, "starterAttackDamageIncrease", starterAttackDamageIncrease));

    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodified);

    log.debug(
      "Starter protection config: maxLevel={}, hurtReduction={}%, attackIncrease={}%",
      starterMaxExperienceLevel, starterHurtDamageReduction, starterAttackDamageIncrease);
  }
}
