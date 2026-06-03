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

package de.markusbordihn.adaptiveperformancetweaks.feature.items;

import de.markusbordihn.adaptiveperformancetweaks.core.config.Config;
import de.markusbordihn.adaptiveperformancetweaks.core.config.CoreConfig;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import java.io.File;
import java.util.Properties;

public final class ExperienceOrbsConfig extends Config {

  public static final String CONFIG_FILE_NAME = "experience_orbs.cfg";
  private static final String CONFIG_FILE_HEADER =
    """
       Experience Orbs Feature Configuration
      
       Controls XP orb merging to reduce entity count.
       Orbs within clusterRange blocks will be merged into a single orb.
       Optionally removes very old XP orbs that survive far beyond normal lifetime.
      """;

  public static ServerLoadLevel minOptimizationLoadLevel = ServerLoadLevel.NORMAL;
  public static boolean optimizeExperienceOrbs = true;
  public static int experienceOrbsClusterRange = 2;
  public static boolean movePositionToLastDrop = false;
  public static boolean removeStaleExperienceOrbs = true;
  public static int staleExperienceOrbAgeTicks = 120_000;

  private ExperienceOrbsConfig() {
  }

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodified = new Properties();
    unmodified.putAll(properties);

    CoreConfig.applyFeatureState(
      FeatureToggle.EXPERIENCE_ORBS,
      parseConfigValue(properties, "enabled", FeatureToggle.EXPERIENCE_ORBS.getDefaultState()));

    minOptimizationLoadLevel = parseOptimizationLevel(properties, "minOptimizationLoadLevel",
      minOptimizationLoadLevel);
    optimizeExperienceOrbs = parseConfigValue(properties, "optimizeExperienceOrbs",
      optimizeExperienceOrbs);
    experienceOrbsClusterRange = parseConfigValue(properties, "experienceOrbsClusterRange",
      experienceOrbsClusterRange);
    movePositionToLastDrop = parseConfigValue(properties, "movePositionToLastDrop",
      movePositionToLastDrop);
    removeStaleExperienceOrbs = parseConfigValue(properties, "removeStaleExperienceOrbs",
      removeStaleExperienceOrbs);
    staleExperienceOrbAgeTicks = parseConfigValue(properties, "staleExperienceOrbAgeTicks",
      staleExperienceOrbAgeTicks);

    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodified);

    log.debug(
      "ExperienceOrbs config: optimize={}, clusterRange={}, moveToLastDrop={}, removeStale={}, staleAgeTicks={}",
      optimizeExperienceOrbs,
      experienceOrbsClusterRange,
      movePositionToLastDrop,
      removeStaleExperienceOrbs,
      staleExperienceOrbAgeTicks);
  }
}
