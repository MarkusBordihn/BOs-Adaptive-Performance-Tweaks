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

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.compat.ModConflictDetector;
import de.markusbordihn.adaptiveperformancetweaks.core.config.Config;
import de.markusbordihn.adaptiveperformancetweaks.core.config.CoreConfig;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import java.io.File;
import java.util.Properties;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class ExperienceOrbsConfig extends Config {

  public static final String CONFIG_FILE_NAME = "experience_orbs.cfg";
  private static final String CONFIG_FILE_HEADER =
      """
       Experience Orbs Feature Configuration

       Controls XP orb merging to reduce entity count.
       Orbs within clusterRange blocks will be merged into a single orb.
      """;

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public static boolean optimizeExperienceOrbs = true;
  public static int experienceOrbsClusterRange = 2;

  private ExperienceOrbsConfig() {}

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodified = new Properties();
    unmodified.putAll(properties);

    CoreConfig.setFeatureEnabled(
        FeatureToggle.EXPERIENCE_ORBS,
        ModConflictDetector.resolveFeatureState(
            FeatureToggle.EXPERIENCE_ORBS,
            parseConfigValue(
                properties, "enabled", FeatureToggle.EXPERIENCE_ORBS.getDefaultState())));

    optimizeExperienceOrbs =
        parseBoolean(properties, "optimizeExperienceOrbs", optimizeExperienceOrbs);
    experienceOrbsClusterRange =
        parseInt(properties, "experienceOrbsClusterRange", experienceOrbsClusterRange);

    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodified);

    log.debug(
        "ExperienceOrbs config: optimize={}, clusterRange={}",
        optimizeExperienceOrbs,
        experienceOrbsClusterRange);
  }

  private static boolean parseBoolean(Properties props, String key, boolean defaultValue) {
    props.putIfAbsent(key, String.valueOf(defaultValue));

    return Boolean.parseBoolean(props.getProperty(key));
  }

  private static int parseInt(Properties props, String key, int defaultValue) {
    props.putIfAbsent(key, String.valueOf(defaultValue));
    try {
      return Integer.parseInt(props.getProperty(key));
    } catch (NumberFormatException exception) {
      log.warn("Invalid integer for '{}', using default {}", key, defaultValue);
      return defaultValue;
    }
  }
}
