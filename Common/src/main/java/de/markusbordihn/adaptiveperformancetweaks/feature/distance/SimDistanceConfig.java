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

package de.markusbordihn.adaptiveperformancetweaks.feature.distance;

import de.markusbordihn.adaptiveperformancetweaks.core.compat.ModConflictDetector;
import de.markusbordihn.adaptiveperformancetweaks.core.config.Config;
import de.markusbordihn.adaptiveperformancetweaks.core.config.CoreConfig;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import java.io.File;
import java.util.Properties;

public final class SimDistanceConfig extends Config {

  public static final String CONFIG_FILE_NAME = "sim_distance.cfg";
  private static final String CONFIG_FILE_HEADER =
    """
       Simulation Distance Feature Configuration
      
       Dynamically adjusts simulation distance for all players based on server load.
       Values are chunk counts. Min: 2, Max: 32.
       Higher load level = lower distance to reduce server tick pressure.
      """;

  public static int simDistanceVeryLow = 12;
  public static int simDistanceLow = 10;
  public static int simDistanceNormal = 8;
  public static int simDistanceMedium = 6;
  public static int simDistanceHigh = 4;
  public static int simDistanceVeryHigh = 2;

  public static int simDistanceMin = 2;
  public static int simDistanceMax = 12;

  private SimDistanceConfig() {
  }

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodified = new Properties();
    unmodified.putAll(properties);

    CoreConfig.setFeatureEnabled(
      FeatureToggle.ADAPTIVE_SIM_DISTANCE,
      ModConflictDetector.resolveFeatureState(
        FeatureToggle.ADAPTIVE_SIM_DISTANCE,
        parseConfigValue(
          properties,
          "enabled",
          FeatureToggle.ADAPTIVE_SIM_DISTANCE.getDefaultState())));

    simDistanceVeryLow = parseInt(properties, "simDistanceVeryLow", simDistanceVeryLow);
    simDistanceLow = parseInt(properties, "simDistanceLow", simDistanceLow);
    simDistanceNormal = parseInt(properties, "simDistanceNormal", simDistanceNormal);
    simDistanceMedium = parseInt(properties, "simDistanceMedium", simDistanceMedium);
    simDistanceHigh = parseInt(properties, "simDistanceHigh", simDistanceHigh);
    simDistanceVeryHigh = parseInt(properties, "simDistanceVeryHigh", simDistanceVeryHigh);
    simDistanceMin = parseInt(properties, "simDistanceMin", simDistanceMin);
    simDistanceMax = parseInt(properties, "simDistanceMax", simDistanceMax);

    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodified);
    log.debug(
      "Simulation distance per load: VERY_LOW={} LOW={} NORMAL={} MEDIUM={} HIGH={} VERY_HIGH={}",
      simDistanceVeryLow, simDistanceLow, simDistanceNormal,
      simDistanceMedium, simDistanceHigh, simDistanceVeryHigh);
  }

  private static int parseInt(Properties props, String key, int defaultValue) {
    props.putIfAbsent(key, String.valueOf(defaultValue));
    try {
      return Integer.parseInt(props.getProperty(key));
    } catch (NumberFormatException exception) {
      log.warn("Invalid integer for '{}', using default {}", key,
        defaultValue);
      return defaultValue;
    }
  }
}
