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

public final class ViewDistanceConfig extends Config {

  public static final String CONFIG_FILE_NAME = "view_distance.cfg";
  private static final String CONFIG_FILE_HEADER =
    """
       View Distance Feature Configuration
      
       Dynamically adjusts chunk view distance for all players based on server load.
       Values are chunk counts. Min: 4, Max: 32.
       This is a safety valve — only enable if view distance causes measurable lag.
      """;

  public static int viewDistanceVeryLow = 12;
  public static int viewDistanceLow = 10;
  public static int viewDistanceNormal = 8;
  public static int viewDistanceMedium = 6;
  public static int viewDistanceHigh = 5;
  public static int viewDistanceVeryHigh = 4;

  public static int viewDistanceMin = 4;
  public static int viewDistanceMax = 32;

  private ViewDistanceConfig() {
  }

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodified = new Properties();
    unmodified.putAll(properties);

    CoreConfig.setFeatureEnabled(
      FeatureToggle.ADAPTIVE_VIEW_DISTANCE,
      ModConflictDetector.resolveFeatureState(
        FeatureToggle.ADAPTIVE_VIEW_DISTANCE,
        parseConfigValue(
          properties,
          "enabled",
          FeatureToggle.ADAPTIVE_VIEW_DISTANCE.getDefaultState())));

    viewDistanceVeryLow = parseInt(properties, "viewDistanceVeryLow", viewDistanceVeryLow);
    viewDistanceLow = parseInt(properties, "viewDistanceLow", viewDistanceLow);
    viewDistanceNormal = parseInt(properties, "viewDistanceNormal", viewDistanceNormal);
    viewDistanceMedium = parseInt(properties, "viewDistanceMedium", viewDistanceMedium);
    viewDistanceHigh = parseInt(properties, "viewDistanceHigh", viewDistanceHigh);
    viewDistanceVeryHigh = parseInt(properties, "viewDistanceVeryHigh", viewDistanceVeryHigh);
    viewDistanceMin = parseInt(properties, "viewDistanceMin", viewDistanceMin);
    viewDistanceMax = parseInt(properties, "viewDistanceMax", viewDistanceMax);

    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodified);
    log.debug(
      "View distance per load: VERY_LOW={} LOW={} NORMAL={} MEDIUM={} HIGH={} VERY_HIGH={}",
      viewDistanceVeryLow, viewDistanceLow, viewDistanceNormal,
      viewDistanceMedium, viewDistanceHigh, viewDistanceVeryHigh);
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
