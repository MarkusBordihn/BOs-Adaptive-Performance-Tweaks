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

package de.markusbordihn.adaptiveperformancetweaks.feature.aithrottle;

import de.markusbordihn.adaptiveperformancetweaks.core.compat.ModConflictDetector;
import de.markusbordihn.adaptiveperformancetweaks.core.config.Config;
import de.markusbordihn.adaptiveperformancetweaks.core.config.CoreConfig;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import java.io.File;
import java.util.Properties;

public final class AiThrottleConfig extends Config {

  public static final String CONFIG_FILE_NAME = "ai_throttle.cfg";
  private static final String CONFIG_FILE_HEADER =
    """
       AI Throttling Feature Configuration
      
       Reduces AI goal evaluation frequency for mobs that are far from all players.
       Throttling only activates at MEDIUM+ server load - zero effect at normal load.
      
       aiThrottleNearbyRadius: mobs within this radius (in blocks) are NEVER throttled.
       Divisors control how many ticks are skipped: 2 = every 2nd tick, 4 = every 4th tick.
      """;

  public static int aiThrottleNearbyRadius = 24;
  public static int aiThrottleMediumDivisor = 2;
  public static int aiThrottleHighDivisor = 4;
  public static int aiThrottleVeryHighDivisor = 8;

  private AiThrottleConfig() {
  }

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodified = new Properties();
    unmodified.putAll(properties);

    CoreConfig.setFeatureEnabled(
      FeatureToggle.AI_THROTTLING,
      ModConflictDetector.resolveFeatureState(
        FeatureToggle.AI_THROTTLING,
        parseConfigValue(
          properties, "enabled", FeatureToggle.AI_THROTTLING.getDefaultState())));

    aiThrottleNearbyRadius = parseInt(properties, "aiThrottleNearbyRadius", aiThrottleNearbyRadius);
    aiThrottleMediumDivisor = parseInt(properties, "aiThrottleMediumDivisor",
      aiThrottleMediumDivisor);
    aiThrottleHighDivisor = parseInt(properties, "aiThrottleHighDivisor", aiThrottleHighDivisor);
    aiThrottleVeryHighDivisor = parseInt(properties, "aiThrottleVeryHighDivisor",
      aiThrottleVeryHighDivisor);

    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodified);
    log.debug("AI throttling: nearbyRadius={} divisors: MEDIUM={} HIGH={} VERY_HIGH={}",
      aiThrottleNearbyRadius,
      aiThrottleMediumDivisor, aiThrottleHighDivisor, aiThrottleVeryHighDivisor);
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
