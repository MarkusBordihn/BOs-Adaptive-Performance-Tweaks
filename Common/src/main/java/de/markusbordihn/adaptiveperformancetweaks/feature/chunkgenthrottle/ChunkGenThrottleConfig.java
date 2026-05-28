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

package de.markusbordihn.adaptiveperformancetweaks.feature.chunkgenthrottle;

import de.markusbordihn.adaptiveperformancetweaks.core.config.Config;
import de.markusbordihn.adaptiveperformancetweaks.core.config.CoreConfig;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import java.io.File;
import java.util.Properties;

public final class ChunkGenThrottleConfig extends Config {

  public static final String CONFIG_FILE_NAME = "chunk_gen_throttle.cfg";
  private static final String CONFIG_FILE_HEADER =
    """
       Chunk Generation Throttle Feature Configuration
      
       Reduces the frequency of chunk generation processing ticks under server load.
       Only activates at MEDIUM+ server load - no effect at normal load.
      
       Divisors control how many ticks are skipped between processing runs:
         2 = process every 2nd tick (50% reduction)
         4 = process every 4th tick (75% reduction)
         8 = process every 8th tick (87.5% reduction)
      """;

  public static int chunkGenThrottleMediumDivisor = 2;
  public static int chunkGenThrottleHighDivisor = 4;
  public static int chunkGenThrottleVeryHighDivisor = 8;

  private ChunkGenThrottleConfig() {
  }

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodified = new Properties();
    unmodified.putAll(properties);

    CoreConfig.applyFeatureState(
      FeatureToggle.CHUNK_GEN_THROTTLE,
      parseConfigValue(properties, "enabled", FeatureToggle.CHUNK_GEN_THROTTLE.getDefaultState()));

    chunkGenThrottleMediumDivisor =
      parseConfigValue(properties, "chunkGenThrottleMediumDivisor", chunkGenThrottleMediumDivisor);
    chunkGenThrottleHighDivisor =
      parseConfigValue(properties, "chunkGenThrottleHighDivisor", chunkGenThrottleHighDivisor);
    chunkGenThrottleVeryHighDivisor =
      parseConfigValue(properties, "chunkGenThrottleVeryHighDivisor",
        chunkGenThrottleVeryHighDivisor);

    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodified);
    log.debug(
      "Chunk gen throttle: divisors MEDIUM={} HIGH={} VERY_HIGH={}",
      chunkGenThrottleMediumDivisor,
      chunkGenThrottleHighDivisor,
      chunkGenThrottleVeryHighDivisor);
  }
}
