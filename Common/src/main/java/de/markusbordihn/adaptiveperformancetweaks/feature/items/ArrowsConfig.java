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
import java.io.File;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;

public final class ArrowsConfig extends Config {

  public static final String CONFIG_FILE_NAME = "arrows.cfg";
  private static final String CONFIG_FILE_HEADER =
    """
       Arrows Feature Configuration
      
       Limits stuck arrows (arrows that have already hit a block or entity) per world and per chunk.
       In-flight arrows and named arrows/tridents are never removed.
       Use the allow/deny lists to include or exclude specific arrow entity types.
       Leave a list empty to disable it.
      """;

  public static int maxNumberOfArrowsPerWorld = 512;
  public static int maxNumberOfArrowsPerChunk = 32;
  public static Set<String> arrowsAllowList = new HashSet<>();
  public static Set<String> arrowsDenyList = new HashSet<>();

  private ArrowsConfig() {
  }

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodified = new Properties();
    unmodified.putAll(properties);

    CoreConfig.applyFeatureState(
      FeatureToggle.ARROWS,
      parseConfigValue(properties, "enabled", FeatureToggle.ARROWS.getDefaultState()));

    maxNumberOfArrowsPerWorld = parseConfigValue(properties, "maxNumberOfArrowsPerWorld",
      maxNumberOfArrowsPerWorld);
    maxNumberOfArrowsPerChunk = parseConfigValue(properties, "maxNumberOfArrowsPerChunk",
      maxNumberOfArrowsPerChunk);
    arrowsAllowList = parseConfigValue(properties, "arrowsAllowList", new HashSet<>());
    arrowsDenyList = parseConfigValue(properties, "arrowsDenyList", new HashSet<>());

    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodified);

    log.debug(
      "Arrows config: maxPerWorld={}, maxPerChunk={}",
      maxNumberOfArrowsPerWorld,
      maxNumberOfArrowsPerChunk);
  }
}
