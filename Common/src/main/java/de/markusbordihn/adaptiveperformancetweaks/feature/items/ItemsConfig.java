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
import java.util.Arrays;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class ItemsConfig extends Config {

  public static final String CONFIG_FILE_NAME = "items.cfg";
  private static final String CONFIG_FILE_HEADER =
      """
       Items Feature Configuration

       Controls item-entity clustering and per-world/per-type limits.
       Use the allow/deny lists to include or exclude specific item types.
       Leave a list empty to disable it.
      """;

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public static boolean optimizeItems = true;
  public static int maxNumberOfItemsPerType = 32;
  public static int maxNumberOfItems = 128;
  public static int itemsClusterRange = 2;
  public static Set<String> itemsAllowList = new HashSet<>();
  public static Set<String> itemsDenyList = new HashSet<>();

  private ItemsConfig() {}

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodified = new Properties();
    unmodified.putAll(properties);

    CoreConfig.setFeatureEnabled(
        FeatureToggle.ITEMS,
        ModConflictDetector.resolveFeatureState(
            FeatureToggle.ITEMS,
            parseConfigValue(properties, "enabled", FeatureToggle.ITEMS.getDefaultState())));

    optimizeItems = parseBoolean(properties, "optimizeItems", optimizeItems);
    maxNumberOfItemsPerType =
        parseInt(properties, "maxNumberOfItemsPerType", maxNumberOfItemsPerType);
    maxNumberOfItems = parseInt(properties, "maxNumberOfItems", maxNumberOfItems);
    itemsClusterRange = parseInt(properties, "itemsClusterRange", itemsClusterRange);
    itemsAllowList = parseStringSet(properties, "itemsAllowList", itemsAllowList);
    itemsDenyList = parseStringSet(properties, "itemsDenyList", itemsDenyList);

    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodified);

    log.debug(
        "Items config: optimize={}, maxPerType={}, maxPerWorld={}, clusterRange={}",
        optimizeItems,
        maxNumberOfItemsPerType,
        maxNumberOfItems,
        itemsClusterRange);
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

  private static Set<String> parseStringSet(
      Properties props, String key, Set<String> defaultValue) {
    props.putIfAbsent(key, "");
    String value = props.getProperty(key, "").trim();
    if (value.isEmpty()) {
      return defaultValue;
    }

    return new HashSet<>(Arrays.asList(value.split(",")));
  }
}
