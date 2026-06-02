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
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;

public final class ItemsConfig extends Config {

  public static final String CONFIG_FILE_NAME = "items.cfg";
  private static final String CONFIG_FILE_HEADER =
    """
       Items Feature Configuration
      
       Controls item-entity clustering and per-world/per-type limits.
       Use the allow/deny lists to include or exclude specific item types.
       Leave a list empty to disable it.
      """;

  public static ServerLoadLevel minOptimizationLoadLevel = ServerLoadLevel.NORMAL;
  public static boolean optimizeItems = true;
  public static int maxNumberOfItemsPerType = 64;
  public static int maxNumberOfItems = 128;
  public static int itemsClusterRange = 2;
  public static int maxStackSize = 64;
  public static boolean movePositionToLastDrop = false;
  public static Set<String> itemsAllowList = new HashSet<>();
  public static Set<String> itemsDenyList = new HashSet<>(Set.of(
    "minecraft:ancient_debris",
    "minecraft:diamond",
    "minecraft:diamond_block",
    "minecraft:enchanted_golden_apple",
    "minecraft:elytra",
    "minecraft:nether_star",
    "minecraft:netherite_ingot",
    "minecraft:netherite_scrap",
    "minecraft:totem_of_undying"
  ));

  private ItemsConfig() {
  }

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodified = new Properties();
    unmodified.putAll(properties);

    CoreConfig.applyFeatureState(
      FeatureToggle.ITEMS,
      parseConfigValue(properties, "enabled", FeatureToggle.ITEMS.getDefaultState()));

    minOptimizationLoadLevel = parseOptimizationLevel(properties, "minOptimizationLoadLevel",
      minOptimizationLoadLevel);
    optimizeItems = parseConfigValue(properties, "optimizeItems", optimizeItems);
    maxNumberOfItemsPerType = parseConfigValue(properties, "maxNumberOfItemsPerType",
      maxNumberOfItemsPerType);
    maxNumberOfItems = parseConfigValue(properties, "maxNumberOfItems", maxNumberOfItems);
    itemsClusterRange = parseConfigValue(properties, "itemsClusterRange", itemsClusterRange);
    maxStackSize = parseConfigValue(properties, "maxStackSize", maxStackSize);
    movePositionToLastDrop = parseConfigValue(properties, "movePositionToLastDrop",
      movePositionToLastDrop);
    itemsAllowList = parseConfigValue(properties, "itemsAllowList", itemsAllowList);
    itemsDenyList = parseConfigValue(properties, "itemsDenyList", itemsDenyList);

    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodified);

    log.debug(
      "Items config: optimize={}, maxPerType={}, maxPerWorld={}, clusterRange={}, maxStackSize={}, moveToLastDrop={}",
      optimizeItems,
      maxNumberOfItemsPerType,
      maxNumberOfItems,
      itemsClusterRange,
      maxStackSize,
      movePositionToLastDrop);
  }
}
