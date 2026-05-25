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

package de.markusbordihn.adaptiveperformancetweaks.feature.gamerules;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.compat.ModConflictDetector;
import de.markusbordihn.adaptiveperformancetweaks.core.config.Config;
import de.markusbordihn.adaptiveperformancetweaks.core.config.CoreConfig;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import java.io.File;
import java.util.Properties;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class GameRulesConfig extends Config {

  public static final String CONFIG_FILE_NAME = "gamerules.cfg";
  private static final String CONFIG_FILE_HEADER =
      """
       GameRules Feature Configuration

       Controls which game rules are automatically adjusted under high server load.
       Integer values define the min/max boundaries used during optimization.
      """;

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public static boolean randomTickSpeedEnabled = true;
  public static int randomTickSpeed = 3;

  public static boolean entityCrammingEnabled = true;
  public static int maxEntityCramming = 24;
  public static int minEntityCramming = 8;
  public static int minEntityCrammingMineColonies = 16;

  public static boolean blockExplodesEnabled = true;
  public static boolean elytraMovementCheckEnabled = true;
  public static boolean insomniaEnabled = true;
  public static boolean mobExplodesEnabled = true;
  public static boolean patrolSpawningEnabled = true;
  public static boolean raidsEnabled = true;
  public static boolean traderSpawningEnabled = true;
  public static boolean tntExplodesEnabled = true;
  public static boolean vinesSpreadEnabled = true;
  public static boolean wardenSpawningEnabled = true;

  private GameRulesConfig() {}

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodified = new Properties();
    unmodified.putAll(properties);

    CoreConfig.setFeatureEnabled(
        FeatureToggle.GAMERULES,
        ModConflictDetector.resolveFeatureState(
            FeatureToggle.GAMERULES,
            parseConfigValue(properties, "enabled", FeatureToggle.GAMERULES.getDefaultState())));

    randomTickSpeedEnabled =
        parseBoolean(properties, "randomTickSpeedEnabled", randomTickSpeedEnabled);
    randomTickSpeed = parseInt(properties, "randomTickSpeed", randomTickSpeed);

    entityCrammingEnabled =
        parseBoolean(properties, "entityCrammingEnabled", entityCrammingEnabled);
    maxEntityCramming = parseInt(properties, "maxEntityCramming", maxEntityCramming);
    minEntityCramming = parseInt(properties, "minEntityCramming", minEntityCramming);
    minEntityCrammingMineColonies =
        parseInt(properties, "minEntityCrammingMineColonies", minEntityCrammingMineColonies);

    blockExplodesEnabled = parseBoolean(properties, "blockExplodesEnabled", blockExplodesEnabled);
    elytraMovementCheckEnabled =
        parseBoolean(properties, "elytraMovementCheckEnabled", elytraMovementCheckEnabled);
    insomniaEnabled = parseBoolean(properties, "insomniaEnabled", insomniaEnabled);
    mobExplodesEnabled = parseBoolean(properties, "mobExplodesEnabled", mobExplodesEnabled);
    patrolSpawningEnabled =
        parseBoolean(properties, "patrolSpawningEnabled", patrolSpawningEnabled);
    raidsEnabled = parseBoolean(properties, "raidsEnabled", raidsEnabled);
    traderSpawningEnabled =
        parseBoolean(properties, "traderSpawningEnabled", traderSpawningEnabled);
    tntExplodesEnabled = parseBoolean(properties, "tntExplodesEnabled", tntExplodesEnabled);
    vinesSpreadEnabled = parseBoolean(properties, "vinesSpreadEnabled", vinesSpreadEnabled);
    wardenSpawningEnabled =
        parseBoolean(properties, "wardenSpawningEnabled", wardenSpawningEnabled);

    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodified);

    log.debug(
        "GameRules config loaded: randomTickSpeed={} (max {}), entityCramming={} ({}-{})",
        randomTickSpeedEnabled,
        randomTickSpeed,
        entityCrammingEnabled,
        minEntityCramming,
        maxEntityCramming);
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
