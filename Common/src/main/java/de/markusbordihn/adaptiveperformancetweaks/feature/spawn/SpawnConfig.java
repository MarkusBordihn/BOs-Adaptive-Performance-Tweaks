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

package de.markusbordihn.adaptiveperformancetweaks.feature.spawn;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.compat.ModConflictDetector;
import de.markusbordihn.adaptiveperformancetweaks.core.config.Config;
import de.markusbordihn.adaptiveperformancetweaks.core.config.CoreConfig;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import java.io.File;
import java.util.Properties;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class SpawnConfig extends Config {

  public static final String CONFIG_FILE_NAME = "spawn.cfg";
  private static final String CONFIG_FILE_HEADER =
    """
       Spawn Feature Configuration
      
       Controls natural mob spawning limits and datapack-driven spawn presets.
       Per-entity-type limits are configured via JSON presets in:
         - data/<namespace>/aptweaks/spawn_presets/*.json  (datapack)
         - config/adaptive_performance_tweaks/spawn_presets/*.json  (server-local)
      
       Global limits below serve as fallback when no preset defines a value.
       Default values are tuned for ~4 players. Scale per-player and per-world limits
       proportionally for larger servers.
      """;

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public static boolean spawnLimitationEnabled = true;
  public static boolean naturalSpawnLimitationEnabled = true;
  public static boolean naturalSpawnPrioritizeByTimeOfDay = true;
  public static double naturalSpawnNightMonsterBonus = 0.15;
  public static double naturalSpawnNightPassivePenalty = 0.10;
  public static double naturalSpawnPassRateVeryLow = 0.95;
  public static double naturalSpawnPassRateLow = 0.90;
  public static double naturalSpawnPassRateNormal = 0.85;
  public static double naturalSpawnPassRateMedium = 0.80;
  public static double naturalSpawnPassRateHigh = 0.70;
  public static double naturalSpawnPassRateVeryHigh = 0.60;
  public static int spawnLimitationMaxMobsPerPlayer = 40;
  public static int spawnLimitationMaxMobsPerWorld = 300;
  public static int spawnLimitationMaxMobsPerServer = 1024;
  public static int spawnLimitationMaxMobsPerChunk = -1;
  public static boolean spawnAggressiveMode = false;
  public static boolean viewAreaEnabled = true;
  public static int friendlyChunkSpawnRate = 9;
  public static boolean spawnEggBypassLimitations = true;
  public static boolean presetReloadOnDatapackReload = true;

  private SpawnConfig() {
  }

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodified = new Properties();
    unmodified.putAll(properties);

    CoreConfig.setFeatureEnabled(
      FeatureToggle.SPAWN,
      ModConflictDetector.resolveFeatureState(
        FeatureToggle.SPAWN,
        parseConfigValue(properties, "enabled", FeatureToggle.SPAWN.getDefaultState())));

    spawnLimitationEnabled = parseBoolean(properties, "spawnLimitationEnabled",
      spawnLimitationEnabled);
    naturalSpawnLimitationEnabled = parseBoolean(properties, "naturalSpawnLimitationEnabled",
      naturalSpawnLimitationEnabled);
    naturalSpawnPrioritizeByTimeOfDay = parseBoolean(properties,
      "naturalSpawnPrioritizeByTimeOfDay",
      naturalSpawnPrioritizeByTimeOfDay);
    naturalSpawnNightMonsterBonus = parseDouble(properties, "naturalSpawnNightMonsterBonus",
      naturalSpawnNightMonsterBonus);
    naturalSpawnNightPassivePenalty = parseDouble(properties, "naturalSpawnNightPassivePenalty",
      naturalSpawnNightPassivePenalty);
    naturalSpawnPassRateVeryLow = parseDouble(properties, "naturalSpawnPassRateVeryLow",
      naturalSpawnPassRateVeryLow);
    naturalSpawnPassRateLow = parseDouble(properties, "naturalSpawnPassRateLow",
      naturalSpawnPassRateLow);
    naturalSpawnPassRateNormal = parseDouble(properties, "naturalSpawnPassRateNormal",
      naturalSpawnPassRateNormal);
    naturalSpawnPassRateMedium = parseDouble(properties, "naturalSpawnPassRateMedium",
      naturalSpawnPassRateMedium);
    naturalSpawnPassRateHigh = parseDouble(properties, "naturalSpawnPassRateHigh",
      naturalSpawnPassRateHigh);
    naturalSpawnPassRateVeryHigh = parseDouble(properties, "naturalSpawnPassRateVeryHigh",
      naturalSpawnPassRateVeryHigh);
    spawnLimitationMaxMobsPerPlayer = parseInt(properties, "spawnLimitationMaxMobsPerPlayer",
      spawnLimitationMaxMobsPerPlayer);
    spawnLimitationMaxMobsPerWorld = parseInt(properties, "spawnLimitationMaxMobsPerWorld",
      spawnLimitationMaxMobsPerWorld);
    spawnLimitationMaxMobsPerServer = parseInt(properties, "spawnLimitationMaxMobsPerServer",
      spawnLimitationMaxMobsPerServer);
    spawnLimitationMaxMobsPerChunk = parseInt(properties, "spawnLimitationMaxMobsPerChunk",
      spawnLimitationMaxMobsPerChunk);
    spawnAggressiveMode = parseBoolean(properties, "spawnAggressiveMode", spawnAggressiveMode);
    viewAreaEnabled = parseBoolean(properties, "viewAreaEnabled", viewAreaEnabled);
    friendlyChunkSpawnRate = parseInt(properties, "friendlyChunkSpawnRate", friendlyChunkSpawnRate);
    spawnEggBypassLimitations = parseBoolean(properties, "spawnEggBypassLimitations",
      spawnEggBypassLimitations);
    presetReloadOnDatapackReload = parseBoolean(properties, "presetReloadOnDatapackReload",
      presetReloadOnDatapackReload);

    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodified);

    log.debug(
      "Spawn config: spawnLimitationEnabled={}, naturalSpawnLimitationEnabled={}, passRates=[vl={}, l={}, n={}, m={}, h={}, vh={}], maxPerPlayer={}, maxPerWorld={}, maxPerServer={}, maxPerChunk={}",
      spawnLimitationEnabled,
      naturalSpawnLimitationEnabled,
      naturalSpawnPassRateVeryLow, naturalSpawnPassRateLow, naturalSpawnPassRateNormal,
      naturalSpawnPassRateMedium, naturalSpawnPassRateHigh, naturalSpawnPassRateVeryHigh,
      spawnLimitationMaxMobsPerPlayer,
      spawnLimitationMaxMobsPerWorld,
      spawnLimitationMaxMobsPerServer,
      spawnLimitationMaxMobsPerChunk);
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
      log.warn("Invalid integer for '{}', using default {}", key,
        defaultValue);
      return defaultValue;
    }
  }

  private static double parseDouble(Properties props, String key, double defaultValue) {
    props.putIfAbsent(key, String.valueOf(defaultValue));
    try {
      return Double.parseDouble(props.getProperty(key));
    } catch (NumberFormatException exception) {
      log.warn("Invalid double for '{}', using default {}", key,
        defaultValue);
      return defaultValue;
    }
  }
}
