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

import de.markusbordihn.adaptiveperformancetweaks.core.config.Config;
import de.markusbordihn.adaptiveperformancetweaks.core.config.CoreConfig;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import java.io.File;
import java.util.Properties;
import java.util.Set;
import java.util.TreeSet;

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

  public static boolean spawnLimitationEnabled = true;
  public static boolean naturalSpawnLimitationEnabled = true;
  public static boolean naturalSpawnPrioritizeByTimeOfDay = true;
  public static double naturalSpawnNightMonsterBonus = 0.15;
  public static double naturalSpawnNightPassivePenalty = 0.10;
  public static double naturalSpawnPassRateVeryLow = 0.90;
  public static double naturalSpawnPassRateLow = 0.85;
  public static double naturalSpawnPassRateNormal = 0.80;
  public static double naturalSpawnPassRateMedium = 0.75;
  public static double naturalSpawnPassRateHigh = 0.65;
  public static double naturalSpawnPassRateVeryHigh = 0.55;
  public static int spawnLimitationMaxMobsPerPlayer = 40;
  public static int spawnLimitationMaxMobsPerWorld = 300;
  public static int spawnLimitationMaxMobsPerServer = 1024;
  public static int spawnLimitationMaxMobsPerChunk = -1;
  public static boolean spawnAggressiveMode = false;
  public static boolean viewAreaEnabled = true;
  public static int friendlyChunkSpawnRate = 9;
  public static boolean spawnEggBypassLimitations = true;
  public static boolean specialSpawnTypeBonusEnabled = true;
  public static Set<String> specialSpawnBonusTypes =
    new TreeSet<>(Set.of("chunk_generation", "event", "patrol", "reinforcement", "structure"));
  public static int specialSpawnBonusPerPlayer = 0;
  public static int specialSpawnBonusPerChunk = 4;
  public static int specialSpawnBonusPerWorld = 16;
  public static int specialSpawnBonusPerServer = 32;
  public static ServerLoadLevel specialSpawnBonusMaxLoadLevel = ServerLoadLevel.MEDIUM;
  public static boolean entityChunkCleanupEnabled = true;
  public static int entityChunkCleanupPerTypeLimit = 64;
  public static int entityChunkCleanupIntervalTicks = 2400;
  public static boolean presetReloadOnDatapackReload = true;

  private SpawnConfig() {
  }

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodified = new Properties();
    unmodified.putAll(properties);

    CoreConfig.applyFeatureState(
      FeatureToggle.SPAWN,
      parseConfigValue(properties, "enabled", FeatureToggle.SPAWN.getDefaultState()));

    spawnLimitationEnabled = parseConfigValue(properties, "spawnLimitationEnabled",
      spawnLimitationEnabled);
    naturalSpawnLimitationEnabled = parseConfigValue(properties, "naturalSpawnLimitationEnabled",
      naturalSpawnLimitationEnabled);
    naturalSpawnPrioritizeByTimeOfDay = parseConfigValue(properties,
      "naturalSpawnPrioritizeByTimeOfDay",
      naturalSpawnPrioritizeByTimeOfDay);
    naturalSpawnNightMonsterBonus = parseConfigValue(properties, "naturalSpawnNightMonsterBonus",
      naturalSpawnNightMonsterBonus);
    naturalSpawnNightPassivePenalty = parseConfigValue(properties,
      "naturalSpawnNightPassivePenalty",
      naturalSpawnNightPassivePenalty);
    naturalSpawnPassRateVeryLow = parseConfigValue(properties, "naturalSpawnPassRateVeryLow",
      naturalSpawnPassRateVeryLow);
    naturalSpawnPassRateLow = parseConfigValue(properties, "naturalSpawnPassRateLow",
      naturalSpawnPassRateLow);
    naturalSpawnPassRateNormal = parseConfigValue(properties, "naturalSpawnPassRateNormal",
      naturalSpawnPassRateNormal);
    naturalSpawnPassRateMedium = parseConfigValue(properties, "naturalSpawnPassRateMedium",
      naturalSpawnPassRateMedium);
    naturalSpawnPassRateHigh = parseConfigValue(properties, "naturalSpawnPassRateHigh",
      naturalSpawnPassRateHigh);
    naturalSpawnPassRateVeryHigh = parseConfigValue(properties, "naturalSpawnPassRateVeryHigh",
      naturalSpawnPassRateVeryHigh);
    spawnLimitationMaxMobsPerPlayer = parseConfigValue(properties,
      "spawnLimitationMaxMobsPerPlayer",
      spawnLimitationMaxMobsPerPlayer);
    spawnLimitationMaxMobsPerWorld = parseConfigValue(properties, "spawnLimitationMaxMobsPerWorld",
      spawnLimitationMaxMobsPerWorld);
    spawnLimitationMaxMobsPerServer = parseConfigValue(properties,
      "spawnLimitationMaxMobsPerServer",
      spawnLimitationMaxMobsPerServer);
    spawnLimitationMaxMobsPerChunk = parseConfigValue(properties, "spawnLimitationMaxMobsPerChunk",
      spawnLimitationMaxMobsPerChunk);
    spawnAggressiveMode = parseConfigValue(properties, "spawnAggressiveMode", spawnAggressiveMode);
    viewAreaEnabled = parseConfigValue(properties, "viewAreaEnabled", viewAreaEnabled);
    friendlyChunkSpawnRate = parseConfigValue(properties, "friendlyChunkSpawnRate",
      friendlyChunkSpawnRate);
    spawnEggBypassLimitations = parseConfigValue(properties, "spawnEggBypassLimitations",
      spawnEggBypassLimitations);
    specialSpawnTypeBonusEnabled = parseConfigValue(properties, "specialSpawnTypeBonusEnabled",
      specialSpawnTypeBonusEnabled);
    specialSpawnBonusTypes = parseConfigValue(properties, "specialSpawnBonusTypes",
      specialSpawnBonusTypes);
    specialSpawnBonusPerPlayer = parseConfigValue(properties, "specialSpawnBonusPerPlayer",
      specialSpawnBonusPerPlayer);
    specialSpawnBonusPerChunk = parseConfigValue(properties, "specialSpawnBonusPerChunk",
      specialSpawnBonusPerChunk);
    specialSpawnBonusPerWorld = parseConfigValue(properties, "specialSpawnBonusPerWorld",
      specialSpawnBonusPerWorld);
    specialSpawnBonusPerServer = parseConfigValue(properties, "specialSpawnBonusPerServer",
      specialSpawnBonusPerServer);
    specialSpawnBonusMaxLoadLevel = parseConfigValue(properties, "specialSpawnBonusMaxLoadLevel",
      specialSpawnBonusMaxLoadLevel);
    entityChunkCleanupEnabled = parseConfigValue(properties, "entityChunkCleanupEnabled",
      entityChunkCleanupEnabled);
    entityChunkCleanupPerTypeLimit = parseConfigValue(properties,
      "entityChunkCleanupPerTypeLimit",
      entityChunkCleanupPerTypeLimit);
    entityChunkCleanupIntervalTicks = parseConfigValue(properties,
      "entityChunkCleanupIntervalTicks",
      entityChunkCleanupIntervalTicks);
    presetReloadOnDatapackReload = parseConfigValue(properties, "presetReloadOnDatapackReload",
      presetReloadOnDatapackReload);

    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodified);

    log.debug(
      "Spawn config: spawnLimitationEnabled={}, naturalSpawnLimitationEnabled={}, passRates=[vl={}, l={}, n={}, m={}, h={}, vh={}], maxPerPlayer={}, maxPerWorld={}, maxPerServer={}, maxPerChunk={}, specialSpawnBonusEnabled={}, specialSpawnBonusTypes={}, specialSpawnBonusMaxLoadLevel={}, entityChunkCleanupEnabled={}, entityChunkCleanupPerTypeLimit={}, entityChunkCleanupIntervalTicks={}",
      spawnLimitationEnabled,
      naturalSpawnLimitationEnabled,
      naturalSpawnPassRateVeryLow, naturalSpawnPassRateLow, naturalSpawnPassRateNormal,
      naturalSpawnPassRateMedium, naturalSpawnPassRateHigh, naturalSpawnPassRateVeryHigh,
      spawnLimitationMaxMobsPerPlayer,
      spawnLimitationMaxMobsPerWorld,
      spawnLimitationMaxMobsPerServer,
      spawnLimitationMaxMobsPerChunk,
      specialSpawnTypeBonusEnabled,
      specialSpawnBonusTypes,
      specialSpawnBonusMaxLoadLevel,
      entityChunkCleanupEnabled,
      entityChunkCleanupPerTypeLimit,
      entityChunkCleanupIntervalTicks);
  }

}
