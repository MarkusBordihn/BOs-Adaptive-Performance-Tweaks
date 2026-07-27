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

import de.markusbordihn.adaptiveperformancetweaks.core.config.Config;
import de.markusbordihn.adaptiveperformancetweaks.core.config.CoreConfig;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import java.io.File;
import java.util.Properties;

public final class ViewDistanceConfig extends Config {

  public static final String CONFIG_FILE_NAME = "view_distance.cfg";
  private static final String CONFIG_FILE_HEADER =
    """
       View Distance Feature Configuration
      
       Dynamically adjusts chunk view distance for all players based on server load.
       Values are chunk counts. Min: 4, Max: 32.
       This is a safety valve - only enable if view distance causes measurable lag.
      
       Steady-state load reduction only engages at minOptimizationLoadLevel (default MEDIUM) and
       above; below that view distance stays at the server default. Warmup temporarily lowers
       view distance on login/teleport and during fast movement at any load. Recovery is
       throttled (hysteresis) to avoid client-side chunk reloads from frequent view distance
       changes.
       recoveryMinDelayTicks acts as a hard floor for both the normal and the fast recovery
       path: lower recoveryDelayTicks/recoveryFastDelayTicks values have no effect below it.
       The movement warmup is speed based and measured in horizontal blocks per second.
       Vertical movement is ignored, since falling does not move the chunk frontier.
       Reference speeds: walking 4.3, sprinting 5.6, horse 5-15, elytra 20-30, boat on ice 40.
         movementSpeedBlocksPerSecond     -> reduction of 1 (exploration)
         movementFastSpeedBlocksPerSecond -> movementReductionMax (fast transit)
      """;

  public static ServerLoadLevel minOptimizationLoadLevel = ServerLoadLevel.MEDIUM;
  public static int viewDistanceVeryLow = 12;
  public static int viewDistanceLow = 10;
  public static int viewDistanceNormal = 8;
  public static int viewDistanceMedium = 6;
  public static int viewDistanceHigh = 5;
  public static int viewDistanceVeryHigh = 4;

  public static int viewDistanceMin = 4;
  public static int viewDistanceMax = 32;

  public static boolean loginWarmupEnabled = true;
  public static int loginWarmupTicks = 60;
  public static boolean movementWarmupEnabled = true;
  public static int movementSpeedBlocksPerSecond = 5;
  public static int movementFastSpeedBlocksPerSecond = 20;
  public static int movementReductionMax = 2;
  public static int evaluationIntervalTicks = 20;
  public static int recoveryMinDelayTicks = 200;
  public static int recoveryDelayTicks = 200;
  public static int recoveryStepTicks = 100;
  public static int recoveryFastDelayTicks = 40;
  public static int recoveryFastStepTicks = 20;

  private ViewDistanceConfig() {
  }

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodified = new Properties();
    unmodified.putAll(properties);

    CoreConfig.applyFeatureState(
      FeatureToggle.ADAPTIVE_VIEW_DISTANCE,
      parseConfigValue(properties, "enabled",
        FeatureToggle.ADAPTIVE_VIEW_DISTANCE.getDefaultState()));

    minOptimizationLoadLevel = parseOptimizationLevel(properties, "minOptimizationLoadLevel",
      minOptimizationLoadLevel);
    viewDistanceVeryLow = parseConfigValue(properties, "viewDistanceVeryLow", viewDistanceVeryLow);
    viewDistanceLow = parseConfigValue(properties, "viewDistanceLow", viewDistanceLow);
    viewDistanceNormal = parseConfigValue(properties, "viewDistanceNormal", viewDistanceNormal);
    viewDistanceMedium = parseConfigValue(properties, "viewDistanceMedium", viewDistanceMedium);
    viewDistanceHigh = parseConfigValue(properties, "viewDistanceHigh", viewDistanceHigh);
    viewDistanceVeryHigh = parseConfigValue(properties, "viewDistanceVeryHigh",
      viewDistanceVeryHigh);
    viewDistanceMin = parseConfigValue(properties, "viewDistanceMin", viewDistanceMin);
    viewDistanceMax = parseConfigValue(properties, "viewDistanceMax", viewDistanceMax);

    loginWarmupEnabled = parseConfigValue(properties, "loginWarmupEnabled", loginWarmupEnabled);
    loginWarmupTicks = Math.max(0,
      parseConfigValue(properties, "loginWarmupTicks", loginWarmupTicks));
    movementWarmupEnabled = parseConfigValue(properties, "movementWarmupEnabled",
      movementWarmupEnabled);
    movementSpeedBlocksPerSecond = Math.max(1, parseConfigValue(properties,
      "movementSpeedBlocksPerSecond", movementSpeedBlocksPerSecond));
    movementFastSpeedBlocksPerSecond = Math.max(movementSpeedBlocksPerSecond,
      parseConfigValue(properties, "movementFastSpeedBlocksPerSecond",
        movementFastSpeedBlocksPerSecond));
    movementReductionMax = Math.max(0,
      parseConfigValue(properties, "movementReductionMax", movementReductionMax));
    evaluationIntervalTicks = Math.max(1,
      parseConfigValue(properties, "evaluationIntervalTicks", evaluationIntervalTicks));
    recoveryMinDelayTicks = Math.max(0,
      parseConfigValue(properties, "recoveryMinDelayTicks", recoveryMinDelayTicks));
    recoveryDelayTicks = Math.max(0,
      parseConfigValue(properties, "recoveryDelayTicks", recoveryDelayTicks));
    recoveryStepTicks = Math.max(1,
      parseConfigValue(properties, "recoveryStepTicks", recoveryStepTicks));
    recoveryFastDelayTicks = Math.max(0,
      parseConfigValue(properties, "recoveryFastDelayTicks", recoveryFastDelayTicks));
    recoveryFastStepTicks = Math.max(1,
      parseConfigValue(properties, "recoveryFastStepTicks", recoveryFastStepTicks));
    properties.remove("movementWarmupMinimumLoadLevel");
    properties.remove("movementDistanceThresholdBlocks");

    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodified);
    log.debug(
      "View distance per load: VERY_LOW={} LOW={} NORMAL={} MEDIUM={} HIGH={} VERY_HIGH={} | minOptLoad={} loginWarmup={} ({}t) movementWarmup={} speed={}/{}b/s reductionMax={} evalInterval={} recovery(minDelay={} delay={} step={}) fastRecovery(delay={} step={})",
      viewDistanceVeryLow, viewDistanceLow, viewDistanceNormal,
      viewDistanceMedium, viewDistanceHigh, viewDistanceVeryHigh, minOptimizationLoadLevel,
      loginWarmupEnabled, loginWarmupTicks, movementWarmupEnabled,
      movementSpeedBlocksPerSecond, movementFastSpeedBlocksPerSecond,
      movementReductionMax, evaluationIntervalTicks,
      recoveryMinDelayTicks, recoveryDelayTicks, recoveryStepTicks,
      recoveryFastDelayTicks, recoveryFastStepTicks);
  }
}
