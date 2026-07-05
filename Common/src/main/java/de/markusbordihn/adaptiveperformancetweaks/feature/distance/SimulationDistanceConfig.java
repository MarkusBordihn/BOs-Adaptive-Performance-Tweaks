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
import de.markusbordihn.adaptiveperformancetweaks.core.player.PlayerPositionManager;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import java.io.File;
import java.util.Properties;

public final class SimulationDistanceConfig extends Config {

  public static final String CONFIG_FILE_NAME = "sim_distance.cfg";
  private static final String CONFIG_FILE_HEADER =
    """
       Simulation Distance Feature Configuration
      
       Dynamically adjusts simulation distance for all players based on server load.
       Values are chunk counts. Min: 2, Max: 32.
       Higher load level = lower distance to reduce server tick pressure.
       Optional movement throttling adds temporary reductions during login, teleport and
       heavy exploration independent of load.
       Note: movementThrottleLoginTicks and movementThrottleDistanceThresholdBlocks are also
       used by the GameRules feature for its random-tick warmup timing.
       movementThrottleRecoveryMinDelayTicks acts as a hard floor: recovery never starts
       earlier, even if movementThrottleRecoveryDelayTicks is set lower.
      """;

  public static ServerLoadLevel minOptimizationLoadLevel = ServerLoadLevel.VERY_LOW;
  public static int simDistanceVeryLow = 12;
  public static int simDistanceLow = 10;
  public static int simDistanceNormal = 8;
  public static int simDistanceMedium = 6;
  public static int simDistanceHigh = 4;
  public static int simDistanceVeryHigh = 2;

  public static int simDistanceMin = 2;
  public static int simDistanceMax = 12;
  public static boolean loginWarmupEnabled = true;
  public static boolean movementThrottleEnabled = true;
  public static int movementThrottleWindowSamples = 3;
  public static int movementThrottleWindowSamplesMax = 5;
  public static int movementThrottleSampleTicks = 20;
  public static int movementThrottleDistanceThresholdBlocks = 24;
  public static int movementThrottleRecoveryMinDelayTicks = 200;
  public static int movementThrottleRecoveryDelayTicks = 140;
  public static int movementThrottleRecoveryStepTicks = 40;
  public static int movementThrottleLoginTicks = 60;
  public static boolean movementThrottleRecoverOnlyWhenStable = true;
  public static int movementThrottleMinReduction = 1;
  public static int movementThrottleMaxReduction = 2;

  private SimulationDistanceConfig() {
  }

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodified = new Properties();
    unmodified.putAll(properties);

    CoreConfig.applyFeatureState(
      FeatureToggle.ADAPTIVE_SIMULATION_DISTANCE,
      parseConfigValue(properties, "enabled",
        FeatureToggle.ADAPTIVE_SIMULATION_DISTANCE.getDefaultState()));

    minOptimizationLoadLevel = parseOptimizationLevel(properties, "minOptimizationLoadLevel",
      minOptimizationLoadLevel);
    simDistanceVeryLow = parseConfigValue(properties, "simDistanceVeryLow", simDistanceVeryLow);
    simDistanceLow = parseConfigValue(properties, "simDistanceLow", simDistanceLow);
    simDistanceNormal = parseConfigValue(properties, "simDistanceNormal", simDistanceNormal);
    simDistanceMedium = parseConfigValue(properties, "simDistanceMedium", simDistanceMedium);
    simDistanceHigh = parseConfigValue(properties, "simDistanceHigh", simDistanceHigh);
    simDistanceVeryHigh = parseConfigValue(properties, "simDistanceVeryHigh", simDistanceVeryHigh);
    simDistanceMin = parseConfigValue(properties, "simDistanceMin", simDistanceMin);
    simDistanceMax = parseConfigValue(properties, "simDistanceMax", simDistanceMax);
    loginWarmupEnabled = parseConfigValue(properties, "loginWarmupEnabled", loginWarmupEnabled);
    movementThrottleEnabled = parseConfigValue(properties, "movementThrottleEnabled",
      movementThrottleEnabled);
    movementThrottleWindowSamplesMax = Math.max(1,
      parseConfigValue(properties, "movementThrottleWindowSamplesMax",
        movementThrottleWindowSamplesMax));
    movementThrottleWindowSamples = Math.max(1, Math.min(movementThrottleWindowSamplesMax,
      parseConfigValue(properties, "movementThrottleWindowSamples",
        movementThrottleWindowSamples)));
    movementThrottleSampleTicks = Math.max(1,
      parseConfigValue(properties, "movementThrottleSampleTicks", movementThrottleSampleTicks));
    movementThrottleDistanceThresholdBlocks = Math.max(1, parseConfigValue(properties,
      "movementThrottleDistanceThresholdBlocks", movementThrottleDistanceThresholdBlocks));
    movementThrottleRecoveryMinDelayTicks = Math.max(0, parseConfigValue(properties,
      "movementThrottleRecoveryMinDelayTicks", movementThrottleRecoveryMinDelayTicks));
    movementThrottleRecoveryDelayTicks = Math.max(1, parseConfigValue(properties,
      "movementThrottleRecoveryDelayTicks", movementThrottleRecoveryDelayTicks));
    movementThrottleRecoveryStepTicks = Math.max(1, parseConfigValue(properties,
      "movementThrottleRecoveryStepTicks", movementThrottleRecoveryStepTicks));
    movementThrottleLoginTicks = Math.max(0,
      parseConfigValue(properties, "movementThrottleLoginTicks",
        movementThrottleLoginTicks));
    movementThrottleRecoverOnlyWhenStable = parseConfigValue(properties,
      "movementThrottleRecoverOnlyWhenStable", movementThrottleRecoverOnlyWhenStable);
    movementThrottleMinReduction = Math.max(0, parseConfigValue(properties,
      "movementThrottleMinReduction", movementThrottleMinReduction));
    movementThrottleMaxReduction = Math.max(movementThrottleMinReduction,
      parseConfigValue(properties,
        "movementThrottleMaxReduction", movementThrottleMaxReduction));
    properties.remove("movementThrottleMinimumLoadLevel");

    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodified);
    PlayerPositionManager.configureMovementTracking(movementThrottleSampleTicks,
      movementThrottleWindowSamples);
    log.debug(
      "Simulation distance per load: VERY_LOW={} LOW={} NORMAL={} MEDIUM={} HIGH={} VERY_HIGH={} | loginWarmup={} movementThrottle={} minLoad={} samples={}/{} sampleTicks={} threshold={} recovery(minDelay={} delay={} step={}) loginTicks={} recoverOnlyWhenStable={} reduction={}..{}",
      simDistanceVeryLow, simDistanceLow, simDistanceNormal,
      simDistanceMedium, simDistanceHigh, simDistanceVeryHigh,
      loginWarmupEnabled, movementThrottleEnabled,
      movementThrottleWindowSamples, movementThrottleWindowSamplesMax,
      movementThrottleSampleTicks, movementThrottleDistanceThresholdBlocks,
      movementThrottleRecoveryMinDelayTicks, movementThrottleRecoveryDelayTicks,
      movementThrottleRecoveryStepTicks, movementThrottleLoginTicks,
      movementThrottleRecoverOnlyWhenStable, movementThrottleMinReduction,
      movementThrottleMaxReduction);
  }

}
