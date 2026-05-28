/*
 * Copyright 2024 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.adaptiveperformancetweaks.core.config;

import de.markusbordihn.adaptiveperformancetweaks.core.compat.ModConflictDetector;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureState;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import java.io.File;
import java.util.EnumMap;
import java.util.Map;
import java.util.Properties;

public class CoreConfig extends Config {

  public static final String CONFIG_FILE_NAME = "core.cfg";
  public static final String CONFIG_FILE_HEADER =
    """
       Core Configuration
      
       Server load thresholds (ms per tick, default Minecraft tick = 50 ms):
       serverLoadVeryLowThreshold  -- up to this ms/tick counts as "very low" (default: 20)
       serverLoadLowThreshold      -- up to this ms/tick counts as "low"      (default: 40)
       serverLoadNormalThreshold   -- up to this ms/tick counts as "normal"   (default: 46)
       serverLoadMediumThreshold   -- up to this ms/tick counts as "medium"   (default: 49)
       serverLoadHighThreshold     -- up to this ms/tick counts as "high"     (default: 55)
       (anything above the high threshold counts as "very high")
      
       timeBetweenUpdates  -- seconds between server load measurements (default: 5)
       logServerLoad       -- write server load changes to the log (default: true)
       logServerLevelLoadChanges -- write per-world load changes to debug log (default: false)
       serverLoadLogIntervalSeconds -- minimum interval for non-significant server load logs (default: 60)
       serverLoadLogSignificantChangeSteps -- ordinal jump treated as significant (default: 2)
       serverLoadLogTopWorldCount -- how many top worlds to include in server load summaries (default: 5)
      
       writeEntityTrackingReport -- write entity_tracking_report.json to the config folder on load/reload (default: true)
      """;

  private static final Map<FeatureToggle, Boolean> featureFlags = new EnumMap<>(
    FeatureToggle.class);
  private static final Map<FeatureToggle, ModConflictDetector.FeatureDecision> featureDecisions =
    new EnumMap<>(FeatureToggle.class);

  public static int serverLoadVeryLowThreshold = 20;
  public static int serverLoadLowThreshold = 40;
  public static int serverLoadNormalThreshold = 46;
  public static int serverLoadMediumThreshold = 49;
  public static int serverLoadHighThreshold = 55;

  public static int timeBetweenUpdates = 5;
  public static boolean logServerLoad = true;
  public static boolean logServerLevelLoadChanges = false;
  public static int serverLoadLogIntervalSeconds = 60;
  public static int serverLoadLogSignificantChangeSteps = 2;
  public static int serverLoadLogTopWorldCount = 5;
  public static boolean writeEntityTrackingReport = true;

  private static File configFile;

  private CoreConfig() {
  }

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    parseConfigFile();
  }

  public static void parseConfigFile() {
    configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodifiedProperties = (Properties) properties.clone();

    serverLoadVeryLowThreshold =
      parseConfigValue(properties, "serverLoadVeryLowThreshold", serverLoadVeryLowThreshold);
    serverLoadLowThreshold =
      parseConfigValue(properties, "serverLoadLowThreshold", serverLoadLowThreshold);
    serverLoadNormalThreshold =
      parseConfigValue(properties, "serverLoadNormalThreshold", serverLoadNormalThreshold);
    serverLoadMediumThreshold =
      parseConfigValue(properties, "serverLoadMediumThreshold", serverLoadMediumThreshold);
    serverLoadHighThreshold =
      parseConfigValue(properties, "serverLoadHighThreshold", serverLoadHighThreshold);

    timeBetweenUpdates = parseConfigValue(properties, "timeBetweenUpdates", timeBetweenUpdates);
    logServerLoad = parseConfigValue(properties, "logServerLoad", logServerLoad);
    logServerLevelLoadChanges = parseConfigValue(properties, "logServerLevelLoadChanges",
      logServerLevelLoadChanges);
    serverLoadLogIntervalSeconds = parseConfigValue(properties, "serverLoadLogIntervalSeconds",
      serverLoadLogIntervalSeconds);
    serverLoadLogSignificantChangeSteps = parseConfigValue(properties,
      "serverLoadLogSignificantChangeSteps", serverLoadLogSignificantChangeSteps);
    serverLoadLogTopWorldCount = parseConfigValue(properties, "serverLoadLogTopWorldCount",
      serverLoadLogTopWorldCount);
    writeEntityTrackingReport = parseConfigValue(properties, "writeEntityTrackingReport",
      writeEntityTrackingReport);

    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodifiedProperties);
  }

  public static boolean isFeatureEnabled(FeatureToggle toggle) {
    if (!featureFlags.containsKey(toggle)) {
      applyFeatureState(toggle, toggle.getDefaultState());
    }

    return featureFlags.get(toggle);
  }

  public static void applyFeatureState(FeatureToggle toggle, FeatureState configuredState) {
    ModConflictDetector.FeatureDecision decision =
      ModConflictDetector.resolveFeatureDecision(toggle, configuredState);
    featureFlags.put(toggle, decision.enabled());
    featureDecisions.put(toggle, decision);
  }

  public static void setFeatureEnabled(FeatureToggle toggle, boolean enabled) {
    featureFlags.put(toggle, enabled);
  }

  public static ModConflictDetector.FeatureDecision getFeatureDecision(FeatureToggle toggle) {
    if (!featureDecisions.containsKey(toggle)) {
      applyFeatureState(toggle, toggle.getDefaultState());
    }

    return featureDecisions.get(toggle);
  }
}
