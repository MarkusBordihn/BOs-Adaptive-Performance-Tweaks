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

package de.markusbordihn.adaptiveperformancetweaks.feature.monitoring;

import de.markusbordihn.adaptiveperformancetweaks.core.config.Config;
import de.markusbordihn.adaptiveperformancetweaks.core.config.CoreConfig;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import java.io.File;
import java.util.Properties;

public final class MonitoringConfig extends Config {

  public static final String CONFIG_FILE_NAME = "monitoring.cfg";
  private static final String CONFIG_FILE_HEADER =
    """
       Monitoring Feature Configuration
      
       Periodically logs server performance metrics to the server log.
       Set monitoringIntervalSeconds to 0 to log only on load-level changes.
      """;

  public static int monitoringIntervalSeconds = 30;
  public static boolean monitoringLogTps = true;
  public static boolean monitoringLogLoadLevel = true;
  public static boolean monitoringLogPlayers = true;
  public static boolean monitoringLogEntities = true;

  private MonitoringConfig() {
  }

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodified = new Properties();
    unmodified.putAll(properties);

    CoreConfig.applyFeatureState(
      FeatureToggle.MONITORING,
      parseConfigValue(properties, "enabled", FeatureToggle.MONITORING.getDefaultState()));

    monitoringIntervalSeconds = parseConfigValue(properties, "monitoringIntervalSeconds",
      monitoringIntervalSeconds);
    monitoringLogTps = parseConfigValue(properties, "monitoringLogTps", monitoringLogTps);
    monitoringLogLoadLevel = parseConfigValue(properties, "monitoringLogLoadLevel",
      monitoringLogLoadLevel);
    monitoringLogPlayers = parseConfigValue(properties, "monitoringLogPlayers",
      monitoringLogPlayers);
    monitoringLogEntities = parseConfigValue(properties, "monitoringLogEntities",
      monitoringLogEntities);

    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodified);
    log.debug("Monitoring enabled: interval={}s tps={} loadLevel={} players={} entities={}",
      monitoringIntervalSeconds, monitoringLogTps, monitoringLogLoadLevel,
      monitoringLogPlayers, monitoringLogEntities);
  }
}
