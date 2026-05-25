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

package de.markusbordihn.adaptiveperformancetweaks.feature.player;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.compat.ModConflictDetector;
import de.markusbordihn.adaptiveperformancetweaks.core.config.Config;
import de.markusbordihn.adaptiveperformancetweaks.core.config.CoreConfig;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import java.io.File;
import java.util.Properties;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class PlayerLoginProtectionConfig extends Config {

  public static final String CONFIG_FILE_NAME = "player_login_protection.cfg";
  private static final String CONFIG_FILE_HEADER =
      """
       Player Login Protection Configuration

       When enabled, players receive brief invisibility + invulnerability on login
       until they move (or until the timeout expires).
      """;

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public static boolean protectPlayerDuringLogin = true;
  public static int playerLoginValidationTimeout = 30;
  public static boolean protectPlayerDuringLoginLogging = true;

  private PlayerLoginProtectionConfig() {}

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodified = new Properties();
    unmodified.putAll(properties);

    CoreConfig.setFeatureEnabled(
        FeatureToggle.PLAYER_LOGIN_PROTECTION,
        ModConflictDetector.resolveFeatureState(
            FeatureToggle.PLAYER_LOGIN_PROTECTION,
            parseConfigValue(
                properties, "enabled", FeatureToggle.PLAYER_LOGIN_PROTECTION.getDefaultState())));

    protectPlayerDuringLogin =
        parseBoolean(properties, "protectPlayerDuringLogin", protectPlayerDuringLogin);
    playerLoginValidationTimeout =
        parseInt(properties, "playerLoginValidationTimeout", playerLoginValidationTimeout);
    protectPlayerDuringLoginLogging =
        parseBoolean(
            properties, "protectPlayerDuringLoginLogging", protectPlayerDuringLoginLogging);

    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodified);

    if (protectPlayerDuringLogin) {
      log.info("Player login protection enabled (max {} secs).", playerLoginValidationTimeout);
    }
  }

  private static boolean parseBoolean(Properties props, String key, boolean defaultValue) {
    props.putIfAbsent(key, String.valueOf(defaultValue));
    return Boolean.parseBoolean(props.getProperty(key));
  }

  private static int parseInt(Properties props, String key, int defaultValue) {
    props.putIfAbsent(key, String.valueOf(defaultValue));
    try {
      return Integer.parseInt(props.getProperty(key));
    } catch (NumberFormatException e) {
      log.warn("Invalid integer for '{}', using default {}", key, defaultValue);
      return defaultValue;
    }
  }
}
