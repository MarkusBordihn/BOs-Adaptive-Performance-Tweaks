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

package de.markusbordihn.adaptiveperformancetweaks.core.debug;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.core.LoggerContext;
import org.apache.logging.log4j.core.config.Configuration;
import org.apache.logging.log4j.core.config.LoggerConfig;

public final class DebugManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static boolean isDevelopmentEnvironment = false;

  private DebugManager() {}

  public static void setLogLevel(Logger logger, Level logLevel) {
    if (logLevel == null || logLevel == logger.getLevel()) {
      return;
    }

    String loggerName = logger.getName();
    LoggerContext context = (LoggerContext) LogManager.getContext(false);
    Configuration config = context.getConfiguration();
    LoggerConfig loggerConfig = config.getLoggerConfig(loggerName);
    LoggerConfig specificConfig = loggerConfig;
    if (!loggerConfig.getName().equals(loggerName)) {
      log.info("Add new logger config for {} with level {} ...", loggerName, logLevel);
      specificConfig = new LoggerConfig(loggerName, logLevel, true);
      specificConfig.setParent(loggerConfig);
      config.addLogger(loggerName, specificConfig);
    } else {
      log.info("Changing log level for {} from {} to {}", loggerName, logger.getLevel(), logLevel);
    }
    specificConfig.setLevel(logLevel);
    context.updateLoggers();
  }

  public static void enableDebugLevel(boolean enable) {
    enableDebugLevel(Constants.LOG_NAME, enable);
  }

  public static void enableDebugLevel(String loggerName, boolean enable) {
    Logger logger = LogManager.getLogger(loggerName);
    setLogLevel(logger, enable ? Level.DEBUG : Level.INFO);
  }

  public static Level getLogLevel(String loggerName) {
    return LogManager.getLogger(loggerName).getLevel();
  }

  public static boolean isDebugLevel(String loggerName) {
    Level level = getLogLevel(loggerName);
    return level == Level.DEBUG || level == Level.TRACE || level == Level.ALL;
  }

  public static void checkForDebugLogging(String loggerName) {
    if (!isDebugLevel(loggerName)) {
      return;
    }

    Logger logger = LogManager.getLogger(loggerName);
    String logLevelName = logger.getLevel().name();
    if (isDevelopmentEnvironment()) {
      logger.info("Detected DEV environment, will not change log level for {}!", loggerName);
    } else {
      enableDebugLevel(loggerName, false);
      logger.warn(
          () ->
              String.format(
                  "⚠ The log level for %s is set to %s. This logs all debug information and may cause "
                      + "performance issues. This is expected in a developer environment or during "
                      + "large-scale troubleshooting. To mitigate this, debug mode has been automatically "
                      + "adjusted to info level. Use /aptweaks debug <module> to re-enable debug mode.",
                  loggerName, logLevelName));
    }
  }

  public static boolean isDevelopmentEnvironment() {
    return DebugManager.isDevelopmentEnvironment;
  }

  public static void setDevelopmentEnvironment(boolean isDevelopmentEnvironment) {
    DebugManager.isDevelopmentEnvironment = isDevelopmentEnvironment;
  }

  public static boolean isDebugMode() {
    return isDevelopmentEnvironment() || isDebugLevel(Constants.LOG_NAME);
  }
}
