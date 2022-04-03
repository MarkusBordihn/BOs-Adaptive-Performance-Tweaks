/**
 * Copyright 2022 Markus Bordihn
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

package de.markusbordihn.adaptiveperformancetweakscore.debug;

import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.core.LoggerContext;
import org.apache.logging.log4j.core.config.Configuration;
import org.apache.logging.log4j.core.config.LoggerConfig;

import de.markusbordihn.adaptiveperformancetweakscore.Constants;

public class DebugManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected DebugManager() {}

  public static void setLogLevel(Logger logger, String logLevel) {
    log.info("Try to change log level to {}", logLevel);
    Level newLogLevel = Level.INFO;
    switch (logLevel) {
      case "info":
        break;
      case "trace":
        newLogLevel = Level.TRACE;
        break;
      case "debug":
        newLogLevel = Level.DEBUG;
        break;
      case "warn":
        newLogLevel = Level.WARN;
        break;
      case "error":
        newLogLevel = Level.ERROR;
        break;
      case "fatal":
        newLogLevel = Level.FATAL;
        break;
      default:
        log.error("Got invalid log level {} from config file for logger {}!", logLevel, logger);
    }
    setLogLevel(logger, newLogLevel);
  }

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
      log.info("Changing log level for {} from {} to {}", loggerName, logger.getLevel(), logLevel);
      specificConfig = new LoggerConfig(loggerName, logLevel, true);
      specificConfig.setParent(loggerConfig);
      config.addLogger(loggerName, specificConfig);
    }
    specificConfig.setLevel(logLevel);
    context.updateLoggers();
  }

  public static void enableDebugLevel(String loggerName, boolean enable) {
    Logger logger = LogManager.getLogger(loggerName);
    if (enable) {
      DebugManager.setLogLevel(logger, Level.DEBUG);
    } else {
      DebugManager.setLogLevel(logger, Level.INFO);
    }
  }

}
