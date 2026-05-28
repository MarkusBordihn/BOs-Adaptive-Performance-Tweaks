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

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureRegistry;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureState;
import java.io.File;
import java.io.FileWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Locale;
import java.util.Properties;
import java.util.Set;
import java.util.TreeSet;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class Config {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String LOG_PREFIX = "[Config]";

  private static Path configPath =
    Paths.get("").toAbsolutePath().resolve("config").resolve(Constants.MOD_ID);
  private static boolean isLoaded = false;

  protected Config() {
  }

  public static void register() {
    prepareConfiguration();
    CoreConfig.registerConfig();
    FeatureRegistry.registerConfigs();
  }

  public static void prepareConfiguration() {
    if (isLoaded) {
      log.error("{} Configuration is already loaded", LOG_PREFIX);
      return;
    }

    if (Constants.CONFIG_DIR != null) {
      configPath = Constants.CONFIG_DIR.resolve(Constants.MOD_ID);
      log.debug("{} Updated configuration path to {}", LOG_PREFIX, configPath);
    }

    if (!configPath.toFile().exists()) {
      log.debug("{} Creating configuration folder {}", LOG_PREFIX, getConfigDirectory());
    }

    isLoaded = true;
  }

  public static void registerConfigFile(
    final String configFileName, final String configFileHeader) {
    File configFile = getConfigFile(configFileName.trim());
    if (configFile == null || !configFile.exists()) {
      createConfigFile(getConfigFile(configFileName.trim()), configFileHeader.trim());
    }
  }

  public static Properties readConfigFile(final File configFile) {
    Properties properties = new Properties();
    if (configFile == null || !configFile.exists()) {
      return properties;
    }

    try (var reader = Files.newBufferedReader(configFile.toPath())) {
      properties.load(reader);
    } catch (Exception exception) {
      log.error("{} Failed to read configuration file {}:", LOG_PREFIX, configFile, exception);
    }

    return properties;
  }

  public static void createConfigFile(final File configFile, final String header) {
    Properties properties = new Properties();
    log.debug("{} Try creating configuration file {}", LOG_PREFIX, configFile);
    try (FileWriter writer = new FileWriter(configFile)) {
      properties.store(writer, header.trim());
      log.debug("{} Created configuration file {}", LOG_PREFIX, configFile);
    } catch (Exception exception) {
      log.error("{} Failed to create configuration file {}:", LOG_PREFIX, configFile, exception);
    }
  }

  public static File getConfigFile(final String configFileName) {
    Path path = getConfigDirectory();
    if (path != null) {
      return path.resolve(configFileName).toFile();
    }

    return null;
  }

  private static Path getConfigDirectory() {
    try {
      return Files.createDirectories(configPath);
    } catch (Exception exception) {
      log.error("{} Failed to create configuration folder {}:", LOG_PREFIX, configPath, exception);
    }

    return null;
  }

  public static void updateConfigFileIfChanged(
    File configFile,
    String configFileHeader,
    Properties properties,
    Properties unmodifiedProperties) {
    if (!properties.equals(unmodifiedProperties)) {
      log.debug("{} Updating configuration file {}", LOG_PREFIX, configFile);
      try (FileWriter writer = new FileWriter(configFile)) {
        properties.store(writer, configFileHeader.trim());
      } catch (Exception exception) {
        log.error("{} Failed to update configuration file {}:", LOG_PREFIX, configFile, exception);
      }
    }
  }

  protected static String parseConfigValue(
    final Properties properties, final String key, final String defaultValue) {
    if (properties.containsKey(key)) {
      try {
        return properties.getProperty(key).trim();
      } catch (Exception exception) {
        log.error("{} Failed to parse String value for key {}:", LOG_PREFIX, key, exception);
      }
    }
    properties.setProperty(key, defaultValue);

    return defaultValue;
  }

  protected static int parseConfigValue(
    final Properties properties, final String key, final int defaultValue) {
    if (properties.containsKey(key)) {
      try {
        return Integer.parseInt(properties.getProperty(key).trim());
      } catch (Exception exception) {
        log.error("{} Failed to parse Integer value for key {}:", LOG_PREFIX, key, exception);
      }
    }
    properties.setProperty(key, Integer.toString(defaultValue));

    return defaultValue;
  }

  protected static double parseConfigValue(
    final Properties properties, final String key, final double defaultValue) {
    if (properties.containsKey(key)) {
      try {
        return Double.parseDouble(properties.getProperty(key).trim());
      } catch (Exception exception) {
        log.error("{} Failed to parse Double value for key {}:", LOG_PREFIX, key, exception);
      }
    }
    properties.setProperty(key, Double.toString(defaultValue));

    return defaultValue;
  }

  protected static boolean parseConfigValue(
    final Properties properties, final String key, final boolean defaultValue) {
    if (properties.containsKey(key)) {
      try {
        return Boolean.parseBoolean(properties.getProperty(key).trim());
      } catch (Exception exception) {
        log.error("{} Failed to parse Boolean value for key {}:", LOG_PREFIX, key, exception);
      }
    }
    properties.setProperty(key, Boolean.toString(defaultValue));

    return defaultValue;
  }

  protected static Set<String> parseConfigValue(
    final Properties properties, final String key, final Set<String> defaultValue) {
    if (properties.containsKey(key)) {
      try {
        String value = properties.getProperty(key).trim();
        if (value.isEmpty()) {
          return new TreeSet<>();
        }
        Set<String> parsed = new TreeSet<>();
        for (String entry : value.split(",\\s*")) {
          String trimmed = entry.trim();
          if (!trimmed.isEmpty()) {
            parsed.add(trimmed);
          }
        }
        return parsed;
      } catch (Exception exception) {
        log.error("{} Failed to parse Set value for key {}:", LOG_PREFIX, key, exception);
      }
    }
    if (!defaultValue.isEmpty()) {
      properties.setProperty(key, String.join(",", defaultValue));
    } else {
      properties.setProperty(key, "");
    }

    return new TreeSet<>(defaultValue);
  }

  protected static FeatureState parseConfigValue(
    final Properties properties, final String key, final FeatureState defaultValue) {
    if (properties.containsKey(key)) {
      try {
        return FeatureState.parse(properties.getProperty(key));
      } catch (Exception exception) {
        log.error("{} Failed to parse FeatureState value for key {}:", LOG_PREFIX, key, exception);
      }
    }
    properties.setProperty(key, defaultValue.name().toLowerCase(Locale.ROOT));

    return defaultValue;
  }

  protected static <E extends Enum<E>> E parseConfigValue(
    final Properties properties, final String key, final E defaultValue) {
    if (properties.containsKey(key)) {
      try {
        return Enum.valueOf(defaultValue.getDeclaringClass(),
          properties.getProperty(key).trim().toUpperCase(Locale.ROOT));
      } catch (Exception exception) {
        log.error("{} Failed to parse Enum value for key {}:", LOG_PREFIX, key, exception);
      }
    }
    properties.setProperty(key, defaultValue.name().toLowerCase(Locale.ROOT));

    return defaultValue;
  }
}
