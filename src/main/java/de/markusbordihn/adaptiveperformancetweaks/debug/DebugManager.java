/**
 * Copyright 2021 Markus Bordihn
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

package de.markusbordihn.adaptiveperformancetweaks.debug;

import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.core.config.Configurator;

import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.config.ModConfig;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.Manager;
import de.markusbordihn.adaptiveperformancetweaks.config.CommonConfig;
import de.markusbordihn.adaptiveperformancetweaks.entity.EntityManager;
import de.markusbordihn.adaptiveperformancetweaks.entity.ExperienceOrbEntityManager;
import de.markusbordihn.adaptiveperformancetweaks.entity.ItemEntityManager;
import de.markusbordihn.adaptiveperformancetweaks.entity.MonsterEntityManager;
import de.markusbordihn.adaptiveperformancetweaks.player.PlayerPositionManager;
import de.markusbordihn.adaptiveperformancetweaks.spawn.SpawnManager;
import de.markusbordihn.adaptiveperformancetweaks.spawn.SpawnerManager;

@EventBusSubscriber(bus = EventBusSubscriber.Bus.MOD)
public class DebugManager extends Manager {

  private static final String LOG_NAME = DebugManager.class.getSimpleName();
  private static final Logger log = getLogger(LOG_NAME);
  private static Level logLevel = Level.INFO;

  @SubscribeEvent
  public static void handleModConfigLoadEvent(ModConfig.Loading event) {
    adjustLogLevel(CommonConfig.COMMON.logLevel.get());
  }

  public static void adjustLogLevel(String logLevel) {
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
        log.error("Got invalid log level {} from config file!", CommonConfig.COMMON.logLevel.get());
    }

    Configurator.setAllLevels(LogManager.getLogger(Constants.LOG_NAME).getName(), newLogLevel);
    Configurator.setAllLevels(LogManager.getLogger(getLoggerName(EntityManager.LOG_NAME)).getName(),
        newLogLevel);
    Configurator.setAllLevels(
        LogManager.getLogger(getLoggerName(ExperienceOrbEntityManager.LOG_NAME)).getName(),
        newLogLevel);
    Configurator.setAllLevels(
        LogManager.getLogger(getLoggerName(ItemEntityManager.LOG_NAME)).getName(), newLogLevel);
    Configurator.setAllLevels(
        LogManager.getLogger(getLoggerName(MonsterEntityManager.LOG_NAME)).getName(), newLogLevel);
    Configurator.setAllLevels(
        LogManager.getLogger(getLoggerName(PlayerPositionManager.LOG_NAME)).getName(), newLogLevel);
    Configurator.setAllLevels(LogManager.getLogger(getLoggerName(SpawnManager.LOG_NAME)).getName(),
        newLogLevel);
    Configurator.setAllLevels(
        LogManager.getLogger(getLoggerName(SpawnerManager.LOG_NAME)).getName(), newLogLevel);
    log.info("Change log level from {} to {}", DebugManager.logLevel, newLogLevel);
    DebugManager.logLevel = newLogLevel;
  }
}
