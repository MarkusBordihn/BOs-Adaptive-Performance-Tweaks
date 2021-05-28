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

@EventBusSubscriber(bus = EventBusSubscriber.Bus.MOD)
public class DebugManager extends Manager {

  private static final Logger log = getLogger(DebugManager.class.getSimpleName());

  @SubscribeEvent
  public static void handleModConfigLoadEvent(ModConfig.Loading event) {
    adjustLogLevel(CommonConfig.COMMON.logLevel.get());
  }

  public static void adjustLogLevel(String logLevel) {
    log.info("Try to change log level to {}", logLevel);
    switch (logLevel) {
      case "info":
        Configurator.setAllLevels(LogManager.getLogger(Constants.LOG_NAME).getName(), Level.INFO);
        break;
      case "trace":
        Configurator.setAllLevels(LogManager.getLogger(Constants.LOG_NAME).getName(), Level.TRACE);
        break;
      case "debug":
        Configurator.setAllLevels(LogManager.getLogger(Constants.LOG_NAME).getName(), Level.DEBUG);
        break;
      case "warn":
        Configurator.setAllLevels(LogManager.getLogger(Constants.LOG_NAME).getName(), Level.WARN);
        break;
      case "error":
        Configurator.setAllLevels(LogManager.getLogger(Constants.LOG_NAME).getName(), Level.ERROR);
        break;
      case "fatal":
        Configurator.setAllLevels(LogManager.getLogger(Constants.LOG_NAME).getName(), Level.FATAL);
        break;
      default:
        log.error("Got invalid log level {} from config file!", CommonConfig.COMMON.logLevel.get());
        Configurator.setAllLevels(LogManager.getLogger(Constants.LOG_NAME).getName(), Level.INFO);
    }
  }
}
