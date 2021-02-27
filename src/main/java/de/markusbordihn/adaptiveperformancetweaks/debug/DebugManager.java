package de.markusbordihn.adaptiveperformancetweaks.debug;

import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.core.config.Configurator;

import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.config.ModConfig;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.Manager;
import de.markusbordihn.adaptiveperformancetweaks.config.CommonConfig;

@EventBusSubscriber(bus = EventBusSubscriber.Bus.MOD)
public class DebugManager extends Manager {

  @SubscribeEvent
  public static void handleModConfigLoadEvent(ModConfig.Loading event) {
    adjustLogLevel(CommonConfig.COMMON.logLevel.get());
  }

  public static void adjustLogLevel(String logLevel) {
    log.info("Try to change log level to {}", logLevel);
    switch (logLevel) {
      case "info":
        Configurator.setAllLevels(LogManager.getLogger(Constants.LOG_NAME).getName(), Level.TRACE);
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
