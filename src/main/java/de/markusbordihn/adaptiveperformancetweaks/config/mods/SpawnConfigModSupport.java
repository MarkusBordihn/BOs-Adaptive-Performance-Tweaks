package de.markusbordihn.adaptiveperformancetweaks.config.mods;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.config.CommonConfig;

public abstract class SpawnConfigModSupport {

  protected SpawnConfigModSupport() {}

  public static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public static final CommonConfig.Config COMMON = CommonConfig.COMMON;
}
