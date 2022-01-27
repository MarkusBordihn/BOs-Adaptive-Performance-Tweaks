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

package de.markusbordihn.adaptiveperformancetweaksgamerules.gamerules;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.world.level.GameRules;

import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.server.ServerLifecycleHooks;
import net.minecraftforge.event.server.ServerAboutToStartEvent;
import net.minecraftforge.event.server.ServerStartingEvent;
import net.minecraftforge.fml.ModList;

import de.markusbordihn.adaptiveperformancetweaksgamerules.Constants;
import de.markusbordihn.adaptiveperformancetweaksgamerules.config.CommonConfig;
import de.markusbordihn.adaptiveperformancetweakscore.CoreConstants;
import de.markusbordihn.adaptiveperformancetweakscore.commands.CommandManager;
import de.markusbordihn.adaptiveperformancetweakscore.server.ServerLoadEvent;

@EventBusSubscriber
public class GameRuleManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final CommonConfig.Config COMMON = CommonConfig.COMMON;

  private static GameRules gameRules;

  private static boolean entityCrammingEnabled = COMMON.entityCrammingEnabled.get();
  private static boolean insomniaEnabled = COMMON.insomniaEnabled.get();
  private static boolean patrolSpawningEnabled = COMMON.patrolSpawningEnabled.get();
  private static boolean raidsEnabled = COMMON.raidsEnabled.get();
  private static boolean randomTickSpeedEnabled = COMMON.randomTickSpeedEnabled.get();
  private static boolean traderSpawningEnabled = COMMON.traderSpawningEnabled.get();

  private static int maxEntityCramming = COMMON.maxEntityCramming.get();
  private static int minEntityCramming = COMMON.maxEntityCramming.get();
  private static int minEntityCrammingMineColonies = 16;
  private static int randomTickSpeed = COMMON.randomTickSpeed.get();

  private static int timeBetweenUpdates = COMMON.timeBetweenUpdates.get() * 1000;
  private static long lastUpdateTime = System.currentTimeMillis();

  protected GameRuleManager() {}

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(ServerAboutToStartEvent event) {
    entityCrammingEnabled = COMMON.entityCrammingEnabled.get();
    insomniaEnabled = COMMON.insomniaEnabled.get();
    patrolSpawningEnabled = COMMON.patrolSpawningEnabled.get();
    raidsEnabled = COMMON.raidsEnabled.get();
    randomTickSpeedEnabled = COMMON.randomTickSpeedEnabled.get();
    traderSpawningEnabled = COMMON.traderSpawningEnabled.get();

    maxEntityCramming = COMMON.maxEntityCramming.get();
    minEntityCramming = COMMON.minEntityCramming.get();
    randomTickSpeed = COMMON.randomTickSpeed.get();

    timeBetweenUpdates = COMMON.timeBetweenUpdates.get() * 1000;

    // Entity Cramming (max).
    if (minEntityCramming >= maxEntityCramming) {
      minEntityCramming = maxEntityCramming - 1;
    }

    // Additional check for Mine Colonies to make sure we don't kill stucked entities.
    if (entityCrammingEnabled && ModList.get().isLoaded(CoreConstants.MINECOLONIES_MOD)
        && minEntityCramming < minEntityCrammingMineColonies) {
      log.warn(
          "WARNING: The recommended value for minEntityCramming with {} is min. {} instead of {}!",
          CoreConstants.MINECOLONIES_NAME, minEntityCrammingMineColonies, minEntityCramming);
      log.info("The minEntityCramming will be automatically set to {}!",
          minEntityCrammingMineColonies);
      minEntityCramming = minEntityCrammingMineColonies;
      if (maxEntityCramming <= minEntityCramming) {
        maxEntityCramming = minEntityCramming + 1;
      }
    }
  }

  @SubscribeEvent
  public static void handleServerStartingEvent(ServerStartingEvent event) {
    gameRules = ServerLifecycleHooks.getCurrentServer().getGameRules();

    log.info("Gamerules will be optimized with an {} sec delay between updates.",
        timeBetweenUpdates / 1000);

    if (randomTickSpeedEnabled) {
      log.info("Random Tick Speed will be optimized between {} and {}", 1, randomTickSpeed);
      if (gameRules.getInt(GameRules.RULE_RANDOMTICKING) != randomTickSpeed) {
        setRandomTickSpeed(randomTickSpeed);
      }
    }

    if (entityCrammingEnabled) {
      log.info("Max Entity Cramming will be optimized between {} and {}", minEntityCramming,
          maxEntityCramming);
      if (gameRules.getInt(GameRules.RULE_MAX_ENTITY_CRAMMING) != maxEntityCramming) {
        setMaxEntityCramming(maxEntityCramming);
      }
    }

    if (patrolSpawningEnabled) {
      log.info("Patrol spawning will be automatically disabled during very high server load!");
    }

    if (raidsEnabled) {
      log.info("Raids will be automatically disabled during very high server load!");
    }

    if (insomniaEnabled) {
      log.info(
          "Insomnia (phantoms spawn) will be automatically disabled during very high server load!");
    }

    if (traderSpawningEnabled) {
      log.info("Trader spawning will be automatically disabled during very high server load!");
    }
  }

  @SubscribeEvent
  public static void handleServerLoadEvent(ServerLoadEvent event) {
    gameRules = ServerLifecycleHooks.getCurrentServer().getGameRules();

    // Specific: Handle very high server load
    if (event.hasVeryHighServerLoad()) {
      if (entityCrammingEnabled) {
        decreaseMaxEntityCramming();
      }
      if (randomTickSpeedEnabled) {
        decreaseRandomTickSpeed();
      }
      if (patrolSpawningEnabled) {
        disablePatrolSpawning();
      }
      if (raidsEnabled) {
        disableRaids();
      }
      if (insomniaEnabled) {
        disableInsomnia();
      }
      if (traderSpawningEnabled) {
        disableTraderSpawning();
      }
      return;
    }

    // Specific: Handle high server load
    if (event.hasHighServerLoad()) {
      if (randomTickSpeedEnabled) {
        decreaseRandomTickSpeed();
      }
      if (raidsEnabled) {
        enableRaids();
      }
      return;
    }

    // To make the updates less noticeable we are delaying increasing updates.
    if (System.currentTimeMillis() - lastUpdateTime < timeBetweenUpdates) {
      return;
    }

    // General: Handle normal and low server load
    if (patrolSpawningEnabled) {
      enablePatrolSpawning();
    }
    if (raidsEnabled) {
      enableRaids();
    }
    if (insomniaEnabled) {
      enableInsomnia();
    }
    if (traderSpawningEnabled) {
      enableTraderSpawning();
    }

    // Specific: Handle normal server load
    if (event.hasNormalServerLoad()) {
      return;
    }

    // Specific: Handle low server load
    if (event.hasLowServerLoad()) {
      if (randomTickSpeedEnabled) {
        increaseRandomTickSpeed();
      }
      if (entityCrammingEnabled) {
        increaseMaxEntityCramming();
      }
    }

    // Update the last update time
    lastUpdateTime = System.currentTimeMillis();
  }

  public static void enablePatrolSpawning() {
    if (!gameRules.getBoolean(GameRules.RULE_DO_PATROL_SPAWNING)) {
      log.debug("Enable PatrolSpawning");
      CommandManager.executeServerCommand(String.format("gamerule doPatrolSpawning %s", true));
    }
  }

  public static void disablePatrolSpawning() {
    if (gameRules.getBoolean(GameRules.RULE_DO_PATROL_SPAWNING)) {
      log.debug("Disable PatrolSpawning");
      CommandManager.executeServerCommand(String.format("gamerule doPatrolSpawning %s", false));
    }
  }

  public static void enableInsomnia() {
    if (!gameRules.getBoolean(GameRules.RULE_DOINSOMNIA)) {
      log.debug("Enable Insomnia");
      CommandManager.executeServerCommand(String.format("gamerule doInsomnia %s", true));
    }
  }

  public static void disableInsomnia() {
    if (gameRules.getBoolean(GameRules.RULE_DOINSOMNIA)) {
      log.debug("Disable Insomnia");
      CommandManager.executeServerCommand(String.format("gamerule doInsomnia %s", false));
    }
  }

  public static void enableRaids() {
    if (gameRules.getBoolean(GameRules.RULE_DISABLE_RAIDS)) {
      log.debug("Enable Raids");
      CommandManager.executeServerCommand(String.format("gamerule disableRaids %s", false));
    }
  }

  public static void disableRaids() {
    if (!gameRules.getBoolean(GameRules.RULE_DISABLE_RAIDS)) {
      log.debug("Disable Raids");
      CommandManager.executeServerCommand(String.format("gamerule disableRaids %s", true));
    }
  }

  public static void enableTraderSpawning() {
    if (!gameRules.getBoolean(GameRules.RULE_DO_TRADER_SPAWNING)) {
      log.debug("Enable TraderSpawning");
      CommandManager.executeServerCommand(String.format("gamerule doTraderSpawning %s", true));
    }
  }

  public static void disableTraderSpawning() {
    if (gameRules.getBoolean(GameRules.RULE_DO_TRADER_SPAWNING)) {
      log.debug("Disable TraderSpawning");
      CommandManager.executeServerCommand(String.format("gamerule doTraderSpawning %s", false));
    }
  }

  public static void decreaseRandomTickSpeed() {
    setRandomTickSpeed(gameRules.getInt(GameRules.RULE_RANDOMTICKING) - 1);
  }

  public static void increaseRandomTickSpeed() {
    setRandomTickSpeed(gameRules.getInt(GameRules.RULE_RANDOMTICKING) + 1);
  }

  public static void setRandomTickSpeed(int tickSpeed) {
    int currentTickSpeed = gameRules.getInt(GameRules.RULE_RANDOMTICKING);
    if (tickSpeed < 1) {
      tickSpeed = 1;
    } else if (tickSpeed > randomTickSpeed) {
      tickSpeed = randomTickSpeed;
    }
    if (currentTickSpeed != tickSpeed) {
      log.debug("Changing randomTickSpeed from {} to {}", currentTickSpeed, tickSpeed);
      CommandManager.executeServerCommand(String.format("gamerule randomTickSpeed %s", tickSpeed));
    }
  }

  public static void decreaseMaxEntityCramming() {
    setMaxEntityCramming(gameRules.getInt(GameRules.RULE_MAX_ENTITY_CRAMMING) - 1);
  }

  public static void increaseMaxEntityCramming() {
    setMaxEntityCramming(gameRules.getInt(GameRules.RULE_MAX_ENTITY_CRAMMING) + 1);
  }

  public static void setMaxEntityCramming(int maxEntity) {
    if (maxEntity < minEntityCramming) {
      maxEntity = minEntityCramming;
    } else if (maxEntity > maxEntityCramming) {
      maxEntity = maxEntityCramming;
    }
    int currentMaxEntityCramming = gameRules.getInt(GameRules.RULE_MAX_ENTITY_CRAMMING);
    if (currentMaxEntityCramming != maxEntity) {
      log.debug("Changing maxEntityCramming from {} to {}", currentMaxEntityCramming, maxEntity);
      CommandManager
          .executeServerCommand(String.format("gamerule maxEntityCramming %s", maxEntity));
    }
  }

  public static GameRules getGameRules() {
    if (gameRules == null) {
      gameRules = ServerLifecycleHooks.getCurrentServer().getGameRules();
    }
    return gameRules;
  }

  public static Map<String, String> getGameRulesOverview() {
    Map<String, String> overview = new ConcurrentHashMap<>();
    overview.put("disableRaids",
        String.valueOf(gameRules.getBoolean(GameRules.RULE_DISABLE_RAIDS)));
    overview.put("doInsomnia", String.valueOf(gameRules.getBoolean(GameRules.RULE_DOINSOMNIA)));
    overview.put("doPatrolSpawning",
        String.valueOf(gameRules.getBoolean(GameRules.RULE_DO_PATROL_SPAWNING)));
    overview.put("doTraderSpawning",
        String.valueOf(gameRules.getBoolean(GameRules.RULE_DO_TRADER_SPAWNING)));
    overview.put("maxEntityCramming",
        String.valueOf(gameRules.getInt(GameRules.RULE_MAX_ENTITY_CRAMMING)));
    overview.put("randomTickSpeed", String.valueOf(gameRules.getInt(GameRules.RULE_RANDOMTICKING)));
    return overview;
  }

}
