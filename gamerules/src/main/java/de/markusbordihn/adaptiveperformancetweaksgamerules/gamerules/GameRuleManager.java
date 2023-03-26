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

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final CommonConfig.Config COMMON = CommonConfig.COMMON;

  private static GameRules gameRules;

  private static int timeBetweenUpdates = 10 * 1000;
  private static long lastUpdateTime = System.currentTimeMillis();

  protected GameRuleManager() {}

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(ServerAboutToStartEvent event) {
    timeBetweenUpdates = COMMON.timeBetweenUpdates.get() * 1000;
  }

  @SubscribeEvent
  public static void handleServerStartingEvent(ServerStartingEvent event) {
    gameRules = ServerLifecycleHooks.getCurrentServer().getGameRules();

    log.info("Gamerules will be optimized with an {} sec delay between updates.",
        timeBetweenUpdates / 1000);

    if (Boolean.TRUE.equals(COMMON.randomTickSpeedEnabled.get())) {
      log.info("Random Tick Speed will be optimized between {} and {}", 1,
          COMMON.randomTickSpeed.get());
      if (gameRules.getInt(GameRules.RULE_RANDOMTICKING) != COMMON.randomTickSpeed.get()) {
        setRandomTickSpeed(COMMON.randomTickSpeed.get());
      }
    }

    if (Boolean.TRUE.equals(COMMON.entityCrammingEnabled.get())) {
      log.info("Max Entity Cramming will be optimized between {} and {}",
          COMMON.minEntityCramming.get(), COMMON.maxEntityCramming.get());
      if (gameRules.getInt(GameRules.RULE_MAX_ENTITY_CRAMMING) != COMMON.maxEntityCramming.get()) {
        setMaxEntityCramming(COMMON.maxEntityCramming.get());
      }
    }

    if (Boolean.TRUE.equals(COMMON.blockExplodesEnabled.get())) {
      log.info("Block explosions will be automatically disabled during very high server load!");
    }

    if (Boolean.TRUE.equals(COMMON.elytraMovementCheckEnabled.get())) {
      log.info(
          "Elytra movement check will be automatically disabled during very high server load!");
    }

    if (Boolean.TRUE.equals(COMMON.insomniaEnabled.get())) {
      log.info(
          "Insomnia (phantoms spawn) will be automatically disabled during very high server load!");
    }

    if (Boolean.TRUE.equals(COMMON.mobExplodesEnabled.get())) {
      log.info("Mob explosions will be automatically disabled during very high server load!");
    }

    if (Boolean.TRUE.equals(COMMON.patrolSpawningEnabled.get())) {
      log.info("Patrol spawning will be automatically disabled during very high server load!");
    }

    if (Boolean.TRUE.equals(COMMON.raidsEnabled.get())) {
      log.info("Raids will be automatically disabled during very high server load!");
    }

    if (Boolean.TRUE.equals(COMMON.traderSpawningEnabled.get())) {
      log.info("Trader spawning will be automatically disabled during very high server load!");
    }

    if (Boolean.TRUE.equals(COMMON.tntExplodesEnabled.get())) {
      log.info("TNT explosions will be automatically disabled during very high server load!");
    }

    if (Boolean.TRUE.equals(COMMON.vinesSpreadEnabled.get())) {
      log.info("Vines spread will be automatically disabled during very high server load!");
    }

    if (Boolean.TRUE.equals(COMMON.wardenSpawningEnabled.get())) {
      log.info("Warden spawning will be automatically disabled during very high server load!");
    }
  }

  @SubscribeEvent
  public static void handleServerLoadEvent(ServerLoadEvent event) {
    gameRules = ServerLifecycleHooks.getCurrentServer().getGameRules();

    // Specific: Handle very high server load
    if (event.hasVeryHighServerLoad()) {
      if (Boolean.TRUE.equals(COMMON.entityCrammingEnabled.get())) {
        decreaseMaxEntityCramming();
      }
      if (Boolean.TRUE.equals(COMMON.randomTickSpeedEnabled.get())) {
        decreaseRandomTickSpeed();
      }
      if (Boolean.TRUE.equals(COMMON.blockExplodesEnabled.get())) {
        enableBlockExplosionDropDecay();
      }
      if (Boolean.TRUE.equals(COMMON.elytraMovementCheckEnabled.get())) {
        disableElytraMovementCheck();
      }
      if (Boolean.TRUE.equals(COMMON.insomniaEnabled.get())) {
        disableInsomnia();
      }
      if (Boolean.TRUE.equals(COMMON.blockExplodesEnabled.get())) {
        enableMobExplosionDropDecay();
      }
      if (Boolean.TRUE.equals(COMMON.raidsEnabled.get())) {
        disableRaids();
      }
      if (Boolean.TRUE.equals(COMMON.patrolSpawningEnabled.get())) {
        disablePatrolSpawning();
      }
      if (Boolean.TRUE.equals(COMMON.traderSpawningEnabled.get())) {
        disableTraderSpawning();
      }
      if (Boolean.TRUE.equals(COMMON.tntExplodesEnabled.get())) {
        enableTntExplosionDropDecay();
      }
      if (Boolean.TRUE.equals(COMMON.vinesSpreadEnabled.get())) {
        disableVinesSpread();
      }
      if (Boolean.TRUE.equals(COMMON.wardenSpawningEnabled.get())) {
        disableWardenSpawning();
      }
      return;
    }

    // Specific: Handle high server load
    if (event.hasHighServerLoad()) {
      if (Boolean.TRUE.equals(COMMON.randomTickSpeedEnabled.get())) {
        decreaseRandomTickSpeed();
      }
      if (Boolean.TRUE.equals(COMMON.raidsEnabled.get())) {
        disableRaids();
      }
      return;
    }

    // To make the updates less noticeable we are delaying increasing updates.
    if (System.currentTimeMillis() - lastUpdateTime < timeBetweenUpdates) {
      return;
    }

    // General: Handle normal and low server load
    if (Boolean.TRUE.equals(COMMON.elytraMovementCheckEnabled.get())) {
      enableElytraMovementCheck();
    }
    if (Boolean.TRUE.equals(COMMON.raidsEnabled.get())) {
      enableRaids();
    }
    if (Boolean.TRUE.equals(COMMON.patrolSpawningEnabled.get())) {
      enablePatrolSpawning();
    }
    if (Boolean.TRUE.equals(COMMON.insomniaEnabled.get())) {
      enableInsomnia();
    }
    if (Boolean.TRUE.equals(COMMON.traderSpawningEnabled.get())) {
      enableTraderSpawning();
    }
    if (Boolean.TRUE.equals(COMMON.tntExplodesEnabled.get())) {
      disableTntExplosionDropDecay();
    }
    if (Boolean.TRUE.equals(COMMON.vinesSpreadEnabled.get())) {
      enableVinesSpread();
    }
    if (Boolean.TRUE.equals(COMMON.wardenSpawningEnabled.get())) {
      enableWardenSpawning();
    }

    // Specific: Handle normal server load
    if (event.hasNormalServerLoad()) {
      return;
    }

    // Specific: Handle low server load
    if (event.hasLowServerLoad()) {
      if (Boolean.TRUE.equals(COMMON.randomTickSpeedEnabled.get())) {
        increaseRandomTickSpeed();
      }
      if (Boolean.TRUE.equals(COMMON.entityCrammingEnabled.get())) {
        increaseMaxEntityCramming();
      }
    }

    // Update the last update time
    lastUpdateTime = System.currentTimeMillis();
  }

  public static void enableBlockExplosionDropDecay() {
    if (!gameRules.getBoolean(GameRules.RULE_BLOCK_EXPLOSION_DROP_DECAY)) {
      log.debug("Enable blockExplosionDropDecay");
      CommandManager.executeGameRuleCommand(GameRules.RULE_BLOCK_EXPLOSION_DROP_DECAY, true);
    }
  }

  public static void disableBlockExplosionDropDecay() {
    if (gameRules.getBoolean(GameRules.RULE_BLOCK_EXPLOSION_DROP_DECAY)) {
      log.debug("Disable blockExplosionDropDecay");
      CommandManager.executeGameRuleCommand(GameRules.RULE_BLOCK_EXPLOSION_DROP_DECAY, false);
    }
  }

  public static void enableElytraMovementCheck() {
    if (gameRules.getBoolean(GameRules.RULE_DISABLE_ELYTRA_MOVEMENT_CHECK)) {
      log.debug("Enable ElytraMovementCheck");
      CommandManager.executeGameRuleCommand(GameRules.RULE_DISABLE_ELYTRA_MOVEMENT_CHECK, false);
    }
  }

  public static void disableElytraMovementCheck() {
    if (!gameRules.getBoolean(GameRules.RULE_DISABLE_ELYTRA_MOVEMENT_CHECK)) {
      log.debug("Disable ElytraMovementCheck");
      CommandManager.executeGameRuleCommand(GameRules.RULE_DISABLE_ELYTRA_MOVEMENT_CHECK, true);
    }
  }

  public static void enableInsomnia() {
    if (!gameRules.getBoolean(GameRules.RULE_DOINSOMNIA)) {
      log.debug("Enable Insomnia");
      CommandManager.executeGameRuleCommand(GameRules.RULE_DOINSOMNIA, true);
    }
  }

  public static void disableInsomnia() {
    if (gameRules.getBoolean(GameRules.RULE_DOINSOMNIA)) {
      log.debug("Disable Insomnia");
      CommandManager.executeGameRuleCommand(GameRules.RULE_DOINSOMNIA, false);
    }
  }

  public static void enableMobExplosionDropDecay() {
    if (!gameRules.getBoolean(GameRules.RULE_MOB_EXPLOSION_DROP_DECAY)) {
      log.debug("Enable mobExplosionDropDecay");
      CommandManager.executeGameRuleCommand(GameRules.RULE_MOB_EXPLOSION_DROP_DECAY, true);
    }
  }

  public static void disableMobExplosionDropDecay() {
    if (gameRules.getBoolean(GameRules.RULE_MOB_EXPLOSION_DROP_DECAY)) {
      log.debug("Disable mobExplosionDropDecay");
      CommandManager.executeGameRuleCommand(GameRules.RULE_MOB_EXPLOSION_DROP_DECAY, false);
    }
  }

  public static void enablePatrolSpawning() {
    if (!gameRules.getBoolean(GameRules.RULE_DO_PATROL_SPAWNING)) {
      log.debug("Enable PatrolSpawning");
      CommandManager.executeGameRuleCommand(GameRules.RULE_DO_PATROL_SPAWNING, true);
    }
  }

  public static void disablePatrolSpawning() {
    if (gameRules.getBoolean(GameRules.RULE_DO_PATROL_SPAWNING)) {
      log.debug("Disable PatrolSpawning");
      CommandManager.executeGameRuleCommand(GameRules.RULE_DO_PATROL_SPAWNING, false);
    }
  }

  public static void enableRaids() {
    if (gameRules.getBoolean(GameRules.RULE_DISABLE_RAIDS)) {
      log.debug("Enable Raids");
      CommandManager.executeGameRuleCommand(GameRules.RULE_DISABLE_RAIDS, false);
    }
  }

  public static void disableRaids() {
    if (!gameRules.getBoolean(GameRules.RULE_DISABLE_RAIDS)) {
      log.debug("Disable Raids");
      CommandManager.executeGameRuleCommand(GameRules.RULE_DISABLE_RAIDS, true);
    }
  }

  public static void enableTraderSpawning() {
    if (!gameRules.getBoolean(GameRules.RULE_DO_TRADER_SPAWNING)) {
      log.debug("Enable TraderSpawning");
      CommandManager.executeGameRuleCommand(GameRules.RULE_DO_TRADER_SPAWNING, true);
    }
  }

  public static void disableTraderSpawning() {
    if (gameRules.getBoolean(GameRules.RULE_DO_TRADER_SPAWNING)) {
      log.debug("Disable TraderSpawning");
      CommandManager.executeGameRuleCommand(GameRules.RULE_DO_TRADER_SPAWNING, false);
    }
  }

  public static void enableTntExplosionDropDecay() {
    if (!gameRules.getBoolean(GameRules.RULE_TNT_EXPLOSION_DROP_DECAY)) {
      log.debug("Enable tntExplosionDropDecay");
      CommandManager.executeGameRuleCommand(GameRules.RULE_TNT_EXPLOSION_DROP_DECAY, true);
    }
  }

  public static void disableTntExplosionDropDecay() {
    if (gameRules.getBoolean(GameRules.RULE_TNT_EXPLOSION_DROP_DECAY)) {
      log.debug("Disable tntExplosionDropDecay");
      CommandManager.executeGameRuleCommand(GameRules.RULE_TNT_EXPLOSION_DROP_DECAY, false);
    }
  }

  public static void enableVinesSpread() {
    if (!gameRules.getBoolean(GameRules.RULE_DO_VINES_SPREAD)) {
      log.debug("Enable VinesSpread");
      CommandManager.executeGameRuleCommand(GameRules.RULE_DO_VINES_SPREAD, true);
    }
  }

  public static void disableVinesSpread() {
    if (gameRules.getBoolean(GameRules.RULE_DO_VINES_SPREAD)) {
      log.debug("Disable VinesSpread");
      CommandManager.executeGameRuleCommand(GameRules.RULE_DO_VINES_SPREAD, false);
    }
  }

  public static void enableWardenSpawning() {
    if (!gameRules.getBoolean(GameRules.RULE_DO_WARDEN_SPAWNING)) {
      log.debug("Enable WardenSpawning");
      CommandManager.executeGameRuleCommand(GameRules.RULE_DO_WARDEN_SPAWNING, true);
    }
  }

  public static void disableWardenSpawning() {
    if (gameRules.getBoolean(GameRules.RULE_DO_WARDEN_SPAWNING)) {
      log.debug("Disable WardenSpawning");
      CommandManager.executeGameRuleCommand(GameRules.RULE_DO_WARDEN_SPAWNING, false);
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
    } else if (tickSpeed > COMMON.randomTickSpeed.get()) {
      tickSpeed = COMMON.randomTickSpeed.get();
    }
    if (currentTickSpeed != tickSpeed) {
      log.debug("Changing randomTickSpeed from {} to {}", currentTickSpeed, tickSpeed);
      CommandManager.executeGameRuleCommand(GameRules.RULE_RANDOMTICKING, tickSpeed);
    }
  }

  public static void decreaseMaxEntityCramming() {
    setMaxEntityCramming(gameRules.getInt(GameRules.RULE_MAX_ENTITY_CRAMMING) - 1);
  }

  public static void increaseMaxEntityCramming() {
    setMaxEntityCramming(gameRules.getInt(GameRules.RULE_MAX_ENTITY_CRAMMING) + 1);
  }

  public static void setMaxEntityCramming(int maxEntity) {
    if (maxEntity < COMMON.minEntityCramming.get()) {
      maxEntity = COMMON.minEntityCramming.get();
    } else if (maxEntity > COMMON.maxEntityCramming.get()) {
      maxEntity = COMMON.maxEntityCramming.get();
    }

    // Additional check for Mine Colonies to make sure we don't kill stucked entities.
    if (ModList.get().isLoaded(CoreConstants.MINECOLONIES_MOD)
        && maxEntity < COMMON.minEntityCrammingMineColonies.get()) {
      log.warn(
          "WARNING: The recommended value for minEntityCramming with {} is min. {} instead of {}!",
          CoreConstants.MINECOLONIES_NAME, COMMON.minEntityCrammingMineColonies.get(),
          COMMON.minEntityCramming.get());
      log.info("The minEntityCramming will be automatically set to {}!",
          COMMON.minEntityCrammingMineColonies.get());
      maxEntity = COMMON.minEntityCrammingMineColonies.get();
    }

    int currentMaxEntityCramming = gameRules.getInt(GameRules.RULE_MAX_ENTITY_CRAMMING);
    if (currentMaxEntityCramming != maxEntity) {
      log.debug("Changing maxEntityCramming from {} to {}", currentMaxEntityCramming, maxEntity);
      CommandManager.executeGameRuleCommand(GameRules.RULE_MAX_ENTITY_CRAMMING, maxEntity);
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
    overview.put(GameRules.RULE_MAX_ENTITY_CRAMMING.getId(),
        String.valueOf(gameRules.getInt(GameRules.RULE_MAX_ENTITY_CRAMMING)));
    overview.put(GameRules.RULE_RANDOMTICKING.getId(),
        String.valueOf(gameRules.getInt(GameRules.RULE_RANDOMTICKING)));

    overview.put(GameRules.RULE_BLOCK_EXPLOSION_DROP_DECAY.getId(),
        String.valueOf(gameRules.getBoolean(GameRules.RULE_BLOCK_EXPLOSION_DROP_DECAY)));
    overview.put(GameRules.RULE_DISABLE_ELYTRA_MOVEMENT_CHECK.getId(),
        String.valueOf(gameRules.getBoolean(GameRules.RULE_DISABLE_ELYTRA_MOVEMENT_CHECK)));
    overview.put(GameRules.RULE_DOINSOMNIA.getId(),
        String.valueOf(gameRules.getBoolean(GameRules.RULE_DOINSOMNIA)));
    overview.put(GameRules.RULE_MOB_EXPLOSION_DROP_DECAY.getId(),
        String.valueOf(gameRules.getBoolean(GameRules.RULE_MOB_EXPLOSION_DROP_DECAY)));
    overview.put(GameRules.RULE_DO_PATROL_SPAWNING.getId(),
        String.valueOf(gameRules.getBoolean(GameRules.RULE_DO_PATROL_SPAWNING)));
    overview.put(GameRules.RULE_DISABLE_RAIDS.getId(),
        String.valueOf(gameRules.getBoolean(GameRules.RULE_DISABLE_RAIDS)));
    overview.put(GameRules.RULE_TNT_EXPLOSION_DROP_DECAY.getId(),
        String.valueOf(gameRules.getBoolean(GameRules.RULE_TNT_EXPLOSION_DROP_DECAY)));
    overview.put(GameRules.RULE_DO_TRADER_SPAWNING.getId(),
        String.valueOf(gameRules.getBoolean(GameRules.RULE_DO_TRADER_SPAWNING)));
    overview.put(GameRules.RULE_DO_VINES_SPREAD.getId(),
        String.valueOf(gameRules.getBoolean(GameRules.RULE_DO_VINES_SPREAD)));
    overview.put(GameRules.RULE_DO_WARDEN_SPAWNING.getId(),
        String.valueOf(gameRules.getBoolean(GameRules.RULE_DO_WARDEN_SPAWNING)));
    return overview;
  }

}
