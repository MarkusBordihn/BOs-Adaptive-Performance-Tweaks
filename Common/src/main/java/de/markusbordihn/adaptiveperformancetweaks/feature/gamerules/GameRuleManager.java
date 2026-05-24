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

package de.markusbordihn.adaptiveperformancetweaks.feature.gamerules;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.commands.CommandManager;
import de.markusbordihn.adaptiveperformancetweaks.core.compat.ModCompat;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadEvent;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerManager;
import net.minecraft.server.MinecraftServer;
import net.minecraft.world.level.GameRules;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class GameRuleManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME_GAMERULES);
  private static final String LOG_PREFIX = "[Gamerule]";

  private static GameRules gameRules;
  private static long lastUpdateTime = System.currentTimeMillis();

  private GameRuleManager() {
  }

  public static void handleServerStarting(MinecraftServer minecraftServer) {
    gameRules = minecraftServer.getGameRules();

    if (GameRulesConfig.randomTickSpeedEnabled) {
      log.debug(
        "{} Random Tick Speed will be optimized between 1 and {}",
        LOG_PREFIX, GameRulesConfig.randomTickSpeed);
      if (gameRules.getInt(GameRules.RULE_RANDOMTICKING) != GameRulesConfig.randomTickSpeed) {
        setRandomTickSpeed(GameRulesConfig.randomTickSpeed);
      }
    }
    if (GameRulesConfig.entityCrammingEnabled) {
      log.debug(
        "{} Max Entity Cramming will be optimized between {} and {}",
        LOG_PREFIX,
        GameRulesConfig.minEntityCramming,
        GameRulesConfig.maxEntityCramming);
      if (gameRules.getInt(GameRules.RULE_MAX_ENTITY_CRAMMING)
        != GameRulesConfig.maxEntityCramming) {
        setMaxEntityCramming(GameRulesConfig.maxEntityCramming);
      }
    }
    logOptimizationInfo();
  }

  public static void handleServerStopping() {
    gameRules = null;
    lastUpdateTime = System.currentTimeMillis();
  }

  public static void handleServerLoadEvent(ServerLoadEvent event) {
    MinecraftServer minecraftServer = ServerManager.getMinecraftServer();
    if (minecraftServer == null) {
      return;
    }
    gameRules = minecraftServer.getGameRules();

    if (event.hasVeryHighServerLoad()) {
      applyVeryHighLoadOptimizations();
      return;
    }

    if (event.hasHighServerLoad()) {
      applyHighLoadOptimizations();
      return;
    }

    if (System.currentTimeMillis() - lastUpdateTime < 10_000L) {
      return;
    }

    restoreNormalLoad();

    if (event.hasLowServerLoad()) {
      if (GameRulesConfig.randomTickSpeedEnabled) {
        increaseRandomTickSpeed();
      }
      if (GameRulesConfig.entityCrammingEnabled) {
        increaseMaxEntityCramming();
      }
    }

    lastUpdateTime = System.currentTimeMillis();
  }

  private static void applyVeryHighLoadOptimizations() {
    if (GameRulesConfig.entityCrammingEnabled) {
      decreaseMaxEntityCramming();
    }
    if (GameRulesConfig.randomTickSpeedEnabled) {
      decreaseRandomTickSpeed();
    }
    if (GameRulesConfig.blockExplodesEnabled) {
      enableBlockExplosionDropDecay();
    }
    if (GameRulesConfig.elytraMovementCheckEnabled) {
      disableElytraMovementCheck();
    }
    if (GameRulesConfig.insomniaEnabled) {
      disableInsomnia();
    }
    if (GameRulesConfig.mobExplodesEnabled) {
      enableMobExplosionDropDecay();
    }
    if (GameRulesConfig.raidsEnabled) {
      disableRaids();
    }
    if (GameRulesConfig.patrolSpawningEnabled) {
      disablePatrolSpawning();
    }
    if (GameRulesConfig.traderSpawningEnabled) {
      disableTraderSpawning();
    }
    if (GameRulesConfig.tntExplodesEnabled) {
      enableTntExplosionDropDecay();
    }
    if (GameRulesConfig.vinesSpreadEnabled) {
      disableVinesSpread();
    }
    if (GameRulesConfig.wardenSpawningEnabled) {
      disableWardenSpawning();
    }
  }

  private static void applyHighLoadOptimizations() {
    if (GameRulesConfig.randomTickSpeedEnabled) {
      decreaseRandomTickSpeed();
    }
    if (GameRulesConfig.raidsEnabled) {
      disableRaids();
    }
  }

  private static void restoreNormalLoad() {
    if (GameRulesConfig.elytraMovementCheckEnabled) {
      enableElytraMovementCheck();
    }
    if (GameRulesConfig.raidsEnabled) {
      enableRaids();
    }
    if (GameRulesConfig.patrolSpawningEnabled) {
      enablePatrolSpawning();
    }
    if (GameRulesConfig.insomniaEnabled) {
      enableInsomnia();
    }
    if (GameRulesConfig.traderSpawningEnabled) {
      enableTraderSpawning();
    }
    if (GameRulesConfig.tntExplodesEnabled) {
      disableTntExplosionDropDecay();
    }
    if (GameRulesConfig.vinesSpreadEnabled) {
      enableVinesSpread();
    }
    if (GameRulesConfig.wardenSpawningEnabled) {
      enableWardenSpawning();
    }
  }

  private static void logOptimizationInfo() {
    StringBuilder active = new StringBuilder();
    if (GameRulesConfig.blockExplodesEnabled) {
      log.debug("{} Block explosions will be disabled during very high server load.", LOG_PREFIX);
      active.append(active.length() > 0 ? ", " : "").append("blockExplosions");
    }
    if (GameRulesConfig.elytraMovementCheckEnabled) {
      log.debug("{} Elytra movement check will be disabled during very high server load.",
        LOG_PREFIX);
      active.append(active.length() > 0 ? ", " : "").append("elytraMovementCheck");
    }
    if (GameRulesConfig.insomniaEnabled) {
      log.debug("{} Insomnia will be disabled during very high server load.", LOG_PREFIX);
      active.append(active.length() > 0 ? ", " : "").append("insomnia");
    }
    if (GameRulesConfig.mobExplodesEnabled) {
      log.debug("{} Mob explosions will be disabled during very high server load.", LOG_PREFIX);
      active.append(active.length() > 0 ? ", " : "").append("mobExplosions");
    }
    if (GameRulesConfig.patrolSpawningEnabled) {
      log.debug("{} Patrol spawning will be disabled during very high server load.", LOG_PREFIX);
      active.append(active.length() > 0 ? ", " : "").append("patrolSpawning");
    }
    if (GameRulesConfig.raidsEnabled) {
      log.debug("{} Raids will be disabled during very high server load.", LOG_PREFIX);
      active.append(active.length() > 0 ? ", " : "").append("raids");
    }
    if (GameRulesConfig.traderSpawningEnabled) {
      log.debug("{} Trader spawning will be disabled during very high server load.", LOG_PREFIX);
      active.append(active.length() > 0 ? ", " : "").append("traderSpawning");
    }
    if (GameRulesConfig.tntExplodesEnabled) {
      log.debug("{} TNT explosions will be disabled during very high server load.", LOG_PREFIX);
      active.append(active.length() > 0 ? ", " : "").append("tntExplosions");
    }
    if (GameRulesConfig.vinesSpreadEnabled) {
      log.debug("{} Vines spread will be disabled during very high server load.", LOG_PREFIX);
      active.append(active.length() > 0 ? ", " : "").append("vinesSpread");
    }
    if (GameRulesConfig.wardenSpawningEnabled) {
      log.debug("{} Warden spawning will be disabled during very high server load.", LOG_PREFIX);
      active.append(active.length() > 0 ? ", " : "").append("wardenSpawning");
    }
    if (active.length() > 0) {
      log.info("{} Very-high-load rules active: {}", LOG_PREFIX, active);
    }
  }

  public static void enableBlockExplosionDropDecay() {
    if (!gameRules.getBoolean(GameRules.RULE_BLOCK_EXPLOSION_DROP_DECAY)) {
      log.debug("{} blockExplosionDropDecay → true", LOG_PREFIX);
      CommandManager.executeGameRuleCommand(GameRules.RULE_BLOCK_EXPLOSION_DROP_DECAY, true);
    }
  }

  public static void enableElytraMovementCheck() {
    if (gameRules.getBoolean(GameRules.RULE_DISABLE_ELYTRA_MOVEMENT_CHECK)) {
      log.debug("{} disableElytraMovementCheck → false", LOG_PREFIX);
      CommandManager.executeGameRuleCommand(GameRules.RULE_DISABLE_ELYTRA_MOVEMENT_CHECK, false);
    }
  }

  public static void disableElytraMovementCheck() {
    if (!gameRules.getBoolean(GameRules.RULE_DISABLE_ELYTRA_MOVEMENT_CHECK)) {
      log.debug("{} disableElytraMovementCheck → true", LOG_PREFIX);
      CommandManager.executeGameRuleCommand(GameRules.RULE_DISABLE_ELYTRA_MOVEMENT_CHECK, true);
    }
  }

  public static void enableInsomnia() {
    if (!gameRules.getBoolean(GameRules.RULE_DOINSOMNIA)) {
      log.debug("{} doInsomnia → true", LOG_PREFIX);
      CommandManager.executeGameRuleCommand(GameRules.RULE_DOINSOMNIA, true);
    }
  }

  public static void disableInsomnia() {
    if (gameRules.getBoolean(GameRules.RULE_DOINSOMNIA)) {
      log.debug("{} doInsomnia → false", LOG_PREFIX);
      CommandManager.executeGameRuleCommand(GameRules.RULE_DOINSOMNIA, false);
    }
  }

  public static void enableMobExplosionDropDecay() {
    if (!gameRules.getBoolean(GameRules.RULE_MOB_EXPLOSION_DROP_DECAY)) {
      log.debug("{} mobExplosionDropDecay → true", LOG_PREFIX);
      CommandManager.executeGameRuleCommand(GameRules.RULE_MOB_EXPLOSION_DROP_DECAY, true);
    }
  }

  public static void enablePatrolSpawning() {
    if (!gameRules.getBoolean(GameRules.RULE_DO_PATROL_SPAWNING)) {
      log.debug("{} doPatrolSpawning → true", LOG_PREFIX);
      CommandManager.executeGameRuleCommand(GameRules.RULE_DO_PATROL_SPAWNING, true);
    }
  }

  public static void disablePatrolSpawning() {
    if (gameRules.getBoolean(GameRules.RULE_DO_PATROL_SPAWNING)) {
      log.debug("{} doPatrolSpawning → false", LOG_PREFIX);
      CommandManager.executeGameRuleCommand(GameRules.RULE_DO_PATROL_SPAWNING, false);
    }
  }

  public static void enableRaids() {
    if (gameRules.getBoolean(GameRules.RULE_DISABLE_RAIDS)) {
      log.debug("{} disableRaids → false", LOG_PREFIX);
      CommandManager.executeGameRuleCommand(GameRules.RULE_DISABLE_RAIDS, false);
    }
  }

  public static void disableRaids() {
    if (!gameRules.getBoolean(GameRules.RULE_DISABLE_RAIDS)) {
      log.debug("{} disableRaids → true", LOG_PREFIX);
      CommandManager.executeGameRuleCommand(GameRules.RULE_DISABLE_RAIDS, true);
    }
  }

  public static void enableTraderSpawning() {
    if (!gameRules.getBoolean(GameRules.RULE_DO_TRADER_SPAWNING)) {
      log.debug("{} doTraderSpawning → true", LOG_PREFIX);
      CommandManager.executeGameRuleCommand(GameRules.RULE_DO_TRADER_SPAWNING, true);
    }
  }

  public static void disableTraderSpawning() {
    if (gameRules.getBoolean(GameRules.RULE_DO_TRADER_SPAWNING)) {
      log.debug("{} doTraderSpawning → false", LOG_PREFIX);
      CommandManager.executeGameRuleCommand(GameRules.RULE_DO_TRADER_SPAWNING, false);
    }
  }

  public static void enableTntExplosionDropDecay() {
    if (!gameRules.getBoolean(GameRules.RULE_TNT_EXPLOSION_DROP_DECAY)) {
      log.debug("{} tntExplosionDropDecay → true", LOG_PREFIX);
      CommandManager.executeGameRuleCommand(GameRules.RULE_TNT_EXPLOSION_DROP_DECAY, true);
    }
  }

  public static void disableTntExplosionDropDecay() {
    if (gameRules.getBoolean(GameRules.RULE_TNT_EXPLOSION_DROP_DECAY)) {
      log.debug("{} tntExplosionDropDecay → false", LOG_PREFIX);
      CommandManager.executeGameRuleCommand(GameRules.RULE_TNT_EXPLOSION_DROP_DECAY, false);
    }
  }

  public static void enableVinesSpread() {
    if (!gameRules.getBoolean(GameRules.RULE_DO_VINES_SPREAD)) {
      log.debug("{} doVinesSpread → true", LOG_PREFIX);
      CommandManager.executeGameRuleCommand(GameRules.RULE_DO_VINES_SPREAD, true);
    }
  }

  public static void disableVinesSpread() {
    if (gameRules.getBoolean(GameRules.RULE_DO_VINES_SPREAD)) {
      log.debug("{} doVinesSpread → false", LOG_PREFIX);
      CommandManager.executeGameRuleCommand(GameRules.RULE_DO_VINES_SPREAD, false);
    }
  }

  public static void enableWardenSpawning() {
    if (!gameRules.getBoolean(GameRules.RULE_DO_WARDEN_SPAWNING)) {
      log.debug("{} doWardenSpawning → true", LOG_PREFIX);
      CommandManager.executeGameRuleCommand(GameRules.RULE_DO_WARDEN_SPAWNING, true);
    }
  }

  public static void disableWardenSpawning() {
    if (gameRules.getBoolean(GameRules.RULE_DO_WARDEN_SPAWNING)) {
      log.debug("{} doWardenSpawning → false", LOG_PREFIX);
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
    int clamped = Math.max(1, Math.min(tickSpeed, GameRulesConfig.randomTickSpeed));
    int current = gameRules.getInt(GameRules.RULE_RANDOMTICKING);
    if (current != clamped) {
      log.debug("{} randomTickSpeed: {} → {}", LOG_PREFIX, current, clamped);
      CommandManager.executeGameRuleCommand(GameRules.RULE_RANDOMTICKING, clamped);
    }
  }

  public static void decreaseMaxEntityCramming() {
    setMaxEntityCramming(gameRules.getInt(GameRules.RULE_MAX_ENTITY_CRAMMING) - 1);
  }

  public static void increaseMaxEntityCramming() {
    setMaxEntityCramming(gameRules.getInt(GameRules.RULE_MAX_ENTITY_CRAMMING) + 1);
  }

  public static void setMaxEntityCramming(int maxEntity) {
    int clamped = Math.max(GameRulesConfig.minEntityCramming,
      Math.min(maxEntity, GameRulesConfig.maxEntityCramming));
    int preAdjustedClamped = clamped;
    if (ModCompat.isModLoaded("minecolonies")
      && clamped < GameRulesConfig.minEntityCrammingMineColonies) {
      clamped = GameRulesConfig.minEntityCrammingMineColonies;
    }
    int current = gameRules.getInt(GameRules.RULE_MAX_ENTITY_CRAMMING);
    if (current != clamped) {
      if (clamped != preAdjustedClamped) {
        log.warn(
          "{} MineColonies detected: raising minEntityCramming from {} to {} to prevent stuck entities",
          LOG_PREFIX,
          preAdjustedClamped,
          GameRulesConfig.minEntityCrammingMineColonies);
      }
      log.debug("{} maxEntityCramming: {} → {}", LOG_PREFIX, current, clamped);
      CommandManager.executeGameRuleCommand(GameRules.RULE_MAX_ENTITY_CRAMMING, clamped);
    }
  }
}
