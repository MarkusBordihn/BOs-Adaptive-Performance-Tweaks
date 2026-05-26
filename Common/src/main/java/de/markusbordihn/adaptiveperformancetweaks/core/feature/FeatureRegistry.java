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

package de.markusbordihn.adaptiveperformancetweaks.core.feature;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.compat.ModConflictDetector;
import de.markusbordihn.adaptiveperformancetweaks.core.config.CoreConfig;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadDispatcher;
import de.markusbordihn.adaptiveperformancetweaks.feature.aithrottle.AiThrottleConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.aithrottle.AiThrottleManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.chunkgenthrottle.ChunkGenThrottleConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.chunkgenthrottle.ChunkGenThrottleManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.distance.SimulationDistanceConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.distance.SimulationDistanceManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.distance.ViewDistanceConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.distance.ViewDistanceManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.gamerules.GameRuleManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.gamerules.GameRulesConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.items.ArrowsConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.items.ExperienceOrbsConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.items.ItemsConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.monitoring.MonitoringConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.monitoring.MonitoringManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.player.PlayerEasyChildModeConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.player.PlayerLoginProtectionConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.player.PlayerStarterProtectionConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.spawn.SpawnConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.spawn.SpawnManager;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class FeatureRegistry {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private FeatureRegistry() {
  }

  public static void registerConfigs() {
    log.debug("{} Feature configurations ...", Constants.LOG_REGISTER_PREFIX);
    ModConflictDetector.logCompatibilityWarnings();
    for (FeatureToggle toggle : FeatureToggle.values()) {
      if (toggle == FeatureToggle.CORE) {
        continue;
      }

      applyFeatureConfig(toggle);
    }
  }

  private static void applyFeatureConfig(FeatureToggle toggle) {
    switch (toggle) {
      case ADAPTIVE_SIMULATION_DISTANCE -> SimulationDistanceConfig.registerConfig();
      case ADAPTIVE_VIEW_DISTANCE -> ViewDistanceConfig.registerConfig();
      case AI_THROTTLING -> AiThrottleConfig.registerConfig();
      case CHUNK_GEN_THROTTLE -> ChunkGenThrottleConfig.registerConfig();
      case GAMERULES -> GameRulesConfig.registerConfig();
      case MONITORING -> MonitoringConfig.registerConfig();
      case ITEMS -> ItemsConfig.registerConfig();
      case EXPERIENCE_ORBS -> ExperienceOrbsConfig.registerConfig();
      case ARROWS -> ArrowsConfig.registerConfig();
      case PLAYER_LOGIN_PROTECTION -> PlayerLoginProtectionConfig.registerConfig();
      case PLAYER_EASY_CHILD_MODE -> PlayerEasyChildModeConfig.registerConfig();
      case PLAYER_STARTER_PROTECTION -> PlayerStarterProtectionConfig.registerConfig();
      case SPAWN -> SpawnConfig.registerConfig();
      default -> {
      }
    }
  }

  public static void registerCommon() {
    log.info("{} Feature Registry (common) ...", Constants.LOG_REGISTER_PREFIX);
    for (FeatureToggle toggle : FeatureToggle.values()) {
      if (!toggle.isEnabled()) {
        continue;
      }
      log.debug("Feature {} ({}) enabled", toggle.getId(), toggle.scope());
      if (toggle == FeatureToggle.ADAPTIVE_SIMULATION_DISTANCE) {
        ServerLoadDispatcher.register(SimulationDistanceManager::handleServerLoadEvent);
      }
      if (toggle == FeatureToggle.ADAPTIVE_VIEW_DISTANCE) {
        ServerLoadDispatcher.register(ViewDistanceManager::handleServerLoadEvent);
      }
      if (toggle == FeatureToggle.AI_THROTTLING) {
        ServerLoadDispatcher.register(AiThrottleManager::handleServerLoadEvent);
      }
      if (toggle == FeatureToggle.CHUNK_GEN_THROTTLE) {
        ServerLoadDispatcher.register(ChunkGenThrottleManager::handleServerLoadEvent);
      }
      if (toggle == FeatureToggle.MONITORING) {
        ServerLoadDispatcher.register(MonitoringManager::handleServerLoadEvent);
      }
      if (toggle == FeatureToggle.GAMERULES) {
        ServerLoadDispatcher.register(GameRuleManager::handleServerLoadEvent);
      }
      if (toggle == FeatureToggle.SPAWN) {
        ServerLoadDispatcher.register(SpawnManager::handleServerLoadEvent);
      }
    }
  }

  public static void reloadConfigs() {
    log.debug("{} Reloading feature configurations ...", Constants.LOG_REGISTER_PREFIX);
    CoreConfig.parseConfigFile();
    for (FeatureToggle toggle : FeatureToggle.values()) {
      if (toggle == FeatureToggle.CORE) {
        continue;
      }

      applyFeatureConfig(toggle);
    }
  }

  public static void registerClient() {
    log.info("{} Feature Registry (client) ...", Constants.LOG_REGISTER_PREFIX);
    for (FeatureToggle toggle : FeatureToggle.values()) {
      if (toggle.scope() != FeatureToggle.Scope.SERVER && toggle.isEnabled()) {
        log.debug("Client feature {} enabled", toggle.getId());
      }
    }
  }
}
