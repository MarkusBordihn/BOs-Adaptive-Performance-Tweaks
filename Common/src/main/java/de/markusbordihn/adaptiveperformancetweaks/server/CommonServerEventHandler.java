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

package de.markusbordihn.adaptiveperformancetweaks.server;

import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.core.player.PlayerPositionManager;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.distance.SimDistanceManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.distance.ViewDistanceManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.gamerules.GameRuleManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.items.ExperienceOrbManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.items.ItemEntityManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.player.PlayerDamageManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.player.PlayerLoginManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.spawn.SpawnManager;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerPlayer;

public final class CommonServerEventHandler {

  private CommonServerEventHandler() {
  }

  public static void handleServerAboutToStart(MinecraftServer server) {
    ServerManager.handleServerAboutToStart(server);
    if (FeatureToggle.ITEMS.isEnabled()) {
      ItemEntityManager.handleServerAboutToStart();
    }
    if (FeatureToggle.EXPERIENCE_ORBS.isEnabled()) {
      ExperienceOrbManager.handleServerAboutToStart();
    }
    if (FeatureToggle.PLAYER_LOGIN_PROTECTION.isEnabled()) {
      PlayerLoginManager.handleServerAboutToStart();
    }
    if (FeatureToggle.SPAWN.isEnabled()) {
      SpawnManager.handleServerAboutToStart();
    }
  }

  public static void handleServerStarting(MinecraftServer server) {
    ServerManager.handleServerStarting(server);
    if (FeatureToggle.GAMERULES.isEnabled()) {
      GameRuleManager.handleServerStarting(server);
    }
    if (FeatureToggle.PLAYER_EASY_CHILD_MODE.isEnabled()) {
      PlayerDamageManager.handleServerStarting();
    }
    if (FeatureToggle.ADAPTIVE_SIM_DISTANCE.isEnabled()) {
      SimDistanceManager.handleServerStarting(server);
    }
    if (FeatureToggle.ADAPTIVE_VIEW_DISTANCE.isEnabled()) {
      ViewDistanceManager.handleServerStarting(server);
    }
  }

  public static void handleServerStarted() {
    if (FeatureToggle.SPAWN.isEnabled()) {
      SpawnManager.handleServerStarted();
    }
  }

  public static void handleServerStopping(MinecraftServer server) {
    ServerManager.handleServerStopping(server);
  }

  public static void handleServerTick() {
    ServerManager.handleServerTick();
    if (FeatureToggle.SPAWN.isEnabled()) {
      SpawnManager.handleServerTick();
    }
    if (FeatureToggle.ITEMS.isEnabled()) {
      ItemEntityManager.handleServerTick();
    }
    if (FeatureToggle.EXPERIENCE_ORBS.isEnabled()) {
      ExperienceOrbManager.handleServerTick();
    }
    if (FeatureToggle.PLAYER_LOGIN_PROTECTION.isEnabled()) {
      PlayerLoginManager.handleServerTick();
    }
  }

  public static void handlePlayerLoggedIn(ServerPlayer serverPlayer) {
    ServerManager.handlePlayerCountChange();
    if (FeatureToggle.PLAYER_LOGIN_PROTECTION.isEnabled()) {
      PlayerLoginManager.handlePlayerLoggedIn(serverPlayer);
    }
  }

  public static void handlePlayerLoggedOut(ServerPlayer serverPlayer) {
    PlayerPositionManager.handlePlayerLoggedOut(serverPlayer.getStringUUID());
    ServerManager.handlePlayerCountChange();
    if (FeatureToggle.PLAYER_LOGIN_PROTECTION.isEnabled()) {
      PlayerLoginManager.handlePlayerLoggedOut(serverPlayer.getName().getString());
    }
  }
}
