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

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.commands.CommandManager;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.feature.player.PlayerDamageManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.spawn.SpawnManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.spawn.SpawnPresetLoader;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.neoforge.event.AddReloadListenerEvent;
import net.neoforged.neoforge.event.RegisterCommandsEvent;
import net.neoforged.neoforge.event.entity.living.FinalizeSpawnEvent;
import net.neoforged.neoforge.event.entity.living.LivingDamageEvent;
import net.neoforged.neoforge.event.entity.living.LivingIncomingDamageEvent;
import net.neoforged.neoforge.event.entity.player.PlayerEvent;
import net.neoforged.neoforge.event.server.ServerAboutToStartEvent;
import net.neoforged.neoforge.event.server.ServerStartedEvent;
import net.neoforged.neoforge.event.server.ServerStartingEvent;
import net.neoforged.neoforge.event.server.ServerStoppingEvent;
import net.neoforged.neoforge.event.tick.LevelTickEvent;
import net.neoforged.neoforge.event.tick.ServerTickEvent;

@SuppressWarnings("unused")
@EventBusSubscriber(modid = Constants.MOD_ID)
public final class ServerEventHandler {

  private ServerEventHandler() {
  }

  @SubscribeEvent
  public static void handleServerAboutToStart(ServerAboutToStartEvent event) {
    CommonServerEventHandler.handleServerAboutToStart(event.getServer());
  }

  @SubscribeEvent
  public static void handleServerStarting(ServerStartingEvent event) {
    CommonServerEventHandler.handleServerStarting(event.getServer());
  }

  @SubscribeEvent
  public static void handleServerStarted(ServerStartedEvent event) {
    CommonServerEventHandler.handleServerStarted();
  }

  @SubscribeEvent
  public static void handleServerStopping(ServerStoppingEvent event) {
    CommonServerEventHandler.handleServerStopping(event.getServer());
  }

  @SubscribeEvent
  public static void handleServerTick(ServerTickEvent.Post event) {
    CommonServerEventHandler.handleServerTick();
  }

  @SubscribeEvent
  public static void handleLevelTickStart(LevelTickEvent.Pre event) {
    if (!(event.getLevel() instanceof ServerLevel serverLevel)) {
      return;
    }
    CommonServerEventHandler.handleServerLevelTickStart(serverLevel);
  }

  @SubscribeEvent
  public static void handleLevelTickEnd(LevelTickEvent.Post event) {
    if (!(event.getLevel() instanceof ServerLevel serverLevel)) {
      return;
    }
    CommonServerEventHandler.handleServerLevelTickEnd(serverLevel);
  }

  @SubscribeEvent
  public static void handleAddReloadListener(AddReloadListenerEvent event) {
    if (FeatureToggle.SPAWN.isEnabled()) {
      event.addListener(new SpawnPresetLoader());
    }
  }

  @SubscribeEvent
  public static void handleFinalizeSpawn(FinalizeSpawnEvent event) {
    if (event.getLevel() instanceof ServerLevel serverLevel
      && SpawnManager.shouldDenyMobSpawn(event.getEntity(), serverLevel, event.getSpawnType())) {
      event.setSpawnCancelled(true);
    }
  }

  @SubscribeEvent
  public static void handleRegisterCommands(RegisterCommandsEvent event) {
    CommandManager.registerCommands(event.getDispatcher());
  }

  @SubscribeEvent
  public static void handlePlayerLoggedIn(PlayerEvent.PlayerLoggedInEvent event) {
    if (event.getEntity() instanceof ServerPlayer serverPlayer) {
      CommonServerEventHandler.handlePlayerLoggedIn(serverPlayer);
    }
  }

  @SubscribeEvent
  public static void handlePlayerLoggedOut(PlayerEvent.PlayerLoggedOutEvent event) {
    if (event.getEntity() instanceof ServerPlayer serverPlayer) {
      CommonServerEventHandler.handlePlayerLoggedOut(serverPlayer);
    }
  }

  @SubscribeEvent
  public static void handlePlayerRespawn(PlayerEvent.PlayerRespawnEvent event) {
    if (event.getEntity() instanceof ServerPlayer serverPlayer) {
      CommonServerEventHandler.handlePlayerTeleported(serverPlayer);
    }
  }

  @SubscribeEvent
  public static void handlePlayerChangedDimension(PlayerEvent.PlayerChangedDimensionEvent event) {
    if (event.getEntity() instanceof ServerPlayer serverPlayer) {
      CommonServerEventHandler.handlePlayerTeleported(serverPlayer);
    }
  }

  @SubscribeEvent
  public static void handleLivingHurt(LivingIncomingDamageEvent event) {
    if (!FeatureToggle.PLAYER_EASY_CHILD_MODE.isEnabled()
      && !FeatureToggle.PLAYER_STARTER_PROTECTION.isEnabled()) {
      return;
    }
    float modified = PlayerDamageManager.handleLivingHurt(event.getEntity(), event.getAmount());
    if (modified <= 0f) {
      event.setCanceled(true);
    } else {
      event.setAmount(modified);
    }
  }

  @SubscribeEvent
  public static void handleLivingDamage(LivingDamageEvent.Post event) {
    PlayerDamageManager.handleLivingDamage(event.getSource(), event.getNewDamage());
  }
}
