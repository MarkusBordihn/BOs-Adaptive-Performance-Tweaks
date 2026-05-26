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
import net.minecraftforge.event.AddReloadListenerEvent;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.entity.living.LivingDamageEvent;
import net.minecraftforge.event.entity.living.LivingHurtEvent;
import net.minecraftforge.event.entity.living.MobSpawnEvent;
import net.minecraftforge.event.entity.player.PlayerEvent;
import net.minecraftforge.event.server.ServerAboutToStartEvent;
import net.minecraftforge.event.server.ServerStartedEvent;
import net.minecraftforge.event.server.ServerStartingEvent;
import net.minecraftforge.event.server.ServerStoppingEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;

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
  public static void handleServerTick(TickEvent.ServerTickEvent event) {
    if (event.phase == TickEvent.Phase.END) {
      CommonServerEventHandler.handleServerTick();
    }
  }

  @SubscribeEvent
  public static void handleLevelTick(TickEvent.LevelTickEvent event) {
    if (!(event.level instanceof ServerLevel serverLevel)) {
      return;
    }

    if (event.phase == TickEvent.Phase.START) {
      CommonServerEventHandler.handleServerLevelTickStart(serverLevel);
      return;
    }

    if (event.phase == TickEvent.Phase.END) {
      CommonServerEventHandler.handleServerLevelTickEnd(serverLevel);
    }
  }

  @SubscribeEvent
  public static void handleAddReloadListener(AddReloadListenerEvent event) {
    if (FeatureToggle.SPAWN.isEnabled()) {
      event.addListener(new SpawnPresetLoader());
    }
  }

  @SubscribeEvent
  public static void handleFinalizeSpawn(MobSpawnEvent.FinalizeSpawn event) {
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
  public static void handleLivingHurt(LivingHurtEvent event) {
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
  public static void handleLivingDamage(LivingDamageEvent event) {
    event.setAmount(PlayerDamageManager.handleLivingDamage(event.getSource(), event.getAmount()));
  }
}
