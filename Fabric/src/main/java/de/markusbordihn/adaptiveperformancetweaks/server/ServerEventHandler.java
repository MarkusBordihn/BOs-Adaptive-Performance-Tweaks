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
import de.markusbordihn.adaptiveperformancetweaks.entity.CommonEntityEventHandler;
import de.markusbordihn.adaptiveperformancetweaks.feature.spawn.SpawnPresetLoader;
import java.util.concurrent.CompletableFuture;
import net.fabricmc.fabric.api.command.v2.CommandRegistrationCallback;
import net.fabricmc.fabric.api.entity.event.v1.ServerLivingEntityEvents;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerEntityEvents;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerLifecycleEvents;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerTickEvents;
import net.fabricmc.fabric.api.networking.v1.ServerPlayConnectionEvents;
import net.fabricmc.fabric.api.resource.ResourceManagerHelper;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.packs.PackType;
import net.minecraft.server.packs.resources.ResourceManager;
import net.minecraft.util.profiling.ProfilerFiller;


public final class ServerEventHandler {

  private ServerEventHandler() {
  }

  public static void register() {
    if (FeatureToggle.SPAWN.isEnabled()) {
      SpawnPresetLoader loader = new SpawnPresetLoader();
      ResourceManagerHelper.get(PackType.SERVER_DATA).registerReloadListener(
        new net.fabricmc.fabric.api.resource.IdentifiableResourceReloadListener() {
          @Override
          public ResourceLocation getFabricId() {
            return new ResourceLocation(Constants.MOD_ID, "spawn_presets");
          }

          @Override
          public CompletableFuture<Void> reload(
            PreparationBarrier barrier, ResourceManager manager,
            ProfilerFiller preparationsProfiler, ProfilerFiller reloadProfiler,
            java.util.concurrent.Executor backgroundExecutor,
            java.util.concurrent.Executor gameExecutor) {
            return loader.reload(barrier, manager, preparationsProfiler, reloadProfiler,
              backgroundExecutor, gameExecutor);
          }
        });
    }

    ServerLifecycleEvents.SERVER_STARTING.register(server -> {
      CommonServerEventHandler.handleServerAboutToStart(server);
    });

    ServerLifecycleEvents.SERVER_STARTED.register(server -> {
      CommonServerEventHandler.handleServerStarting(server);
      CommonServerEventHandler.handleServerStarted();
    });

    ServerLifecycleEvents.SERVER_STOPPING.register(CommonServerEventHandler::handleServerStopping);

    ServerTickEvents.END_SERVER_TICK.register(
      server -> CommonServerEventHandler.handleServerTick());

    ServerPlayConnectionEvents.JOIN.register((handler, sender, server) ->
      CommonServerEventHandler.handlePlayerLoggedIn(handler.player));

    ServerPlayConnectionEvents.DISCONNECT.register((handler, server) ->
      CommonServerEventHandler.handlePlayerLoggedOut(handler.player));

    ServerEntityEvents.ENTITY_LOAD.register((entity, level) -> {
      if (CommonEntityEventHandler.handleEntityJoinLevel(entity, level)) {
        entity.discard();
      }
    });

    ServerEntityEvents.ENTITY_UNLOAD.register((entity, level) ->
      CommonEntityEventHandler.handleEntityLeaveLevel(entity, level));

    ServerLivingEntityEvents.AFTER_DEATH.register(
      (entity, damageSource) -> CommonEntityEventHandler.handleLivingDeath(entity));

    CommandRegistrationCallback.EVENT.register(
      (dispatcher, registryAccess, environment) -> CommandManager.registerCommands(dispatcher));
  }
}
