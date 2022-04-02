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

package de.markusbordihn.adaptiveperformancetweaksplayer.player;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.server.MinecraftServer;
import net.minecraft.server.players.PlayerList;

import net.minecraftforge.event.server.ServerAboutToStartEvent;
import net.minecraftforge.event.server.ServerStartingEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.server.ServerLifecycleHooks;
import de.markusbordihn.adaptiveperformancetweakscore.CoreConstants;
import de.markusbordihn.adaptiveperformancetweakscore.message.WarnMessages;
import de.markusbordihn.adaptiveperformancetweakscore.server.ServerLoadEvent;
import de.markusbordihn.adaptiveperformancetweaksplayer.Constants;
import de.markusbordihn.adaptiveperformancetweaksplayer.config.CommonConfig;

@EventBusSubscriber
public class PlayerSimulationDistance {

  private static final CommonConfig.Config COMMON = CommonConfig.COMMON;
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static boolean optimizeSimulationDistance = COMMON.optimizeSimulationDistance.get();
  private static int simulationDistanceDefault = COMMON.simulationDistanceDefault.get();
  private static int simulationDistanceMax = COMMON.simulationDistanceMax.get();
  private static int simulationDistanceMin = COMMON.simulationDistanceMin.get();
  private static int simulationDistanceTimeBetweenUpdates =
      COMMON.simulationDistanceTimeBetweenUpdates.get();

  private static long lastUpdateTime = System.currentTimeMillis();
  private static int timeBetweenUpdates = 10000;

  protected PlayerSimulationDistance() {}

  @SubscribeEvent
  public static void onServerAboutToStartEvent(ServerAboutToStartEvent event) {
    optimizeSimulationDistance = COMMON.optimizeSimulationDistance.get();
    simulationDistanceDefault = COMMON.simulationDistanceDefault.get();
    simulationDistanceMin = COMMON.simulationDistanceMin.get();
    simulationDistanceMax = COMMON.simulationDistanceMax.get();
    simulationDistanceTimeBetweenUpdates = COMMON.simulationDistanceTimeBetweenUpdates.get();
    timeBetweenUpdates = simulationDistanceTimeBetweenUpdates * 1000;
  }

  @SubscribeEvent
  public static void onServerStarting(ServerStartingEvent event) {
    if (!optimizeSimulationDistance) {
      return;
    }
    log.info(
        "Simulation distance will be optimized between {} and {} with {} as default and {} sec delay between updates.",
        simulationDistanceMin, simulationDistanceMax, simulationDistanceDefault,
        simulationDistanceTimeBetweenUpdates);
    setSimulationDistance(simulationDistanceMin);

    if (CoreConstants.DYNVIEW_LOADED) {
      log.warn(WarnMessages.conflictingFeaturesModWarning(CoreConstants.DYNVIEW_NAME,
          "optimizing the player simulation distance"));
    }
  }

  @SubscribeEvent
  public static void handleServerLoadEvent(ServerLoadEvent event) {
    if (!optimizeSimulationDistance) {
      return;
    }

    // To make the updates less noticeable we are delaying increasing updates.
    boolean shouldOptimizeViewDistance =
        System.currentTimeMillis() - lastUpdateTime >= timeBetweenUpdates;

    boolean hasUpdated = false;
    int numOfPlayers = ServerLifecycleHooks.getCurrentServer().getPlayerList().getPlayerCount();

    // Decrease the simulation distance immediately, but increase the simulation distance with an
    // delay.
    if (event.hasHighServerLoad()) {
      hasUpdated = decreaseSimulationDistance();
    } else if (event.hasNormalServerLoad() && numOfPlayers >= 1) {
      hasUpdated = decreaseSimulationDistance();
    } else if (shouldOptimizeViewDistance && event.hasNormalServerLoad() && numOfPlayers == 1) {
      hasUpdated = increaseSimulationDistance();
    } else if (shouldOptimizeViewDistance && event.hasLowServerLoad() && numOfPlayers >= 1) {
      hasUpdated = increaseSimulationDistance();
    }

    // Only update the last update time if we really updated something.
    if (hasUpdated) {
      lastUpdateTime = System.currentTimeMillis();
    }
  }

  public static boolean setSimulationDistance(int simulationDistance) {
    if (simulationDistance > simulationDistanceMax) {
      simulationDistance = simulationDistanceMax;
    } else if (simulationDistance < simulationDistanceMin) {
      simulationDistance = simulationDistanceMin;
    }
    MinecraftServer minecraftServer = ServerLifecycleHooks.getCurrentServer();
    PlayerList playerList = minecraftServer.getPlayerList();
    if (!playerList.getPlayers().isEmpty()
        && playerList.getSimulationDistance() != simulationDistance) {
      log.debug("Changing server simulation distance from {} to {} for {} players.",
          playerList.getSimulationDistance(), simulationDistance, playerList.getPlayers().size());
      playerList.setSimulationDistance(simulationDistance);
      return true;
    }
    return false;
  }

  public static boolean increaseSimulationDistance(int factor) {
    return setSimulationDistance(getSimulationDistance() + factor);
  }

  public static boolean decreaseSimulationDistance(int factor) {
    return setSimulationDistance(getSimulationDistance() - factor);
  }

  public static boolean increaseSimulationDistance() {
    return increaseSimulationDistance(1);
  }

  public static boolean decreaseSimulationDistance() {
    return decreaseSimulationDistance(1);
  }

  public static int getSimulationDistance() {
    final MinecraftServer minecraftServer = ServerLifecycleHooks.getCurrentServer();
    PlayerList playerList = minecraftServer.getPlayerList();
    return playerList.getSimulationDistance() > 0 ? playerList.getSimulationDistance()
        : simulationDistanceDefault;
  }

  public static int getMaxSimulationDistance() {
    return simulationDistanceMax;
  }

  public static int getMinSimulationDistance() {
    return simulationDistanceMin;
  }
}
