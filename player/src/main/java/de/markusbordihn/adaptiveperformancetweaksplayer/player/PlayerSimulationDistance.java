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

  protected PlayerSimulationDistance() {}

  @SubscribeEvent
  public static void onServerAboutToStartEvent(ServerAboutToStartEvent event) {
    optimizeSimulationDistance = COMMON.optimizeSimulationDistance.get();
    simulationDistanceDefault = COMMON.simulationDistanceDefault.get();
    simulationDistanceMin = COMMON.simulationDistanceMin.get();
    simulationDistanceMax = COMMON.simulationDistanceMax.get();
  }

  @SubscribeEvent
  public static void onServerStarting(ServerStartingEvent event) {
    if (!optimizeSimulationDistance) {
      return;
    }
    log.info("Simulation distance will be optimized between {} and {} with {} as default",
        simulationDistanceMin, simulationDistanceMax, simulationDistanceDefault);
    setSimulationDistance(simulationDistanceMin);
  }

  @SubscribeEvent
  public static void handleServerLoadEvent(ServerLoadEvent event) {
    if (!optimizeSimulationDistance) {
      return;
    }
    int numOfPlayers = ServerLifecycleHooks.getCurrentServer().getPlayerList().getPlayerCount();
    if (event.hasHighServerLoad()) {
      decreaseSimulationDistance();
    } else if (event.hasNormalServerLoad() && numOfPlayers >= 1) {
      decreaseSimulationDistance();
    } else if (event.hasNormalServerLoad() && numOfPlayers == 1) {
      increaseSimulationDistance();
    } else if (event.hasLowServerLoad() && numOfPlayers >= 1) {
      increaseSimulationDistance();
    }
  }

  public static void setSimulationDistance(int simulationDistance) {
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
    }
  }

  public static void increaseSimulationDistance() {
    setSimulationDistance(getSimulationDistance() + 1);
  }

  public static void decreaseSimulationDistance() {
    setSimulationDistance(getSimulationDistance() - 1);
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
