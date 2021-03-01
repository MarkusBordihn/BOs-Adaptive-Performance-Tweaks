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

package de.markusbordihn.adaptiveperformancetweaks.gamerules;

import net.minecraft.world.GameRules;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.event.server.FMLServerStartingEvent;
import net.minecraftforge.fml.server.ServerLifecycleHooks;

import de.markusbordihn.adaptiveperformancetweaks.Manager;
import de.markusbordihn.adaptiveperformancetweaks.commands.CommandManager;
import de.markusbordihn.adaptiveperformancetweaks.server.ServerLoadEvent;

@EventBusSubscriber
public class GameRuleManager extends Manager {

  private static int defaultRandomTickSpeed = 3;
  private static int defaultMaxEntityCramming = 24;
  private static GameRules gameRules;

  @SubscribeEvent
  public static void handleServerStartingEvent(FMLServerStartingEvent event) {
    gameRules = ServerLifecycleHooks.getCurrentServer().getGameRules();
    int configRandomTickSpeed = gameRules.getInt(GameRules.RANDOM_TICK_SPEED);
    if (configRandomTickSpeed > defaultRandomTickSpeed) {
      defaultRandomTickSpeed = configRandomTickSpeed;
    }
    int configMaxEntityCramming = gameRules.getInt(GameRules.MAX_ENTITY_CRAMMING);
    if (configMaxEntityCramming > defaultMaxEntityCramming) {
      defaultMaxEntityCramming = configMaxEntityCramming;
    }
    log.info("Random Tick Speed will be optimized between {} and {}", 1, defaultRandomTickSpeed);
    log.info("Max Entity Cramming will be optimized between {} and {}", 1,
        defaultMaxEntityCramming);
  }

  @SubscribeEvent
  public static void handleServerLoadEvent(ServerLoadEvent event) {
    gameRules = ServerLifecycleHooks.getCurrentServer().getGameRules();
    if (event.hasHighServerLoad()) {
      decreaseRandomTickSpeed();
      decreaseMaxEntityCramming();
    } else if (event.hasLowServerLoad()) {
      increaseRandomTickSpeed();
      increaseMaxEntityCramming();
    }
  }

  public static void decreaseRandomTickSpeed() {
    setRandomTickSpeed(gameRules.getInt(GameRules.RANDOM_TICK_SPEED) - 1);
  }

  public static void increaseRandomTickSpeed() {
    setRandomTickSpeed(gameRules.getInt(GameRules.RANDOM_TICK_SPEED) + 1);
  }

  public static void setRandomTickSpeed(int tickSpeed) {
    int currentTickSpeed = gameRules.getInt(GameRules.RANDOM_TICK_SPEED);
    if (tickSpeed < 1) {
      tickSpeed = 1;
    } else if (tickSpeed > defaultRandomTickSpeed) {
      tickSpeed = defaultRandomTickSpeed;
    }
    if (currentTickSpeed != tickSpeed) {
      log.debug("Changing randomTickSpeed from {} to {}", currentTickSpeed, tickSpeed);
      CommandManager.executeServerCommand(String.format("gamerule randomTickSpeed %s", tickSpeed));
    }
  }

  public static void decreaseMaxEntityCramming() {
    setMaxEntityCramming(gameRules.getInt(GameRules.MAX_ENTITY_CRAMMING) - 1);
  }

  public static void increaseMaxEntityCramming() {
    setMaxEntityCramming(gameRules.getInt(GameRules.MAX_ENTITY_CRAMMING) + 1);
  }

  public static void setMaxEntityCramming(int maxEntityCramming) {
    int currentMaxEntityCramming = gameRules.getInt(GameRules.MAX_ENTITY_CRAMMING);
    if (maxEntityCramming < 1) {
      maxEntityCramming = 1;
    } else if (maxEntityCramming > defaultMaxEntityCramming) {
      maxEntityCramming = defaultMaxEntityCramming;
    }
    if (currentMaxEntityCramming != maxEntityCramming) {
      log.debug("Changing maxEntityCramming from {} to {}", currentMaxEntityCramming,
          maxEntityCramming);
      CommandManager
          .executeServerCommand(String.format("gamerule maxEntityCramming %s", maxEntityCramming));
    }
  }
}
