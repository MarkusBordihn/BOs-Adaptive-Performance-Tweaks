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
import net.minecraftforge.fml.event.server.FMLServerAboutToStartEvent;
import net.minecraftforge.fml.event.server.FMLServerStartingEvent;
import net.minecraftforge.fml.server.ServerLifecycleHooks;

import de.markusbordihn.adaptiveperformancetweaks.Manager;
import de.markusbordihn.adaptiveperformancetweaks.commands.CommandManager;
import de.markusbordihn.adaptiveperformancetweaks.server.ServerLoadEvent;

@EventBusSubscriber
public class GameRuleManager extends Manager {

  private static GameRules gameRules;
  private static int maxEntityCramming = COMMON.maxEntityCramming.get();
  private static int randomTickSpeed = COMMON.randomTickSpeed.get();
  private static boolean gameruleEnabled = COMMON.gameruleEnabled.get();

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(FMLServerAboutToStartEvent event) {
    gameruleEnabled = COMMON.gameruleEnabled.get();
    maxEntityCramming = COMMON.maxEntityCramming.get();
    randomTickSpeed = COMMON.randomTickSpeed.get();
  }

  @SubscribeEvent
  public static void handleServerStartingEvent(FMLServerStartingEvent event) {
    gameRules = ServerLifecycleHooks.getCurrentServer().getGameRules();
    if (!gameruleEnabled) {
      return;
    }
    log.info("Random Tick Speed will be optimized between {} and {}", 1, randomTickSpeed);
    log.info("Max Entity Cramming will be optimized between {} and {}", 1, maxEntityCramming);
    if (gameRules.getInt(GameRules.RANDOM_TICK_SPEED) != randomTickSpeed) {
      setRandomTickSpeed(randomTickSpeed);
    }
    if (gameRules.getInt(GameRules.MAX_ENTITY_CRAMMING) != maxEntityCramming) {
      setMaxEntityCramming(maxEntityCramming);
    }
  }

  @SubscribeEvent
  public static void handleServerLoadEvent(ServerLoadEvent event) {
    if (!gameruleEnabled) {
      return;
    }
    gameRules = ServerLifecycleHooks.getCurrentServer().getGameRules();

    if (event.hasVeryHighServerLoad()) {
      decreaseMaxEntityCramming();
    }
    if (event.hasHighServerLoad()) {
      decreaseRandomTickSpeed();
    }
    if (event.hasLowServerLoad()) {
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
    } else if (tickSpeed > randomTickSpeed) {
      tickSpeed = randomTickSpeed;
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

  public static void setMaxEntityCramming(int maxEntity) {
    if (maxEntity < 1) {
      maxEntity = 1;
    } else if (maxEntity > maxEntityCramming) {
      maxEntity = maxEntityCramming;
    }
    int currentMaxEntityCramming = gameRules.getInt(GameRules.MAX_ENTITY_CRAMMING);
    if (currentMaxEntityCramming != maxEntityCramming) {
      log.debug("Changing maxEntityCramming from {} to {}", currentMaxEntityCramming,
      maxEntity);
      CommandManager
          .executeServerCommand(String.format("gamerule maxEntityCramming %s", maxEntity));
    }
  }
}
