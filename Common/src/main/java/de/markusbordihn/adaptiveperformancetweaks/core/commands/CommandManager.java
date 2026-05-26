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

package de.markusbordihn.adaptiveperformancetweaks.core.commands;

import com.mojang.brigadier.CommandDispatcher;
import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerManager;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.server.MinecraftServer;
import net.minecraft.world.level.GameRules;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class CommandManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private CommandManager() {
  }

  public static void registerCommands(CommandDispatcher<CommandSourceStack> dispatcher) {
    log.debug("{}Registering /{} commands ...", Constants.LOG_REGISTER_PREFIX,
      Constants.MOD_COMMAND);
    dispatcher.register(
      Commands.literal(Constants.MOD_COMMAND)
        .then(DebugCommand.register())
        .then(EntityCommand.register())
        .then(FeatureCommand.register())
        .then(KillCommand.register())
        .then(LoadCommand.register())
        .then(PlayerPositionCommand.register())
        .then(ReloadCommand.register())
        .then(BenchmarkCommand.register())
        .then(StatsCommand.register())
        .then(StatusCommand.register()));
  }

  public static void executeUserCommand(String command) {
    MinecraftServer minecraftServer = ServerManager.getMinecraftServer();
    if (minecraftServer == null) {
      log.warn("Unable to execute user command '{}': server not available.", command);
      return;
    }

    log.debug("Executing user command: {}", command);
    minecraftServer.getCommands().performPrefixedCommand(
      minecraftServer.createCommandSourceStack(), command);
  }

  public static void executeGameRuleCommand(GameRules.Key<?> gameRule, int value) {
    executeGameRuleCommand(gameRule, String.valueOf(value));
  }

  public static void executeGameRuleCommand(GameRules.Key<?> gameRule, boolean value) {
    executeGameRuleCommand(gameRule, value ? "true" : "false");
  }

  public static void executeGameRuleCommand(GameRules.Key<?> gameRule, String value) {
    MinecraftServer minecraftServer = ServerManager.getMinecraftServer();
    if (minecraftServer == null || gameRule == null || value == null || value.isEmpty()) {
      return;
    }
    String command = String.format("gamerule %s %s", gameRule.getId(), value);
    Commands commands = minecraftServer.getCommands();
    commands.performCommand(
      commands.getDispatcher()
        .parse(command, minecraftServer.createCommandSourceStack().withSuppressedOutput()),
      command);
  }
}
