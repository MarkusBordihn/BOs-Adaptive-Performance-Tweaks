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

import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoad;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerManager;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.server.MinecraftServer;

public class StatusCommand extends CustomCommand {

  private static final StatusCommand command = new StatusCommand();

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("status").requires(source -> source.hasPermission(2)).executes(command);
  }

  @Override
  public int run(CommandContext<CommandSourceStack> context) {
    sendFeedback(context, String.format("=== %s Status ===", Constants.MOD_NAME));
    MinecraftServer server = ServerManager.getMinecraftServer();
    int playerCount = server != null ? server.getPlayerList().getPlayerCount() : 0;
    sendFeedback(
      context,
      String.format(
        "Server Load: %s (avg. %.1fms) | Players: %d",
        ServerLoad.getCurrentServerLoad(), ServerLoad.getAvgTickTime(), playerCount));

    sendFeedback(context, "--- Features ---");
    for (FeatureToggle toggle : FeatureToggle.values()) {
      String state = toggle.isEnabled() ? "ON" : "off";
      sendFeedback(context, String.format("  %-32s [%s]", toggle.getId(), state));
    }

    return 0;
  }
}
