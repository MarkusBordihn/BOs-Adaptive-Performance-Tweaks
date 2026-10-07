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
import de.markusbordihn.adaptiveperformancetweaks.core.diagnostics.TrackingDiagnostics;
import de.markusbordihn.adaptiveperformancetweaks.core.entity.CoreEntityManager;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DiagnosticsCommand extends CustomCommand {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final DiagnosticsCommand command = new DiagnosticsCommand();

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("diagnostics").requires(source -> source.hasPermission(2))
      .executes(command)
      .then(Commands.literal("verify").executes(context -> {
        CoreEntityManager.verifyTrackedEntities();
        sendReport(context);
        return 0;
      }));
  }

  private static void sendReport(CommandContext<CommandSourceStack> context) {
    for (String line : TrackingDiagnostics.createReportLines()) {
      log.info("[Diagnostics] {}", line);
      sendFeedback(context, line);
    }
  }

  @Override
  public int run(CommandContext<CommandSourceStack> context) {
    sendReport(context);
    return 0;
  }
}
