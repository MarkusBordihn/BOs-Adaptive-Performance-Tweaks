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
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureRegistry;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;

public class ReloadCommand extends CustomCommand {

  private static final ReloadCommand command = new ReloadCommand();

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("reload").requires(source -> source.hasPermission(2)).executes(command);
  }

  @Override
  public int run(CommandContext<CommandSourceStack> context) throws CommandSyntaxException {
    Map<FeatureToggle, Boolean> previousStates = new EnumMap<>(FeatureToggle.class);
    for (FeatureToggle toggle : FeatureToggle.values()) {
      previousStates.put(toggle, toggle.isEnabled());
    }

    FeatureRegistry.reloadConfigs();

    sendFeedback(context, "APTweaks configuration reloaded.");

    boolean restartRequired = false;
    for (FeatureToggle toggle : FeatureToggle.values()) {
      boolean wasEnabled = previousStates.get(toggle);
      boolean isNowEnabled = toggle.isEnabled();
      if (wasEnabled != isNowEnabled) {
        sendFeedback(
          context,
          String.format(
            "  [!] %s: %s -> %s (restart required to take effect)",
            toggle.getId(),
            wasEnabled ? "ON" : "off",
            isNowEnabled ? "ON" : "off"));
        restartRequired = true;
      }
    }

    if (restartRequired) {
      sendFeedback(
        context,
        "Note: Feature enable/disable changes require a server restart to fully take effect.");
    }

    return SINGLE_SUCCESS;
  }
}
