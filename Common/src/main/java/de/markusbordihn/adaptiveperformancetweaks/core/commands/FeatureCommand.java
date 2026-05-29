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

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import de.markusbordihn.adaptiveperformancetweaks.core.config.CoreConfig;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.server.permissions.Permissions;

public class FeatureCommand extends CustomCommand {

  private static final FeatureCommand command = new FeatureCommand();

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("feature")
      .requires(cs -> cs.permissions().hasPermission(Permissions.COMMANDS_GAMEMASTER))
      .then(
        Commands.argument("id", StringArgumentType.word())
          .suggests(
            (context, builder) -> {
              for (FeatureToggle toggle : FeatureToggle.values()) {
                builder.suggest(toggle.getId());
              }
              return builder.buildFuture();
            })
          .then(Commands.argument("enabled", BoolArgumentType.bool()).executes(command)));
  }

  @Override
  public int run(CommandContext<CommandSourceStack> context) {
    String featureId = StringArgumentType.getString(context, "id");
    boolean enabled = BoolArgumentType.getBool(context, "enabled");

    FeatureToggle target = null;
    for (FeatureToggle toggle : FeatureToggle.values()) {
      if (toggle.getId().equals(featureId)) {
        target = toggle;
        break;
      }
    }

    if (target == null) {
      sendFeedback(context, "Unknown feature: " + featureId);
      return 1;
    }

    CoreConfig.setFeatureEnabled(target, enabled);
    sendFeedback(
      context, String.format("Feature '%s' %s.", featureId, enabled ? "enabled" : "disabled"));
    return 0;
  }
}
