/**
 * Copyright 2021 Markus Bordihn
 *
 * <p>Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * <p>The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * <p>THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package de.markusbordihn.adaptiveperformancetweaksitems.commands;

import com.mojang.brigadier.CommandDispatcher;
import de.markusbordihn.adaptiveperformancetweaksitems.Constants;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@EventBusSubscriber
public class CommandManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected CommandManager() {}

  @SubscribeEvent
  public static void handleRegisterCommandsEvent(RegisterCommandsEvent event) {
    log.info("Registering /aptweaks commands for {} ...", Constants.MOD_NAME);
    CommandDispatcher<CommandSourceStack> commandDispatcher = event.getDispatcher();
    commandDispatcher.register(
        Commands.literal(Constants.MOD_COMMAND)
            // @formatter:off
            .then(DebugCommand.register())
            .then(ItemsCommand.register())
        // @formatter:on
        );
  }
}
