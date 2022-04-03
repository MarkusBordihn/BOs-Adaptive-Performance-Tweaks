/**
 * Copyright 2022 Markus Bordihn
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

package de.markusbordihn.adaptiveperformancetweakscore.dimension;

import java.util.ArrayList;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;

import net.minecraftforge.event.server.ServerStartedEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;

import de.markusbordihn.adaptiveperformancetweakscore.Constants;

@EventBusSubscriber
public class DimensionManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static List<String> dimensionList = new ArrayList<>();

  protected DimensionManager() {}

  @SubscribeEvent
  public static void handleServerStartedEvent(ServerStartedEvent event) {
    MinecraftServer server = event.getServer();
    if (server != null) {
      // Mapping names to server level for easier access.
      for (ServerLevel serverLevel : server.getAllLevels()) {
        String dimensionLocation = serverLevel.dimension().location().toString();
        log.info("Found dimension {}", dimensionLocation);
        if (!dimensionList.contains(dimensionLocation)) {
          dimensionList.add(dimensionLocation);
        }
      }
    }
  }

  public static List<String> getDimensionList() {
    return dimensionList;
  }

}
