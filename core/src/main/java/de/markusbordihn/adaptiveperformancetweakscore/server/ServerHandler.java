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

package de.markusbordihn.adaptiveperformancetweakscore.server;

import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.event.server.ServerAboutToStartEvent;
import net.minecraftforge.event.server.ServerStartedEvent;
import net.minecraftforge.event.server.ServerStartingEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;

import de.markusbordihn.adaptiveperformancetweakscore.Constants;

@EventBusSubscriber(Dist.DEDICATED_SERVER)
public class ServerHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static boolean showedServerAboutToStart = false;
  private static boolean showedServerStartingEvent = false;
  private static boolean showedServerStarted = false;

  private static long serverAboutToStartTime;
  private static long serverStartingTime;
  private static long serverStartedTime;

  protected ServerHandler() {}

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(ServerAboutToStartEvent event) {
    if (!showedServerAboutToStart) {
      RuntimeMXBean runtimeMXBean = ManagementFactory.getRuntimeMXBean();
      serverAboutToStartTime = System.currentTimeMillis();
      log.info("{} ⏲ Phase 1/3 - Server took about {} sec to load ...", Constants.LOG_PREFIX,
          (serverAboutToStartTime - runtimeMXBean.getStartTime()) / 1000f);
      showedServerAboutToStart = true;
    }
  }

  @SubscribeEvent
  public static void handleServerStartingEvent(ServerStartingEvent event) {
    if (!showedServerStartingEvent) {
      RuntimeMXBean runtimeMXBean = ManagementFactory.getRuntimeMXBean();
      serverStartingTime = System.currentTimeMillis();
      log.info("{} ⏲ Phase 2/3 - Server took about {} sec to starting (∆: {} secs)...",
          Constants.LOG_PREFIX, (serverStartingTime - runtimeMXBean.getStartTime()) / 1000f,
          (serverStartingTime - serverAboutToStartTime) / 1000f);
      showedServerStartingEvent = true;
    }
  }

  @SubscribeEvent
  public static void handleServerStartedEvent(ServerStartedEvent event) {
    if (!showedServerStarted) {
      RuntimeMXBean runtimeMXBean = ManagementFactory.getRuntimeMXBean();
      serverStartedTime = System.currentTimeMillis();
      log.info(
          "{} ⏲ Phase 3/3 - Server took about {} sec to be available and ready to play (∆: {} secs) ...",
          Constants.LOG_PREFIX, (serverStartedTime - runtimeMXBean.getStartTime()) / 1000f,
          (serverStartedTime - serverStartingTime) / 1000f);
      showedServerStarted = true;
    }
  }

  public static long getServerAboutToStartTime() {
    return serverAboutToStartTime;
  }

  public static long getServerStartingTime() {
    return serverStartingTime;
  }

  public static long getServerStartedTime() {
    return serverStartedTime;
  }

}
