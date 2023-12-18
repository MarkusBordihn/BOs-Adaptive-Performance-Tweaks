/*
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

package de.markusbordihn.adaptiveperformancetweaksplayer;

import de.markusbordihn.adaptiveperformancetweakscore.debug.DebugManager;
import net.minecraftforge.fml.IExtensionPoint;
import net.minecraftforge.fml.IExtensionPoint.DisplayTest;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod;

@Mod(Constants.MOD_ID)
public class AdaptivePerformanceTweaksPlayer {

  public AdaptivePerformanceTweaksPlayer() {
    // Make sure the mod being absent on the other network side does not cause the client to display
    // the server as incompatible
    ModLoadingContext.get()
        .registerExtensionPoint(
            IExtensionPoint.DisplayTest.class,
            () ->
                new IExtensionPoint.DisplayTest(
                    () -> DisplayTest.IGNORESERVERONLY, (a, b) -> true));

    // Warn if debugging is enabled and automatically disable debug on prod for performance reasons.
    DebugManager.checkForDebugLogging(Constants.LOG_NAME);
  }
}
