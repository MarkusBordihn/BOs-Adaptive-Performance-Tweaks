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

package de.markusbordihn.adaptiveperformancetweakscore.message;

public class WarnMessages {

  protected WarnMessages() {}

  public static String coreModWarning(String modName) {
    return String.format(
        "The mod %s use core modifications which could conflicting with this none-core mod. Do not report any issues with both mods enabled.",
        modName);
  }

  public static String conflictingFeaturesModWarning(String modName, String reason) {
    return String.format(
        "The mod %s %s, which could conflict with the feature of this mod. Don't use both optimizations together to avoid any side effects!",
        modName, reason);
  }

  public static String knownIssuesModWarning(String modName) {
    return String.format("There are known issue with the %s mod, please not report any issue!",
        modName);
  }

}
