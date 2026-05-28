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

package de.markusbordihn.adaptiveperformancetweaks.core.feature;

import static org.junit.jupiter.api.Assertions.assertEquals;

import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadDispatcher;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import org.junit.jupiter.api.Test;

class FeatureRegistryTest {

  @SuppressWarnings("unchecked")
  private static List<Consumer<?>> readListeners() throws Exception {
    Field field = ServerLoadDispatcher.class.getDeclaredField("listeners");
    field.setAccessible(true);
    return (List<Consumer<?>>) field.get(null);
  }

  @Test
  void registerCommonAddsAllLoadListenersRegardlessOfFeatureState() throws Exception {
    Map<FeatureToggle, Boolean> previousStates = new EnumMap<>(FeatureToggle.class);
    for (FeatureToggle featureToggle : FeatureToggle.values()) {
      if (featureToggle != FeatureToggle.CORE) {
        previousStates.put(featureToggle, featureToggle.isEnabled());
      }
    }

    List<Consumer<?>> listeners = readListeners();
    List<Consumer<?>> originalListeners = new ArrayList<>(listeners);
    try {
      for (FeatureToggle featureToggle : previousStates.keySet()) {
        featureToggle.setEnabled(false);
      }

      listeners.clear();
      FeatureRegistry.registerCommon();

      assertEquals(7, listeners.size());
    } finally {
      listeners.clear();
      listeners.addAll(originalListeners);
      previousStates.forEach(FeatureToggle::setEnabled);
    }
  }
}
