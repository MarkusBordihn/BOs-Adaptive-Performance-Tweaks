/**
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

package de.markusbordihn.adaptiveperformancetweaks;

import java.util.Map;
import org.apache.commons.lang3.mutable.MutableInt;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import de.markusbordihn.adaptiveperformancetweaks.config.CommonConfig;
import net.minecraft.entity.Entity;

public abstract class Manager {

  protected Manager() {}

  public static final CommonConfig.Config COMMON = CommonConfig.COMMON;

  public static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public static void incrementCounter(Map<String, MutableInt> counterMap, String name) {
    MutableInt counter = counterMap.get(name);
    if (counter == null) {
      counterMap.put(name, new MutableInt());
    } else {
      counter.increment();
    }
  }

  public static void decrementCounter(Map<String, MutableInt> counterMap, Entity entity) {
    String entityName = entity.getEntityString();
    decrementCounter(counterMap, entityName);
  }

  public static void decrementCounter(Map<String, MutableInt> counterMap, String name) {
    MutableInt counter = counterMap.get(name);
    if (counter == null) {
      counterMap.put(name, new MutableInt());
    } else if (counter.getValue() > 0) {
      counter.decrement();
    } else {
      counter.setValue(0);
    }
  }

  public static Integer getCounter(Map<String, MutableInt> counterMap, String name) {
    MutableInt counter = counterMap.get(name);
    if (counter == null) {
      return 0;
    }
    return counter.getValue();
  }

}
