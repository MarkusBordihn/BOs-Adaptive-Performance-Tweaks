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

package de.markusbordihn.adaptiveperformancetweaks.system;

import java.lang.management.MemoryMXBean;

public class MemoryInfo {

  private double init;
  private double used;
  private double max;
  private double committed;
  private static final int BYTES_TO_MEGABYTES = 1048576;

  public MemoryInfo(MemoryMXBean memoryMXBean) {
    this.init = memoryMXBean.getHeapMemoryUsage().getInit();
    this.used = memoryMXBean.getHeapMemoryUsage().getUsed();
    this.max = memoryMXBean.getHeapMemoryUsage().getMax();
    this.committed = memoryMXBean.getHeapMemoryUsage().getCommitted();
  }

  public double getInit() {
    return this.init / BYTES_TO_MEGABYTES;
  }

  public double getUsed() {
    return this.used / BYTES_TO_MEGABYTES;
  }

  public double getMax() {
    return this.max / BYTES_TO_MEGABYTES;
  }

  public double getCommitted() {
    return this.committed / BYTES_TO_MEGABYTES;
  }

}
