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

import java.lang.Thread.State;
import java.lang.management.ManagementFactory;
import java.lang.management.ThreadInfo;
import java.lang.management.ThreadMXBean;

public class Thread {

  private StackTraceElement[] stackTrace;
  private State threadState;
  private String threadName;
  private ThreadMXBean threadMXBean = ManagementFactory.getThreadMXBean();
  private long cpuTime;
  private long threadId;

  public Thread(ThreadInfo threadInfo) {
    this.cpuTime = threadMXBean.getThreadCpuTime(threadInfo.getThreadId());
    this.stackTrace = threadInfo.getStackTrace();
    this.threadId = threadInfo.getThreadId();
    this.threadName = threadInfo.getThreadName();
    this.threadState = threadInfo.getThreadState();
  }

  public long getId() {
    return this.threadId;
  }

  public String getName() {
    return this.threadName;
  }

  public State getState() {
    return this.threadState;
  }

  public long getCPUTime() {
    return this.cpuTime;
  }

  public StackTraceElement[] getStackTrace() {
    return this.stackTrace;
  }

  public String getTrackName() {
    if (this.stackTrace.length > 1) {
      return String.format("%s (%s.%s)", this.threadName, this.stackTrace[1].getClassName(), this.stackTrace[1].getMethodName());
    }
    return this.threadName;
  }

  public String toString() {
    return String.format("[%s:%s] %s %sms", getId(), getState(), getTrackName(),
        getCPUTime() / 1000000);
  }

}
