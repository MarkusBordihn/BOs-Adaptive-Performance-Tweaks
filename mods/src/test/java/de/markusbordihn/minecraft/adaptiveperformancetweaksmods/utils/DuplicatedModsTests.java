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

package de.markusbordihn.minecraft.adaptiveperformancetweaksmods.utils;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

public class DuplicatedModsTests {

  private final String testFilePath = "src/test/resources/testfile/duplicates";
  private final File testFiles = new File(testFilePath);

  private final String twoTestFilePath = "src/test/resources/testfile/two_duplicates";
  private final File twoTestFiles = new File(twoTestFilePath);

  @Test
  void testFindLatestMod() {
    System.out.printf("Test Mod Files: %s\n", testFiles);
    List<File> testFileList = new ArrayList<>();
    for (File testFile : testFiles.listFiles()) {
      testFileList.add(testFile);
    }
    File result = DuplicatedMods.findLatestMod(testFileList);
    assertEquals("adaptive_performance_tweaks_items_1.18.1-2.20.9.jar", result.getName());
  }

  @Test
  void testSearchDuplicatedMods() {

    System.out.printf("Two Test Mod Files: %s\n", twoTestFiles);
    int two_result = DuplicatedMods.searchDuplicatedMods(twoTestFiles, ".jar", true);
    assertTrue(two_result > 0);

    System.out.printf("Test Mod Files: %s\n", testFiles);
    int result = DuplicatedMods.searchDuplicatedMods(testFiles, ".jar", true);
    assertTrue(result > 0);
  }
}
