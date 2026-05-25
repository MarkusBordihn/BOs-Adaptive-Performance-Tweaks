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

package de.markusbordihn.adaptiveperformancetweaks.gametest;

import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadEvent;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.feature.gamerules.GameRuleManager;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.MinecraftServer;
import net.minecraft.world.level.GameRules;

public final class GameRuleAdaptationTests {

  private GameRuleAdaptationTests() {}

  public static void testVeryHighLoadDisablesGameRules(GameTestHelper helper) {
    MinecraftServer server = helper.getLevel().getServer();
    GameRuleManager.handleServerStarting(server);
    GameRules rules = server.getGameRules();

    GameRuleManager.enablePatrolSpawning();
    GameRuleManager.enableRaids();
    GameRuleManager.enableInsomnia();

    GameRuleManager.handleServerLoadEvent(
        new ServerLoadEvent(ServerLoadLevel.VERY_HIGH, ServerLoadLevel.NORMAL, 200.0, 50.0));

    GameTestHelpers.assertTrue(
        helper,
        "patrolSpawning should be disabled under VERY_HIGH load",
        !rules.getBoolean(GameRules.RULE_DO_PATROL_SPAWNING));
    GameTestHelpers.assertTrue(
        helper,
        "raids should be disabled under VERY_HIGH load",
        rules.getBoolean(GameRules.RULE_DISABLE_RAIDS));
    GameTestHelpers.assertTrue(
        helper,
        "insomnia should be disabled under VERY_HIGH load",
        !rules.getBoolean(GameRules.RULE_DOINSOMNIA));

    GameRuleManager.enablePatrolSpawning();
    GameRuleManager.enableRaids();
    GameRuleManager.enableInsomnia();
    helper.succeed();
  }
}
