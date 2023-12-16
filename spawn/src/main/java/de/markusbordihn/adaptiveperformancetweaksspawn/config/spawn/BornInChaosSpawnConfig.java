/*
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

package de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn;

import de.markusbordihn.adaptiveperformancetweakscore.CoreConstants;
import de.markusbordihn.adaptiveperformancetweaksspawn.Constants;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import net.minecraftforge.common.ForgeConfigSpec;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.config.ModConfig;
import net.minecraftforge.fml.loading.FMLPaths;
import net.minecraftforge.fml.loading.FileUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@EventBusSubscriber(bus = EventBusSubscriber.Bus.MOD)
public final class BornInChaosSpawnConfig {

  public static final ForgeConfigSpec commonSpec;
  public static final Config COMMON;
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  static {
    com.electronwill.nightconfig.core.Config.setInsertionOrderPreserved(true);
    final Pair<Config, ForgeConfigSpec> specPair =
        new ForgeConfigSpec.Builder().configure(Config::new);
    commonSpec = specPair.getRight();
    COMMON = specPair.getLeft();
    log.info(
        "Registering {} {} Spawn config ...", Constants.MOD_NAME, CoreConstants.BORN_IN_CHAOS_NAME);
    try {
      FileUtils.getOrCreateDirectory(
          FMLPaths.CONFIGDIR.get().resolve(CoreConstants.CONFIG_ID), CoreConstants.CONFIG_ID);
    } catch (Exception exception) {
      log.error("There was an error, creating the directory:", exception);
    }
    ModLoadingContext.get()
        .registerConfig(
            ModConfig.Type.COMMON,
            commonSpec,
            CoreConstants.CONFIG_ID_PREFIX + "/spawn/BornInChaosSpawn.toml");
  }

  private BornInChaosSpawnConfig() {}

  public static class Config {

    public final ForgeConfigSpec.BooleanValue enabled;
    public final ForgeConfigSpec.ConfigValue<String> id;

    public final ForgeConfigSpec.IntValue hostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue hostileMobsPerWorld;
    public final ForgeConfigSpec.IntValue hostileMobsPerServer;
    public final ForgeConfigSpec.ConfigValue<List<String>> hostileMobsList;

    Config(ForgeConfigSpec.Builder builder) {
      builder.comment(Constants.MOD_NAME);

      builder.push("Born in Chaos Spawn Config");
      enabled = builder.define("Enabled", true);
      id = builder.define("Id", CoreConstants.BORN_IN_CHAOS_MOD);

      hostileMobsPerPlayer =
          builder
              .comment("e.g. mobs which will always attack yon ...")
              .defineInRange("HostileMobsPerPlayer", 16, 1, 64);
      hostileMobsPerWorld = builder.defineInRange("HostileMobsPerWorld", 32, 1, 512);
      hostileMobsPerServer = builder.defineInRange("HostileMobsPerServer", 320, 1, 1024);
      hostileMobsList =
          builder
              .comment(Constants.CONFIG_LIST_HOSTILE_MOBS)
              .define(
                  "HostileMobsList",
                  new ArrayList<>(
                      Arrays.asList(
                          // @formatter:off
                          "born_in_chaos_v1:baby_s_2",
                          "born_in_chaos_v1:baby_skeleton",
                          "born_in_chaos_v1:bone_imp",
                          "born_in_chaos_v1:bone_imp_2",
                          "born_in_chaos_v1:bonescaller",
                          "born_in_chaos_v1:dark_vortex",
                          "born_in_chaos_v1:decaying_zombie",
                          "born_in_chaos_v1:decrepit_skeleton",
                          "born_in_chaos_v1:diamond_thermite",
                          "born_in_chaos_v1:dread_hound",
                          "born_in_chaos_v1:fallen_chaos_knight",
                          "born_in_chaos_v1:firelight",
                          "born_in_chaos_v1:mr_pumpkin",
                          "born_in_chaos_v1:nightmare_stalker",
                          "born_in_chaos_v1:phantom_creeper",
                          "born_in_chaos_v1:pumpkin_spirit",
                          "born_in_chaos_v1:restless_spirit",
                          "born_in_chaos_v1:scarletpersecutor",
                          "born_in_chaos_v1:seared_spirit",
                          "born_in_chaos_v1:shy_spirit",
                          "born_in_chaos_v1:skeleton_bomb",
                          "born_in_chaos_v1:skeleton_thrasher",
                          "born_in_chaos_v1:spirit_guide_assistant",
                          "born_in_chaos_v1:spiritual_guide",
                          "born_in_chaos_v1:supreme_bonecaller",
                          "born_in_chaos_v1:wither_strider",
                          "born_in_chaos_v1:zombie_clown",
                          "born_in_chaos_v1:zombiesina_barrel",
                          "born_in_chaos_v1:zombieswith_door"
                          // @formatter:on
                          )));

      builder.pop();
    }
  }
}
