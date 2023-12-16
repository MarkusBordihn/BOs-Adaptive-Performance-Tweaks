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

package de.markusbordihn.adaptiveperformancetweaksplayer.config;

import de.markusbordihn.adaptiveperformancetweakscore.CoreConstants;
import de.markusbordihn.adaptiveperformancetweaksplayer.Constants;
import java.util.ArrayList;
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
public final class CommonConfig {

  public static final ForgeConfigSpec commonSpec;
  public static final Config COMMON;
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  static {
    com.electronwill.nightconfig.core.Config.setInsertionOrderPreserved(true);
    final Pair<Config, ForgeConfigSpec> specPair =
        new ForgeConfigSpec.Builder().configure(Config::new);
    commonSpec = specPair.getRight();
    COMMON = specPair.getLeft();
    log.info("Registering {} common config ...", Constants.MOD_NAME);
    try {
      FileUtils.getOrCreateDirectory(
          FMLPaths.CONFIGDIR.get().resolve(CoreConstants.CONFIG_ID), CoreConstants.CONFIG_ID);
    } catch (Exception exception) {
      log.error("There was an error, creating the directory:", exception);
    }
    ModLoadingContext.get()
        .registerConfig(
            ModConfig.Type.COMMON, commonSpec, CoreConstants.CONFIG_ID_PREFIX + "player.toml");
  }

  private CommonConfig() {}

  public static class Config {

    public final ForgeConfigSpec.BooleanValue optimizePlayerLogin;
    public final ForgeConfigSpec.IntValue playerLoginValidationTimeout;

    public final ForgeConfigSpec.BooleanValue protectPlayerDuringLogin;
    public final ForgeConfigSpec.BooleanValue protectPlayerDuringLoginLogging;

    public final ForgeConfigSpec.BooleanValue enableChildPlayerProtection;
    public final ForgeConfigSpec.ConfigValue<List<String>> childPlayerProtectionList;
    public final ForgeConfigSpec.BooleanValue childPlayerInvisible;
    public final ForgeConfigSpec.BooleanValue childPlayerInvulnerable;
    public final ForgeConfigSpec.IntValue childPlayerHurtDamageReduction;
    public final ForgeConfigSpec.IntValue childPlayerAttackDamageIncrease;

    Config(ForgeConfigSpec.Builder builder) {
      builder.comment(Constants.MOD_NAME);

      builder.push("Player Login");
      optimizePlayerLogin =
          builder
              .comment("Enable/Disable optimization which are happening during player login.")
              .define("optimizePlayerLogin", true);
      playerLoginValidationTimeout =
          builder
              .comment("Timeout in seconds for the player validation.")
              .defineInRange("playerLoginValidationTimeout", 90, 16, 300);
      builder.pop();

      builder.push("Player Protection");
      protectPlayerDuringLogin =
          builder
              .comment("Protect player during login within the validation timeout.")
              .define("protectPlayerDuringLogin", true);
      protectPlayerDuringLoginLogging =
          builder
              .comment("Enable/Disable player protection logging with login time.")
              .define("protectPlayerDuringLoginLogging", true);
      builder.pop();

      builder.push("Child Player Protection");
      enableChildPlayerProtection =
          builder
              .comment("Protect child player and give them a more enjoyable play experience.")
              .define("enableChildPlayerProtection", true);
      childPlayerProtectionList =
          builder
              .comment("List of child player username for the child player protection.")
              .define("childPlayerProtectionList", new ArrayList<>(List.of("")));
      childPlayerInvisible =
          builder
              .comment("Sets the child player to invisible.")
              .define("childPlayerInvisible", true);
      childPlayerInvulnerable =
          builder
              .comment("Sets the child player to invulnerable.")
              .define("childPlayerInvulnerable", false);
      childPlayerHurtDamageReduction =
          builder
              .comment(
                  "Reduces the hurt damage to a child player by the amount of % (0 = disabled).")
              .defineInRange("childPlayerHurtDamageReduction", 50, 0, 100);
      childPlayerAttackDamageIncrease =
          builder
              .comment(
                  "Increase the attack damage of a child player by the amount of % (0 = disabled).")
              .defineInRange("childPlayerAttackDamageIncrease", 50, 0, 1000);
      builder.pop();
    }
  }
}
