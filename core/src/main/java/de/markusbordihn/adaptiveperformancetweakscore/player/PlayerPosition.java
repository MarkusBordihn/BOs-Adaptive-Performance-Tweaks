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

package de.markusbordihn.adaptiveperformancetweakscore.player;

import de.markusbordihn.adaptiveperformancetweakscore.viewarea.ViewArea;
import java.util.UUID;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;

public class PlayerPosition {

  private static final String NETHER = "minecraft:the_nether";
  private static final String THE_END = "minecraft:the_end";
  private static final int CHUNK_SIZE = 16;

  private final ViewArea viewArea;
  private final UUID playerUUID;
  private final String playerName;
  private String levelName;

  public PlayerPosition(ServerPlayer player, int viewDistance, int simulationDistance) {
    this.playerName = player.getName().getString();
    this.playerUUID = player.getUUID();
    this.levelName = player.level.dimension().location().toString();

    // Calculate initial view area distance.
    int viewAreaDistance =
        getViewAreaDistance(player, this.levelName, viewDistance, simulationDistance);
    this.viewArea = new ViewArea(player, viewAreaDistance);
  }

  public static int getViewAreaDistance(
      ServerPlayer serverPlayer, String levelName, int viewDistance, int simulationDistance) {

    // Get players meta data
    boolean isNether = levelName.equals(NETHER);
    boolean isTheEnd = levelName.equals(THE_END);
    boolean canSeeSky =
        !isNether && serverPlayer.getLevel().canSeeSky(serverPlayer.blockPosition());
    boolean isUnderWater = !isNether && serverPlayer.isUnderWater();

    // Calculate view area distance in blocks based on surrounding factors like dimension, player
    // can see sky or is underwater.
    if ((!isNether && !isTheEnd && !canSeeSky) || isUnderWater) {
      return (Math.min(simulationDistance, viewDistance - 1)) * CHUNK_SIZE;
    }
    return viewDistance * CHUNK_SIZE;
  }

  public String getPlayerName() {
    return this.playerName;
  }

  public boolean update(
      ServerPlayer serverPlayer, String levelName, int viewDistance, int simulationDistance) {

    // Update level name and calculate view area distance.
    this.levelName = levelName;
    int viewAreaDistance =
        getViewAreaDistance(serverPlayer, levelName, viewDistance, simulationDistance);

    // Update view area
    return this.viewArea.update(serverPlayer, viewAreaDistance, levelName);
  }

  public boolean isInsidePlayerViewArea(String levelName) {
    return this.levelName.equals(levelName);
  }

  public boolean isInsidePlayerViewArea(String levelName, int x, int y, int z) {
    return this.levelName.equals(levelName) && this.viewArea.isInside(x, y, z);
  }

  public boolean isInsidePlayerViewArea(Entity entity, String levelName) {
    return this.viewArea.isInside(entity, levelName);
  }

  public String toString() {
    int simulationDistance = 4;
    int viewDistance = 8;
    return "PlayerPosition[player='"
        + this.playerName
        + "', uuid="
        + this.playerUUID
        + ", level='"
        + this.levelName
        + "', viewDistance="
        + viewDistance
        + ", simulationDistance="
        + simulationDistance
        + ", viewArea="
        + this.viewArea.toString()
        + "]";
  }
}
