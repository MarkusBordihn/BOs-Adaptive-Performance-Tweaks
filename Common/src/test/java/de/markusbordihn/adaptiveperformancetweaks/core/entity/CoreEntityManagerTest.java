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

package de.markusbordihn.adaptiveperformancetweaks.core.entity;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import de.markusbordihn.adaptiveperformancetweaks.feature.spawn.SpawnPreset;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.SharedConstants;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.monster.Zombie;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class CoreEntityManagerTest {

  @BeforeAll
  static void bootstrapMinecraft() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
    Bootstrap.validate();
  }

  private static void setExcludedNamespacesViaReflection(Set<String> value) throws Exception {
    Field field = CoreEntityManager.class.getDeclaredField("excludedModNamespaces");
    field.setAccessible(true);
    field.set(null, value);
  }

  @SuppressWarnings("unchecked")
  private static <T> T readStaticField(String fieldName) throws Exception {
    Field field = CoreEntityManager.class.getDeclaredField(fieldName);
    field.setAccessible(true);
    return (T) field.get(null);
  }

  private static void writeStaticField(String fieldName, Object value) throws Exception {
    Field field = CoreEntityManager.class.getDeclaredField(fieldName);
    field.setAccessible(true);
    field.set(null, value);
  }

  private static Entity mockEntity(boolean removed) {
    Entity entity = mock(Entity.class);
    when(entity.isRemoved()).thenReturn(removed);
    return entity;
  }

  private static Set<Entity> newEntitySet(Entity... entities) {
    Set<Entity> entitySet = ConcurrentHashMap.newKeySet();
    Collections.addAll(entitySet, entities);
    return entitySet;
  }

  @BeforeEach
  void resetExcludedNamespaces() throws Exception {
    setExcludedNamespacesViaReflection(Collections.emptySet());
    CoreEntityManager.reset();
  }

  @AfterEach
  void cleanupExcludedNamespaces() throws Exception {
    setExcludedNamespacesViaReflection(Collections.emptySet());
    CoreEntityManager.reset();
  }

  @Test
  void nullEntityIdNotExcluded() {
    CoreEntityManager.setExcludedModNamespaces(Set.of("create"));
    assertFalse(CoreEntityManager.isExcludedModNamespace(null));
  }

  @Test
  void emptyExclusionSetNeverExcludes() {
    assertFalse(CoreEntityManager.isExcludedModNamespace("create:contraption"));
  }

  @Test
  void entityIdWithoutColonNotExcluded() {
    CoreEntityManager.setExcludedModNamespaces(Set.of("create"));
    assertFalse(CoreEntityManager.isExcludedModNamespace("contraption"));
  }

  @ParameterizedTest
  @ValueSource(strings = {
    "create:contraption",
    "create:seat",
    "create:super_glue",
    "botania:spark",
    "botania:mana_burst",
    "mana-and-artifice:residual_magic",
    "appliedenergistics2:meteor",
    "mekanism:robit",
    "minecolonies:citizen",
    "immersive_aircraft:biplane",
    "immersiveengineering:tesla_coil",
    "industrialforegoing:pink_slime",
    "fluxnetworks:flux_point",
    "guardvillagers:archer",
    "xnet:connector"
  })
  void excludedNamespaceEntityIsExcluded(String entityId) {
    String namespace = entityId.substring(0, entityId.indexOf(':'));
    CoreEntityManager.setExcludedModNamespaces(Set.of(namespace));
    assertTrue(CoreEntityManager.isExcludedModNamespace(entityId));
  }

  @ParameterizedTest
  @ValueSource(
    strings = {
      "minecraft:zombie",
      "minecraft:skeleton",
      "minecraft:creeper",
      "alexsmobs:hammerhead_shark",
      "exoticbirds:parrot",
      "friendsandfoes:frog"
    })
  void nonExcludedNamespaceNotExcluded(String entityId) {
    CoreEntityManager.setExcludedModNamespaces(Set.of("create", "botania", "mekanism"));
    assertFalse(CoreEntityManager.isExcludedModNamespace(entityId));
  }

  @Test
  void setExcludedModNamespacesIsImmutable() {
    HashSet<String> mutable = new HashSet<>(Set.of("create"));
    CoreEntityManager.setExcludedModNamespaces(mutable);
    mutable.add("botania");
    assertFalse(
      CoreEntityManager.isExcludedModNamespace("botania:spark"),
      "External modification of source set should not affect internal state");
  }

  @Test
  void multipleNamespacesAllExcluded() {
    CoreEntityManager.setExcludedModNamespaces(
      Set.of("create", "botania", "mana-and-artifice", "minecolonies",
        "appliedenergistics2", "mekanism", "industrialforegoing",
        "immersive_aircraft", "immersiveengineering", "fluxnetworks", "guardvillagers",
        "human_companions", "lootr", "biggerreactors", "modularrouters",
        "pipez", "pokecube_aio", "refinedstorage", "storagedrawers",
        "ultimate_car", "viescraft_machines", "weather2", "xnet",
        "corpse", "easy_npc"));

    assertTrue(CoreEntityManager.isExcludedModNamespace("create:contraption"));
    assertTrue(CoreEntityManager.isExcludedModNamespace("botania:mana_burst"));
    assertTrue(CoreEntityManager.isExcludedModNamespace("mana-and-artifice:residual_magic"));
    assertTrue(CoreEntityManager.isExcludedModNamespace("minecolonies:citizen"));
    assertTrue(CoreEntityManager.isExcludedModNamespace("appliedenergistics2:tiny_tnt"));
    assertTrue(CoreEntityManager.isExcludedModNamespace("mekanism:robit"));
    assertTrue(CoreEntityManager.isExcludedModNamespace("immersive_aircraft:biplane"));
    assertTrue(CoreEntityManager.isExcludedModNamespace("industrialforegoing:pink_slime"));
    assertTrue(CoreEntityManager.isExcludedModNamespace("fluxnetworks:flux_point"));
    assertTrue(CoreEntityManager.isExcludedModNamespace("guardvillagers:archer"));
    assertTrue(CoreEntityManager.isExcludedModNamespace("xnet:connector"));
    assertTrue(CoreEntityManager.isExcludedModNamespace("corpse:human_corpse"));
    assertTrue(CoreEntityManager.isExcludedModNamespace("easy_npc:npc"));
    assertFalse(CoreEntityManager.isExcludedModNamespace("minecraft:zombie"));
    assertFalse(CoreEntityManager.isExcludedModNamespace("mekanismadditions:baby_creeper"));
  }

  @Test
  void nullEntityReturnsFalse() {
    assertFalse(CoreEntityManager.isRelevantEntity(null));
  }

  @Test
  void protectNamespaceRuleKeepsManagedLivingEntitiesOutOfTracking() {
    SpawnPreset preset = new SpawnPreset(
      false,
      "testnpc",
      List.of(),
      100,
      new SpawnPreset.DimensionFilter(List.of(), List.of(), List.of()),
      new SpawnPreset.EntityLimits(Set.of(), Set.of(), 1, 1, 1, 1),
      SpawnPreset.LoadFactors.defaults(),
      TrackingMode.PROTECT_NAMESPACE,
      TrackingCategory.MANAGED_LIVING,
      "Managed living test namespace",
      Set.of());
    CoreEntityManager.reloadTrackingRules(List.of(preset));

    Zombie zombie = new Zombie(EntityType.ZOMBIE, mock(ServerLevel.class));
    assertFalse(CoreEntityManager.isRelevantEntity(zombie, "testnpc:guard"));
    assertFalse(CoreEntityManager.isRelevantEntity(zombie, "testnpc:guard"),
      "Second call should use the cached namespace decision");
  }

  @Test
  void replacingExclusionSetTakesPrecedence() {
    CoreEntityManager.setExcludedModNamespaces(Set.of("create"));
    assertTrue(CoreEntityManager.isExcludedModNamespace("create:contraption"));

    CoreEntityManager.setExcludedModNamespaces(Set.of("botania"));
    assertFalse(
      CoreEntityManager.isExcludedModNamespace("create:contraption"),
      "Old exclusion set should be fully replaced");
    assertTrue(CoreEntityManager.isExcludedModNamespace("botania:spark"));
  }

  @Test
  void emptyReplacementClearsAllExclusions() {
    CoreEntityManager.setExcludedModNamespaces(Set.of("create", "botania"));
    CoreEntityManager.setExcludedModNamespaces(Collections.emptySet());
    assertFalse(CoreEntityManager.isExcludedModNamespace("create:contraption"));
    assertFalse(CoreEntityManager.isExcludedModNamespace("botania:spark"));
  }

  @Test
  void verifyEntitiesRemovesDiscardedEntityReferencesFromAllTrackingMaps() throws Exception {
    Entity activeEntity = mockEntity(false);
    Entity removedEntity = mockEntity(true);
    String levelName = "minecraft:overworld";
    String entityName = "minecraft:zombie";
    String entityMapKey = CoreEntityManager.getEntityMapKey(levelName, entityName);
    String activeChunkKey = "[minecraft:overworld:0x0]";
    String staleChunkKey = "[minecraft:overworld:1x1]";

    ConcurrentHashMap<String, Set<Entity>> entityMap = new ConcurrentHashMap<>();
    entityMap.put(entityMapKey, newEntitySet(activeEntity, removedEntity));

    ConcurrentHashMap<String, Set<Entity>> entityMapPerChunk = new ConcurrentHashMap<>();
    entityMapPerChunk.put(activeChunkKey, newEntitySet(activeEntity, removedEntity));

    ConcurrentHashMap<String, Set<Entity>> entityMapGlobal = new ConcurrentHashMap<>();
    entityMapGlobal.put(entityName, newEntitySet(activeEntity, removedEntity));

    ConcurrentHashMap<Entity, String> entityChunkKeyMap = new ConcurrentHashMap<>();
    entityChunkKeyMap.put(activeEntity, activeChunkKey);
    entityChunkKeyMap.put(removedEntity, activeChunkKey);

    writeStaticField("entityMap", entityMap);
    writeStaticField("entityMapPerChunk", entityMapPerChunk);
    writeStaticField("entityMapGlobal", entityMapGlobal);
    writeStaticField("entityChunkKeyMap", entityChunkKeyMap);
    Map<String, Boolean> entityChunkMap = readStaticField("entityChunkMap");
    entityChunkMap.clear();
    entityChunkMap.put(activeChunkKey, true);
    entityChunkMap.put(staleChunkKey, true);

    Method method = CoreEntityManager.class.getDeclaredMethod("verifyEntities");
    method.setAccessible(true);
    method.invoke(null);

    Map<String, Set<Entity>> cleanedEntityMap = readStaticField("entityMap");
    Map<String, Set<Entity>> cleanedEntityMapPerChunk = readStaticField("entityMapPerChunk");
    Map<String, Set<Entity>> cleanedEntityMapGlobal = readStaticField("entityMapGlobal");
    Map<Entity, String> cleanedChunkKeyMap = readStaticField("entityChunkKeyMap");
    Map<String, Boolean> cleanedChunkMap = readStaticField("entityChunkMap");

    assertEquals(Set.of(activeEntity), cleanedEntityMap.get(entityMapKey));
    assertEquals(Set.of(activeEntity), cleanedEntityMapPerChunk.get(activeChunkKey));
    assertEquals(Set.of(activeEntity), cleanedEntityMapGlobal.get(entityName));
    assertEquals(Map.of(activeEntity, activeChunkKey), cleanedChunkKeyMap);
    assertTrue(cleanedChunkMap.containsKey(activeChunkKey));
    assertFalse(cleanedChunkMap.containsKey(staleChunkKey));
  }
}
