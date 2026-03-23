package io.github.iltotore.redhdl.minecraft

import io.github.iltotore.redhdl.minecraft.nbt.NBT

/**
 * The data associated with a Minecraft block entity (also known as a tile entity).
 *
 * Block entities are used for blocks that store extra data beyond the block-state
 * attributes — for example, signs store their display text in an NBT compound tag.
 * During schematic serialisation each [[BlockEntity]] is written to the
 * `Blocks.BlockEntities` list of the Sponge schematic.
 *
 * @param id   The namespaced entity type identifier (e.g. `"minecraft:sign"`).
 * @param data The NBT compound tag carrying the entity's persistent data.
 */
case class BlockEntity(id: String, data: NBT.CompoundTag)
