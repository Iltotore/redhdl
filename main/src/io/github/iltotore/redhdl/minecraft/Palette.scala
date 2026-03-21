package io.github.iltotore.redhdl.minecraft

import kyo.*

/**
 * Helpers for handling block palettes used when generating schematics.
 *
 * A palette is an ordered list of [[Block]]s used to colour the wire segments in the
 * generated schematic.  Wires are assigned colours by cycling through the palette
 * based on their horizontal pin column index.
 */
object Palette:

  /**
   * Default palette used when the user does not specify anything.
   *
   * A single white wool block is used, resulting in a uniformly white circuit.
   */
  val default: Chunk[Block] = Chunk(Block("minecraft:white_wool"))

  /**
   * Nearly all the coloured wools provided by Minecraft.  The `rainbow` alias expands to
   * this list (12 entries).
   */
  val rainbow: Chunk[Block] = Chunk(
    "minecraft:orange_wool",
    "minecraft:magenta_wool",
    "minecraft:light_blue_wool",
    "minecraft:yellow_wool",
    "minecraft:lime_wool",
    "minecraft:pink_wool",
    "minecraft:cyan_wool",
    "minecraft:purple_wool",
    "minecraft:blue_wool",
    "minecraft:brown_wool",
    "minecraft:green_wool",
    "minecraft:red_wool"
  ).map(Block(_))

  /**
   * Build a palette from the list of string arguments supplied on the command-line.
   *
   * Special values:
   *   - `"rainbow"` (case-insensitive) expands to the full [[rainbow]] wool list.
   *
   * Any other string is treated as a namespaced block ID and wrapped in a plain [[Block]].
   * If the resulting list is empty (no `--palette` flags were given) the [[default]]
   * palette is returned instead.
   *
   * @param strs The raw palette strings from the command line.
   * @return The resolved palette, or [[default]] if `strs` is empty.
   */
  def fromStrings(strs: Chunk[String]): Chunk[Block] =
    val expanded: Chunk[Block] = strs.flatMap:
      case s if s.equalsIgnoreCase("rainbow") => rainbow
      case other                              => Chunk(Block(other))
    if expanded.isEmpty then default else expanded
