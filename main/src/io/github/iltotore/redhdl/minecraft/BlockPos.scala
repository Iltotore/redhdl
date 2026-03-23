package io.github.iltotore.redhdl.minecraft

/**
 * An integer three-dimensional position in a Minecraft world or structure.
 *
 * Axes follow the Minecraft convention: `x` is east/west, `y` is up/down, and `z`
 * is north/south.  [[BlockPos]] is used both as an absolute position and as a
 * displacement vector; arithmetic operators are provided for both use cases.
 *
 * @param x The east/west coordinate.
 * @param y The up/down coordinate.
 * @param z The north/south coordinate.
 */
case class BlockPos(x: Int, y: Int, z: Int):

  /**
   * Translate this position by another [[BlockPos]] vector.
   *
   * @param pos The displacement to add.
   * @return A new [[BlockPos]] equal to `(this.x + pos.x, this.y + pos.y, this.z + pos.z)`.
   */
  def +(pos: BlockPos): BlockPos = BlockPos(x + pos.x, y + pos.y, z + pos.z)

  /**
   * Translate this position by a plain integer triple.
   *
   * @param pos A tuple `(dx, dy, dz)` to add component-wise.
   * @return A new [[BlockPos]] shifted by the given amounts.
   */
  def +(pos: (Int, Int, Int)): BlockPos = BlockPos(x + pos._1, y + pos._2, z + pos._3)

  /**
   * Subtract another [[BlockPos]] vector from this position.
   *
   * @param pos The displacement to subtract.
   * @return A new [[BlockPos]] equal to `(this.x - pos.x, this.y - pos.y, this.z - pos.z)`.
   */
  def -(pos: BlockPos): BlockPos = BlockPos(x - pos.x, y - pos.y, z - pos.z)

  /**
   * Subtract a plain integer triple from this position.
   *
   * @param pos A tuple `(dx, dy, dz)` to subtract component-wise.
   * @return A new [[BlockPos]] shifted by the negated amounts.
   */
  def -(pos: (Int, Int, Int)): BlockPos = BlockPos(x - pos._1, y - pos._2, z - pos._3)
