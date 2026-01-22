package io.github.iltotore.redhdl.minecraft

case class BlockPos(x: Int, y: Int, z: Int):

  def +(pos: BlockPos): BlockPos = BlockPos(x + pos.x, y + pos.y, z + pos.z)

  def +(pos: (Int, Int, Int)): BlockPos = BlockPos(x + pos._1, y + pos._2, z + pos._3)

  def -(pos: BlockPos): BlockPos = BlockPos(x - pos.x, y - pos.y, z - pos.z)

  def -(pos: (Int, Int, Int)): BlockPos = BlockPos(x - pos._1, y - pos._2, z - pos._3)