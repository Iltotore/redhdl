package io.github.iltotore.redhdl.graph

import kyo.Chunk

/**
 * A single horizontal routing row inside a [[Channel]].
 *
 * A [[Track]] is a sequence of [[NetId]]s that have been assigned to the same
 * horizontal row by the left-edge channel routing algorithm.  The nets within a
 * track must not overlap horizontally (i.e. the right endpoint of each net must
 * be less than the left endpoint of the next net in the same track).
 *
 * Tracks are numbered from top to bottom within their channel and are translated
 * directly into rows of redstone wire bridges in the generated Minecraft schematic.
 *
 * @param nets The ordered list of [[NetId]]s assigned to this track, from left
 *             to right.
 */
case class Track(nets: Chunk[NetId])
