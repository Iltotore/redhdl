package io.github.iltotore.redhdl.graph

import kyo.Absent
import kyo.Chunk
import kyo.Maybe
import kyo.Present
import scala.math.Ordering.Implicits.infixOrderingOps

case class Channel(nets: Chunk[Net], tracks: Chunk[Track], firstOuterColumn: Maybe[PinX]):

  def getNet(id: NetId): Net = nets(id.value)

  def getNetAt(pos: PinX): Chunk[(NetId, Net)] =
    nets
      .zipWithIndex
      .collect:
        case (net, id) if net.start == pos => (NetId.assume(id), net)

  // TODO Store TrackId in Net instead of NetId in Track
  def getNetTrack(id: NetId): Maybe[TrackId] =
    Maybe.fromOption(
      tracks.zipWithIndex.collectFirst:
        case (track, tid) if track.nets.contains(id) => TrackId.assume(tid)
    )

  def getTrackStart(track: Track): PinX = getNet(track.nets.head).left

  def getTrackEnd(track: Track): PinX = getNet(track.nets.last).right

  def maxPinX: PinX = nets.map(_.right).max

  def sizeX: Int = maxPinX.value + 1

  def sizeZ: Int = tracks.size

  def isOuterColumn(column: PinX): Boolean = firstOuterColumn.exists(column >= _)

  /*
  0 1 2
  + |
  -----
    | +
    | |
    + |
  --- |
  +   |
  |   +
  | ---
  | +
  2 3


   */

  def reroute(id: NetId): Channel =
    val net = getNet(id)
    val outerStart = maxPinX + 1
    val outerId = NetId.assume(nets.size)
    val outerNet = Net(outerStart, net.end, net.outerNet)

    this.copy(
      nets = nets
        .updated(id.value, Net(net.start, outerStart, Present(outerId)))
        .appended(outerNet),
      firstOuterColumn = firstOuterColumn.orElse(Present(outerStart))
    )
