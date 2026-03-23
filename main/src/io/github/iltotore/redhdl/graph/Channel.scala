package io.github.iltotore.redhdl.graph

import kyo.Absent
import kyo.Chunk
import kyo.Maybe
import kyo.Present
import scala.math.Ordering.Implicits.infixOrderingOps

/**
 * A routing channel that sits between two consecutive layers of a logic graph.
 *
 * A [[Channel]] is the central data structure used by the left-edge routing
 * algorithm.  It records every [[Net]] (a horizontal wire segment) and the
 * [[Track]]s (horizontal rows within the channel) they have been assigned to.
 *
 * When a net would create a routing cycle the [[reroute]] method moves part of
 * it to an ''outer'' column beyond the right edge of the channel so the cycle
 * can be broken.
 *
 * @param nets             All nets in the channel (indexed by [[NetId]]).
 * @param tracks           The tracks assigned so far; each track owns a sequence
 *                         of non-overlapping nets.
 * @param firstOuterColumn The X position of the first column used for re-routed
 *                         nets, if any rerouting has taken place.
 */
case class Channel(nets: Chunk[Net], tracks: Chunk[Track], firstOuterColumn: Maybe[PinX]):

  /**
   * Retrieve a net by its identifier.
   *
   * @param id The [[NetId]] to look up.
   * @return The [[Net]] at position `id` in the nets chunk.
   */
  def getNet(id: NetId): Net = nets(id.value)

  /**
   * Find all nets whose left endpoint is at the given X position.
   *
   * @param pos The pin X coordinate to search for.
   * @return A chunk of `(NetId, Net)` pairs where the net starts at `pos`.
   */
  def getNetAt(pos: PinX): Chunk[(NetId, Net)] =
    nets
      .zipWithIndex
      .collect:
        case (net, id) if net.start == pos => (NetId.assume(id), net)

  /**
   * Find the [[TrackId]] of the track that contains the given net.
   *
   * @param id The net to search for.
   * @return [[kyo.Present]] with the [[TrackId]] if the net has been assigned to a
   *         track, or [[kyo.Absent]] if it has not yet been placed.
   */
  // TODO Store TrackId in Net instead of NetId in Track
  def getNetTrack(id: NetId): Maybe[TrackId] =
    Maybe.fromOption(
      tracks.zipWithIndex.collectFirst:
        case (track, tid) if track.nets.contains(id) => TrackId.assume(tid)
    )

  /**
   * Determine the leftmost X position of the first net in a track.
   *
   * @param track The track whose start position is requested.
   * @return The [[PinX]] of the left endpoint of the track's first net.
   */
  def getTrackStart(track: Track): PinX = getNet(track.nets.head).left

  /**
   * Determine the rightmost X position of the last net in a track.
   *
   * @param track The track whose end position is requested.
   * @return The [[PinX]] of the right endpoint of the track's last net.
   */
  def getTrackEnd(track: Track): PinX = getNet(track.nets.last).right

  /**
   * The largest right endpoint across all nets in the channel.
   *
   * @return The maximum [[PinX]] value used by any net.
   */
  def maxPinX: PinX = nets.map(_.right).max

  /**
   * The number of X columns required by this channel (= [[maxPinX]] + 1).
   *
   * @return The width of the channel in pin columns.
   */
  def sizeX: Int = maxPinX.value + 1

  /**
   * The number of tracks (rows) in the channel.
   *
   * @return The height of the channel in track rows.
   */
  def sizeZ: Int = tracks.size

  /**
   * Determine whether a given column belongs to the outer (re-routed) region.
   *
   * @param column The X position to test.
   * @return `true` if `column` is at or to the right of the first outer column.
   */
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

  /**
   * Re-route a net that would introduce a cycle by appending an outer column.
   *
   * The net identified by `id` is split: the original net is truncated to end at a
   * new outer column, and a new continuation net is appended at that column.  The
   * [[firstOuterColumn]] pointer is updated if this is the first rerouting.
   *
   * @param id The [[NetId]] of the net to reroute.
   * @return A new [[Channel]] with the rerouted net and the outer column registered.
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
