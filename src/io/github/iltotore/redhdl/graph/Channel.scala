package io.github.iltotore.redhdl.graph

import kyo.Chunk
import kyo.Maybe

case class Channel(nets: Chunk[Net], tracks: Chunk[Track]):

  def getNet(id: NetId): Net = nets(id.value)

  //TODO Store TrackId in Net instead of NetId in Track
  def getNetTrack(id: NetId): TrackId =
    tracks.zipWithIndex.collectFirst:
      case (track, tid) if track.nets.contains(id) => TrackId.assume(tid)
    .get

  def getTrackStart(track: Track): PinX = getNet(track.nets.head).left

  def getTrackEnd(track: Track): PinX = getNet(track.nets.last).right