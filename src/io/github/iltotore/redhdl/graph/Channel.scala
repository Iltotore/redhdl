package io.github.iltotore.redhdl.graph

import kyo.Chunk
import kyo.Maybe

case class Channel(nets: Chunk[Net], tracks: Chunk[Track]):

  def getNet(id: NetId): Net = nets(id.value)

  def getTrackEnd(track: Track): PinX = getNet(track.nets.last).right