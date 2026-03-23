package io.github.iltotore.redhdl.graph

/**
 * A pair of vertically adjacent nets that share the same horizontal pin column.
 *
 * A [[PortPair]] records the X coordinate of the shared column together with the
 * [[NetId]]s of the net in the top channel ([[netTop]]) and the net in the bottom
 * channel ([[netBot]]) that both terminate at that column.  This is used during
 * schematic generation to connect adjacent channels through a shared vertical wire.
 *
 * @param x      The horizontal pin column where the two nets meet.
 * @param netTop The [[NetId]] of the net in the channel above this pair.
 * @param netBot The [[NetId]] of the net in the channel below this pair.
 */
case class PortPair(x: PinX, netTop: NetId, netBot: NetId)
