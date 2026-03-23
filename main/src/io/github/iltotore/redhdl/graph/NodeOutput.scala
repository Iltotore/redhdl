package io.github.iltotore.redhdl.graph

/**
 * A directed edge in the logic [[Graph]], representing a wire from one node to a
 * specific input slot of another node.
 *
 * [[NodeOutput]] values are stored on the ''source'' node; the source node drives the
 * signal and the `id`/`connectedInput` pair identifies where that signal arrives.
 *
 * A [[CanEqual]] instance is derived so that [[NodeOutput]] values can be compared
 * with `==` in collections and maps without implicit widening.
 *
 * @param id             The [[NodeId]] of the downstream (consumer) node.
 * @param connectedInput The zero-based input index on the downstream node that
 *                       this edge is connected to.  Multi-input gates (OR, AND, XOR)
 *                       use index 0 for the left operand and 1 for the right.
 */
case class NodeOutput(id: NodeId, connectedInput: Int) derives CanEqual
