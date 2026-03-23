package io.github.iltotore.redhdl

import io.github.iltotore.redhdl.minecraft.{SchematicContext, SchematicGeneration, Block, Palette, Structure, BlockPos, GateType, SchematicGenerator}
import io.github.iltotore.redhdl.graph.{Channel => GraphChannel, Net, NodeType, PinX}
import io.github.iltotore.redhdl.ast.Identifier
import kyo.AllowUnsafe.embrace.danger
import io.github.iltotore.redhdl.graph.PinX
import kyo.*
import utest.*
import io.github.iltotore.redhdl.minecraft.RepeaterDelay

/**
 * Unit tests for the palette and schematic-colouring subsystem.
 *
 * These tests verify:
 *   - That the default palette is returned from [[SchematicContext.getPaletteBlock]]
 *     when no explicit palette is configured.
 *   - That palette entries cycle correctly when the pin index exceeds the palette size.
 *   - That the `"rainbow"` alias in [[Palette.fromStrings]] expands to the full
 *     rainbow wool list.
 *   - That gate schematics are correctly recoloured via [[SchematicGenerator.pasteGateSchematic]].
 *   - That input gates receive a colour derived from the outgoing channel's first net.
 */
object PaletteTests extends TestSuite {
  import kyo.AllowUnsafe.embrace.danger
  val tests = Tests:

    /**
     * Verify that [[SchematicContext.getPaletteBlock]] returns white wool when the
     * context was constructed with the [[Palette.default]] single-entry palette.
     */
    test("default palette"):
      val ctx = SchematicContext(Map.empty, Palette.default, RepeaterDelay(1))
      // run the effect to obtain a block
      val block = Sync.Unsafe.evalOrThrow(
        SchematicContext.getPaletteBlock(PinX(0))
          .handle(
            Env.run(ctx),
            Abort.recover(e => throw new RuntimeException(e.toString))
          )
      )
      assert(block.equals(Block("minecraft:white_wool")))

    /**
     * Verify that palette entries cycle correctly when there are more pin columns
     * than colours: pin 3 should map to the same entry as pin 0 for a 3-entry palette.
     */
    test("cycle multiple"):
      val colors = Chunk(Block("a"), Block("b"), Block("c"))
      val ctx = SchematicContext(Map.empty, colors, RepeaterDelay(1))
      val r0 = Sync.Unsafe.evalOrThrow(
        SchematicContext.getPaletteBlock(PinX(0))
          .handle(
            Env.run(ctx),
            Abort.recover(e => throw new RuntimeException(e.toString))
          )
      )
      val r1 = Sync.Unsafe.evalOrThrow(
        SchematicContext.getPaletteBlock(PinX(1))
          .handle(
            Env.run(ctx),
            Abort.recover(e => throw new RuntimeException(e.toString))
          )
      )
      val r3 = Sync.Unsafe.evalOrThrow(
        SchematicContext.getPaletteBlock(PinX(3))
          .handle(
            Env.run(ctx),
            Abort.recover(e => throw new RuntimeException(e.toString))
          )
      )
      assert(r0.equals(Block("a")))
      assert(r1.equals(Block("b")))
      assert(r3.equals(Block("a")))

    /**
     * Verify that passing `"rainbow"` to [[Palette.fromStrings]] expands to the
     * full [[Palette.rainbow]] list and that additional explicit block IDs are
     * appended after it.
     */
    test("rainbow alias parsing"):
      val input = Chunk("rainbow", "minecraft:oak_log")
      val pal = Palette.fromStrings(input)
      assert(pal.startsWith(Palette.rainbow))
      assert(pal.last.equals(Block("minecraft:oak_log")))

    /**
     * Verify that [[SchematicGenerator.pasteGateSchematic]] respects the `color`
     * override: when a blue wool block is passed as the colour, the pasted structure
     * should contain blue wool instead of the original white wool.
     */
    test("gate schematic recoloured"):
      // create a trivial schematic containing a single white wool block
      val single = Structure(BlockPos(1, 1, 1), Chunk(Block("minecraft:white_wool")))
      val ctx = SchematicContext(Map(GateType.Not -> single), Palette.default, RepeaterDelay(1))

      val colored = Sync.Unsafe.evalOrThrow(
        SchematicGenerator.pasteGateSchematic(
          GateType.Not,
          Structure.empty(BlockPos(1, 1, 1)),
          BlockPos(0, 0, 0),
          Some(Block("minecraft:blue_wool"))
        ).handle(
          Env.run(ctx),
          Abort.recover(e => throw new RuntimeException(e.toString))
        )
      )

      assert(colored.blocks.head.id == "minecraft:blue_wool")

    /**
     * Verify that when [[SchematicGenerator.putLayer]] places an input gate it
     * derives the gate's colour from the outgoing channel's first net, so that the
     * gate does not remain white wool.
     */
    test("input gate recoloured via outgoing channel"):
      // one-input schematic
      val single = Structure(BlockPos(1, 1, 1), Chunk(Block("minecraft:white_wool")))
      val ctx = SchematicContext(Map(GateType.Input -> single), Palette.default, RepeaterDelay(1))

      // channel with a net starting at position 0; this will be used to colour
      // the input gate that resides at x=0 in the first layer
      val channel = GraphChannel(
        Chunk(Net(PinX(0), PinX(0), Absent)),
        Chunk.empty,
        Absent
      )

      val layer = Chunk(NodeType.Input(Identifier.assume("in")))
      val result = Sync.Unsafe.evalOrThrow(
        SchematicGenerator.putLayer(layer, Structure.empty(BlockPos(1, 1, 1)), BlockPos(0, 0, 0), None, Some(channel))
          .handle(
            Env.run(ctx),
            Abort.recover(e => throw new RuntimeException(e.toString))
          )
      )

      assert(result.blocks.head.id != "minecraft:white_wool")
    }
