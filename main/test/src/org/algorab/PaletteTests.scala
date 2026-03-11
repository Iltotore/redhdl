package io.github.iltotore.redhdl

import io.github.iltotore.redhdl.minecraft.{SchematicContext, SchematicGeneration, Block, Palette, Structure, BlockPos, GateType, SchematicGenerator}
import io.github.iltotore.redhdl.graph.{Channel => GraphChannel, Net, NodeType, PinX}
import io.github.iltotore.redhdl.ast.Identifier
import kyo.AllowUnsafe.embrace.danger
import io.github.iltotore.redhdl.graph.PinX
import kyo.*
import utest.*
import io.github.iltotore.redhdl.minecraft.RepeaterDelay

object PaletteTests extends TestSuite {
  import kyo.AllowUnsafe.embrace.danger
  val tests = Tests:

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

    test("rainbow alias parsing"):
      val input = Chunk("rainbow", "minecraft:oak_log")
      val pal = Palette.fromStrings(input)
      assert(pal.startsWith(Palette.rainbow))
      assert(pal.last.equals(Block("minecraft:oak_log")))

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
