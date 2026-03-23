package io.github.iltotore.redhdl.minecraft

/**
 * Failures that can occur during schematic loading or generation.
 *
 * These failures are surfaced through the [[SchematicGeneration]] effect and are
 * ultimately collected as [[io.github.iltotore.redhdl.CompilerFailure]] diagnostics
 * by the [[io.github.iltotore.redhdl.Compilation]] runner.
 */
enum SchematicFailure:
  /**
   * A gate schematic resource could not be located.
   *
   * This typically means the bundled `.schem` file for `tpe` is absent from the
   * classpath resources, which indicates a packaging or build issue.
   *
   * @param tpe The [[GateType]] whose resource file could not be found.
   */
  case MissingSchematic(tpe: GateType)

  /**
   * A schematic file was found but its contents could not be parsed successfully.
   *
   * @param path    A human-readable identifier for the schematic (e.g. the resource path
   *                or a file system path).
   * @param message A description of the parse error.
   */
  case InvalidSchematic(path: String, message: String)
