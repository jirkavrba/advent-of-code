package dev.vrba.adventofcode.day8

sealed trait Instruction

case object NOP extends Instruction
case class ACC(value: Int) extends Instruction
case class JMP(steps: Int) extends Instruction
