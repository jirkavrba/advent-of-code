package dev.vrba.adventofcode.day8

import dev.vrba.adventofcode.helpers.ResourceLoader
import dev.vrba.adventofcode.{AdventOfCodeTask, AdventOfCodeTaskSolution, FirstPart, SecondPart, TaskPart}

import scala.annotation.tailrec

@AdventOfCodeTask(2020, 8)
class HandheldHalting extends AdventOfCodeTaskSolution {

  private case class ProgramState(instructions: Seq[Instruction],
                                  visited: Set[Int] = Set(),
                                  accumulator: Int = 0,
                                  instructionPointer: Int = 0)

  private def parseInstruction(program: Seq[String]): Seq[Instruction] = {
    val pattern = "(nop|acc|jmp) ([+\\-])([0-9]+)".r

    program.map {
      case pattern(instruction, sign, value) =>
        val multiplier = if (sign.head == '-') -1 else 1
        instruction match {
          case "nop" => NOP
          case "acc" => ACC(value.toInt * multiplier)
          case "jmp" => JMP(value.toInt * multiplier)
          case _ => NOP
        }

      case _ => NOP
    }
  }

  private def performNextInstruction(state: ProgramState): ProgramState = {
    val instruction = state.instructions(state.instructionPointer)
    val visited = state.visited + state.instructionPointer

    instruction match {
      case NOP => ProgramState(state.instructions, visited, state.accumulator, state.instructionPointer + 1)
      case ACC(value) => ProgramState(state.instructions, visited, state.accumulator + value, state.instructionPointer + 1)
      case JMP(steps) => ProgramState(state.instructions, visited, state.accumulator, state.instructionPointer + steps)
    }
  }

  private def programIterator(instructions: Seq[Instruction]): Iterator[ProgramState] = {
    Iterator.iterate(ProgramState(instructions))(performNextInstruction)
  }

  private def findValueBeforeInfiniteLoop(instructions: Seq[Instruction]): Int = {
    programIterator(instructions)
      .dropWhile(state => !state.visited.contains(state.instructionPointer))
      .next
      .accumulator
  }

  private def findValueAfterTermination(instructions: Seq[Instruction]): Int = {
    programIterator(instructions)
      .dropWhile(state => state.instructionPointer < instructions.length)
      .next
      .accumulator
  }

  // TODO: Refactor this shit
  private def isFinite(instructions: Seq[Instruction]): Boolean = {
    try !programIterator(instructions).exists(state => state.visited.contains(state.instructionPointer))
    catch { case _: Throwable => true }
  }

  private def findTerminatingValue(instructions: Seq[Instruction]): Int = {
    val jumps = instructions.zipWithIndex
      .filter { case (instruction, _) => instruction.isInstanceOf[JMP] }
      .map { case (_, index) => index }

    jumps.map(withReplacedJump(instructions, _)).find(isFinite) match  {
      case Some(state) =>
        println(s"Find finite state: $state")
        findValueAfterTermination(state)
      case None => 0
    }
  }

  private def withReplacedJump(instructions: Seq[Instruction], index: Int): Seq[Instruction] = {
    val buffer = instructions.toBuffer
    buffer(index) = NOP
    buffer.toSeq
  }

  override def execute(part: TaskPart): Unit = {
    val program = ResourceLoader.readResourceLines("/2020/08/input")
    val instructions = parseInstruction(program)

    val result = part match {
      case FirstPart => findValueBeforeInfiniteLoop(instructions)
      case SecondPart => findTerminatingValue(instructions)
    }

    println(s"Result: $result")
  }
}
