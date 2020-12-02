package dev.vrba.adventofcode.day2

import dev.vrba.adventofcode.helpers.ResourceLoader
import dev.vrba.adventofcode.{AdventOfCodeTask, AdventOfCodeTaskSolution, FirstPart, TaskPart}

@AdventOfCodeTask(2020, 2)
class PasswordPhilosophy extends AdventOfCodeTaskSolution {
  override def execute(part: TaskPart = FirstPart): Unit = {
    val source = ResourceLoader.readResourceLines("/2020/02/input")
    val policies = parseInput(source)
    val result = policies.count { case (policy, password) => policy.matches(password, part) }

    println(s"Result: $result")
  }

  private def parseInput(lines: Array[String]): Array[(PasswordPolicy, String)] = {
    val pattern = """(\d+)-(\d+) (\w): (\w+)""".r

    lines.map {
      case pattern(min, max, character, password) => (PasswordPolicy(min.toInt, max.toInt, character.head), password)
    }
  }
}
