package dev.vrba.adventofcode.day6

import dev.vrba.adventofcode.helpers.ResourceLoader
import dev.vrba.adventofcode.{AdventOfCodeTask, AdventOfCodeTaskSolution, FirstPart, SecondPart, TaskPart}

import scala.io.Source

@AdventOfCodeTask(2020, 6)
class CustomCustoms extends AdventOfCodeTaskSolution {

  private def loadGroups(source: Source): Array[Array[String]] = {
    source.mkString("")
      .split("\n\n")
      .map(_.split("\n"))
  }

  private def cumulativeResponses(group: Array[String]): Int = {
    group.mkString("")
      .toSet[Char]
      .size
  }

  private def commonResponses(group: Array[String]): Int = {
    group.map(_.toSet[Char])
      .foldLeft(('a' to 'z').toSet)((current, accumulator) => accumulator intersect current)
      .size
  }

  override def execute(part: TaskPart): Unit = {
    val source = ResourceLoader.readResource("/2020/06/input")
    val groups = loadGroups(source)

    val result = part match {
      case FirstPart => groups.map(cumulativeResponses).sum
      case SecondPart => groups.map(commonResponses).sum
    }

    println(s"Result: $result")
  }
}
