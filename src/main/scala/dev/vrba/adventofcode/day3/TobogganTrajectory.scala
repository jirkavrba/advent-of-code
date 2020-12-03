package dev.vrba.adventofcode.day3

import dev.vrba.adventofcode.helpers.ResourceLoader
import dev.vrba.adventofcode.{AdventOfCodeTask, AdventOfCodeTaskSolution, FirstPart, SecondPart, TaskPart}

@AdventOfCodeTask(2020, 3)
class TobogganTrajectory extends AdventOfCodeTaskSolution {

  def traverseToboggan(source: Seq[String], slope: (Int, Int)): Seq[Char] = {
    val width = source.head.length
    val height = source.length

    val (x, y) = slope

    (0 until height / y).map { index => source(index * y)((index * x) % width) }
  }

  override def execute(part: TaskPart): Unit = {
    val source = ResourceLoader.readResourceLines("/2020/03/input")
    val result = part match {
      case FirstPart => traverseToboggan(source, (3, 1)).count(_ == '#')
      case SecondPart =>
        val slopes = Seq((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
        slopes.map(traverseToboggan(source, _).count(_ == '#')).product
    }

    println(s"Result: $result")
  }
}
