package dev.vrba.adventofcode.day5

import dev.vrba.adventofcode.helpers.ResourceLoader
import dev.vrba.adventofcode.{AdventOfCodeTask, AdventOfCodeTaskSolution, FirstPart, SecondPart, TaskPart}

import scala.collection.SortedSet

@AdventOfCodeTask(2020, 5)
class BinaryBoarding extends AdventOfCodeTaskSolution {

  private case class PlaneSeat(row: Int, column: Int) {
    def id: Int = row * 8 + column
  }

  private def passportCodeToSeat(code: String): PlaneSeat = {
    val (first, second) = code.splitAt(7)

    val row = Integer.parseInt(first.replace("F", "0").replace("B", "1"), 2)
    val column = Integer.parseInt(second.replace("L", "0").replace("R", "1"), 2)

    PlaneSeat(row, column)
  }

  override def execute(part: TaskPart): Unit = {
    val source = ResourceLoader.readResourceLines("/2020/05/input")
    val codes = source
      .takeWhile(_.matches("[FB]{7}[RLR]{3}"))
      .map(passportCodeToSeat(_).id)

    val result = part match {
      case FirstPart => codes.max
      case SecondPart =>
        val seats = codes.to(SortedSet)
        (seats.min to seats.max).find(!seats.contains(_))
    }

    println(s"Result: $result")
  }
}
