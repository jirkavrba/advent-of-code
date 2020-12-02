package dev.vrba.adventofcode

import scala.annotation.Annotation

case class AdventOfCodeTask(year: Int, day: Int) extends Annotation

sealed trait TaskPart

case object FirstPart extends TaskPart
case object SecondPart extends TaskPart

trait AdventOfCodeTaskSolution {
  def execute(part: TaskPart): Unit
}
