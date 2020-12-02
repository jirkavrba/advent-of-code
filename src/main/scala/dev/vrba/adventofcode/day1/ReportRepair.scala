package dev.vrba.adventofcode.day1

import dev.vrba.adventofcode.helpers.ResourceLoader
import dev.vrba.adventofcode.{AdventOfCodeTask, AdventOfCodeTaskSolution, FirstPart, SecondPart, TaskPart}

import scala.collection.mutable

@AdventOfCodeTask(2019, 1)
class ReportRepair extends AdventOfCodeTaskSolution {
  override def execute(part: TaskPart): Unit = {
    val input = ResourceLoader.readResourceLines("/2020/01/input")
      .map(_.toInt)
      .toSet

    part match {
      case FirstPart => findMatchingTuple(input, 2020).foreach(result => println(s"Result: ${result._1 * result._2}"))
      case SecondPart => findMatchingTriple(input, 2020).foreach(result => println(s"Result: ${result._1 * result._2 * result._3}"))
    }
  }

  def findMatchingTuple(set: Set[Int], target: Int): Option[(Int, Int)] = {
    val complements = mutable.Set[Int]()

    set.foreach { value =>
      if (complements.contains(target - value)) return Some(value, target - value)
      else complements.add(value)
    }

    None
  }

  def findMatchingTriple(set: Set[Int], target: Int): Option[(Int, Int, Int)] = {
    set.zipWithIndex.foreach { case(value, index) =>
      val subset = set.drop(index)

      findMatchingTuple(subset, target - value) match {
        case Some(result) => return Some(value, result._1, result._2)
        case None =>
      }
    }

    None
  }
}
