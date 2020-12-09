package dev.vrba.adventofcode.day9

import dev.vrba.adventofcode.helpers.ResourceLoader
import dev.vrba.adventofcode.{AdventOfCodeTask, AdventOfCodeTaskSolution, FirstPart, SecondPart, TaskPart}

import scala.collection.mutable

@AdventOfCodeTask(2020, 9)
class EncodingError extends AdventOfCodeTaskSolution {

  private def numberMatches(preamble: Seq[Long], number: Long): Boolean = {
    val complements = mutable.Set[Long]()

    preamble.toSet[Long].foreach { value =>
      if (complements.contains(number - value)) return true
      else complements.add(value)
    }

    false
  }

  private def firstNumberThatDoesntMatch(iterator: Iterator[Long]#GroupedIterator[Long]): Long = {
    iterator.find(chunk => !numberMatches(chunk.take(25), chunk.last))
      .getOrElse(Seq(0L))
      .last
  }

  // TODO: Improve this algorithm by using optimized sums
  private def contiguousSumRange(numbers: Seq[Long], sum: Long): Seq[Long] = {
    val sumNumbers = numbers.scanLeft(0L)(_ + _)
    sumNumbers
      .zipWithIndex
      .combinations(2)
      .collectFirst({ case Seq((sum1, i1), (sum2, i2)) if i1 + 1 < i2 && sum1 + sum == sum2 => numbers.slice(i1, i2) })
      .get
  }

  private def findContiguousRangeThatSumsUp(numbers: Iterator[Long], invalid: Long): Long = {
    val range = contiguousSumRange(numbers.toSeq, invalid)
    range.min + range.max
  }

  override def execute(part: TaskPart): Unit = {
    val source = ResourceLoader.readResourceLines("/2020/09/input")
    val converted = source.map(_.toLong)
    val broken = firstNumberThatDoesntMatch(Iterator.from(converted).sliding(26))

    val result = part match {
      case FirstPart => broken
      case SecondPart => findContiguousRangeThatSumsUp(Iterator.from(converted), broken)
    }

    println(s"Result: $result")
  }
}
