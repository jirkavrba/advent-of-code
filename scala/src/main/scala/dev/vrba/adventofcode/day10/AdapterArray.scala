package dev.vrba.adventofcode.day10

import dev.vrba.adventofcode.helpers.ResourceLoader
import dev.vrba.adventofcode.{AdventOfCodeTask, AdventOfCodeTaskSolution, FirstPart, SecondPart, TaskPart}

import scala.annotation.tailrec
import scala.collection.AbstractIterator

@AdventOfCodeTask(2020, 10)
class AdapterArray extends AdventOfCodeTaskSolution {

  private implicit class ZipIteratorOps[A](it: Iterator[A]) {
    def zipWithTail: Iterator[(A, A)] = {
      if (it.hasNext) {
        new AbstractIterator[(A, A)] {
          private var prev: A = it.next()

          override def hasNext: Boolean = it.hasNext
          override def next(): (A, A) = {
            val cur = it.next()
            val ret = (prev, cur)
            prev = cur
            ret
          }
        }
      }
      else
        Iterator.empty
    }
  }

  def differencesProduct(jolts: Seq[Int]): Int = {
    val initialJolt = 0
    val builtinJolt = jolts.max + 3
    val allJolts = initialJolt +: builtinJolt +: jolts
    val diffs = allJolts.sorted.iterator.zipWithTail.map({ case (a, b) => b - a }).toSeq
    diffs.count(_ == 1) * diffs.count(_ == 3)
  }

  def countArrangements(jolts: Seq[Int]): Long = {
    val builtinJolt = jolts.max + 3
    val allJolts = builtinJolt +: jolts

    @tailrec
    def helper(jolts: List[Int], prevs: Map[Int, Long]): Long = jolts match {
      case Nil => prevs(builtinJolt)
      case jolt :: newJolts =>
        val joltValue = prevs(jolt - 3) + prevs(jolt - 2) + prevs(jolt - 1)
        val newPrevs = prevs + (jolt -> joltValue)
        helper(newJolts, newPrevs)
    }

    helper(allJolts.sorted.toList, Map(0 -> 1L).withDefaultValue(0))
  }

  override def execute(part: TaskPart): Unit = {
    val source = ResourceLoader.readResourceLines("/2020/10/input").map(_.toInt).toSeq
    val result = part match {
      case FirstPart => differencesProduct(source)
      case SecondPart => countArrangements(source)
    }

    println(s"Result: $result")
  }
}