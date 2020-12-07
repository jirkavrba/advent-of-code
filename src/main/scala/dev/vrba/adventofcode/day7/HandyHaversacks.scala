package dev.vrba.adventofcode.day7

import dev.vrba.adventofcode.helpers.ResourceLoader
import dev.vrba.adventofcode.{AdventOfCodeTask, AdventOfCodeTaskSolution, FirstPart, SecondPart, TaskPart}

import scala.annotation.tailrec

@AdventOfCodeTask(2020, 7)
class HandyHaversacks extends AdventOfCodeTaskSolution {
  private def parseRules(source: String): Seq[BagContentRule] = {
    val parts = source.split(""" bags?[,.]\s?""")
    val pattern = """(\d+) (.*)""".r

    parts.filter(_ != "no other").map { case pattern(amount, color) => BagContentRule(amount.toInt, color) }
  }

  private def parseBag(source: String): Option[Bag] = {
    source.split(" bags contain ") match {
      case Array(color, rules) => Some(Bag(color, parseRules(rules)))
      case _ => None
    }
  }

  private def allBagContaining(bags: Seq[Bag], color: String): Set[String] = {
    val matching = bags.filter { _.rules.exists(_.color == color) }.map(_.color).toSet
    matching ++ matching.flatMap(allBagContaining(bags, _))
  }

  override def execute(part: TaskPart): Unit = {
    val source = ResourceLoader.readResourceLines("/2020/07/input")
    val bags = source.map(parseBag).filter(_.isDefined).map(_.get)

    val result = part match {
      case FirstPart => allBagContaining(bags, "shiny gold").size
      case SecondPart => 0
    }

    println(s"Result: $result")
  }
}
