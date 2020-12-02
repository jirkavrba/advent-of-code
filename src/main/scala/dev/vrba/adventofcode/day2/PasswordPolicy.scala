package dev.vrba.adventofcode.day2

import dev.vrba.adventofcode.helpers.BooleanImplicits.BooleanOperations
import dev.vrba.adventofcode.{FirstPart, SecondPart, TaskPart}

case class PasswordPolicy(min: Int, max: Int, character: Char) {
  def matches(password: String, part: TaskPart): Boolean = {
      part match {
        case FirstPart => (min to max).contains(password.count(_ == character))
        case SecondPart => password(min - 1) == character xor password(max - 1) == character
      }
  }
}
