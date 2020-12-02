package dev.vrba.adventofcode.helpers

object BooleanImplicits {

  implicit class BooleanOperations(val source: Boolean) {
    def xor(other: Boolean): Boolean = source && !other || !source && other
  }

}
