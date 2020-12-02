package dev.vrba.adventofcode.helpers

import scala.io.Source

object ResourceLoader {
  def readResourceLines(path: String): Array[String] = {
    val source = Source.fromFile(getClass.getResource(path).toURI)

    try source.mkString.split("\n")
    finally source.close()
  }
}
