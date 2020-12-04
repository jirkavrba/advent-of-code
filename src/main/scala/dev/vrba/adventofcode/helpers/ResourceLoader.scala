package dev.vrba.adventofcode.helpers

import scala.io.Source

object ResourceLoader {
  def readResource(path: String): Source = Source.fromFile(getClass.getResource(path).toURI)

  def readResourceLines(path: String): Array[String] = {
    val source = readResource(path)

    try source.mkString.split("\n")
    finally source.close()
  }

}
