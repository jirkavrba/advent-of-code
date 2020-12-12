package dev.vrba.adventofcode.day4

import dev.vrba.adventofcode.helpers.ResourceLoader
import dev.vrba.adventofcode.{AdventOfCodeTask, AdventOfCodeTaskSolution, FirstPart, SecondPart, TaskPart}

import scala.io.Source
import scala.util.matching.Regex

@AdventOfCodeTask(2020, 4)
class PassportProcessing extends AdventOfCodeTaskSolution {

  private type Passport = Map[String, String]

  private def loadPassports(input: Source): Seq[Passport] = {
    input.getLines().mkString("\n")
      .split("\n\n")
      .map(_.replace("\n", " ").split(" "))
      .map(
        _.map(part =>
          part.split(":") match {
            case Array(key, value) => (key, value)
            case _ => ("", "") // This is just to make the compiler happy while reasoning about the output type
          }
        ).toMap[String, String]
      )
  }

  private def validateFields(passport: Passport): Boolean = {
    Seq("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid").forall { passport.keySet.contains(_) }
  }

  private def validate(value: String, pattern: Regex, validation: String => Boolean): Boolean = {
    value match {
      case pattern(_*) => validation(value)
      case _ => false
    }
  }

  private def validateValues(passport: Passport): Boolean = {
    passport.forall {
      case (key, value) => key match {
        case "byr" => validate(value, "[0-9]{4}".r, { 1920 to 2020 contains _.toInt })
        case "iyr" => validate(value, "[0-9]{4}".r, { 2010 to 2020 contains _.toInt })
        case "eyr" => validate(value, "[0-9]{4}".r, { 2020 to 2030 contains _.toInt })
        case "hgt" =>
          val pattern = "([0-9]{2,3})(cm|in)".r
          value match {
            case pattern(height, unit) => unit match {
              case "cm" => height.toIntOption.forall { 150 to 193 contains _ }
              case "in" => height.toIntOption.forall { 59 to 76 contains _ }
              case _ => false
            }
            case _ => false
          }
        case "hcl" => value.matches("#[0-9a-f]{6}")
        case "ecl" => Seq("amb", "blu", "brn", "gry", "grn", "hzl", "oth") contains value
        case "pid" => value.matches("[0-9]{9}")
        case "cid" => true
        case _ => false
      }
    }
  }

  override def execute(part: TaskPart): Unit = {
    val source = ResourceLoader.readResource("/2020/04/input")
    val passports = loadPassports(source).filter(validateFields)

    val result = part match {
      case FirstPart => passports.length
      case SecondPart => passports.count(validateValues)
    }

    println(s"Result: $result")
  }
}
