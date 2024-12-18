package adventofcode

import java.awt.Color
import scala.io.Source
import scala.util.Try
import scala.util.Failure

/*
    byr (Birth Year)
    iyr (Issue Year)
    eyr (Expiration Year)
    hgt (Height)
    hcl (Hair Color)
    ecl (Eye Color)
    pid (Passport ID)
    cid (Country ID)
*/
case class Passport(
  byr: String, iyr: String, eyr: String, hgt: String, 
  hcl: String, ecl: String,
  pid: String, cid: Option[String])

case class BirthYear(value: String) {
  val year = value.toInt
  require(year >= 1920 && year <= 2002, s"invalid birth year ${value}")
}

case class IssueYear(value: String) {
  val year = value.toInt
  require(year >= 2010 && year <= 2020, s"invalid issue year ${value}")
}

case class ExpirationYear(value: String) {
  val year = value.toInt
  require(year >= 2020 && year <= 2030, s"invalid expiration year ${value}")
}

case class Height(value: String) {
  require(value.endsWith("cm") || value.endsWith("in"), s"invalid height ${value}")
  if (value.endsWith("cm")) {
    val height = value.substring(0, value.size - 2).toInt
    require(height >= 150 && height <= 193, s"invalid height ${value}")
  } else if (value.endsWith("in")) {
    val height = value.substring(0, value.size - 2).toInt
    require(height >= 59 && height <= 76, s"invalid height ${value}")
  }
}

case class HairColor(value: String) {
  require(value.startsWith("#"), s"invalid hair color ${value}")
  Color.decode(value)
}

case class EyeColor(value: String) {
  require(Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(value), s"invalid eye color ${value}")
}

case class PassportId(value: String) {
  require(value.size == 9, s"invalid passport id ${value}")
}

case class CountryId(value: String)

case class ValidatedPassport(
  byr: BirthYear, iyr: IssueYear, eyr: ExpirationYear, hgt: Height, 
  hcl: HairColor, ecl: EyeColor,
  pid: PassportId, cid: Option[CountryId])

object Passport {

  def parsePassport(line: String): Try[Passport] = {
    val values = line.split(' ').map(value => {
      val kv = value.split(':')
      kv(0) -> kv(1) 
    }).toMap

    Try(
      Passport(
        values("byr"),
        values("iyr"),
        values("eyr"),
        values("hgt"),
        values("hcl"),
        values("ecl"),
        values("pid"),
        values.get("cid")
      )
    )
  }

  def load: IndexedSeq[Try[Passport]] =
    Source.fromFile("src/main/resources/day4.txt").mkString.split("\\n\\n")
      .map(_.replace('\n', ' ')).map(parsePassport).toIndexedSeq
}

object Day4Part1 extends App {

  println("Day4 Part1")

  println(Passport.load.filter(_.isSuccess).size)
}

object Day4Part2 extends App {

  def validatePassport(passport: Passport): Try[ValidatedPassport] = {
    Try(
      ValidatedPassport(
        BirthYear(passport.byr),
        IssueYear(passport.iyr),
        ExpirationYear(passport.eyr),
        Height(passport.hgt),
        HairColor(passport.hcl),
        EyeColor(passport.ecl),
        PassportId(passport.pid),
        passport.cid.map(CountryId)
      )
    )
  }

  println("Day4 Part2")

  println(Passport.load.map(_.flatMap(validatePassport)).filter(_.isSuccess).size)
}
