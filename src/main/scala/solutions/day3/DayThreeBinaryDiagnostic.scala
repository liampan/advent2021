package solutions.day3

import com.sun.tools.classfile.TypeAnnotation.Position
import utils.InputReader

import scala.annotation.tailrec

object DayThreeBinaryDiagnostic extends App {

  val input: List[String] = InputReader.getInput("day3/binaries.txt")

  val exampleInput = """00100
                       |11110
                       |10110
                       |10111
                       |10101
                       |01111
                       |00111
                       |11100
                       |10000
                       |11001
                       |00010
                       |01010""".stripMargin.split("\n").toList

  def partOne(list: List[String]): Int = {
    val groupedBits = list.transpose
    val gammaBin = groupedBits.map(group => if (group.count(_ == '1') > (group.length/2)) '1' else '0').mkString
    val gammaDec = Integer.parseInt(gammaBin, 2)

    val epsilonBin = gammaBin.map{case '0' => '1'; case '1' => '0'}
    val epsilonDec = Integer.parseInt(epsilonBin, 2)

    gammaDec * epsilonDec
  }

  println(partOne(exampleInput) == 198)
  println(partOne(input))


  def partTwo(list: List[String]): Int ={

    def mostCommon(list: List[String], index: Int, preferred: Char, notPreferred: Char)= {
      val selected = list.transpose.apply(index)
      val zeroCount = selected.count(_ == '0')
      val oneCount = selected.count(_ == '1')
      if (zeroCount == oneCount) preferred
      else if(zeroCount > oneCount) notPreferred
      else preferred
    }

    @tailrec
    def partTwoHelper(list: List[String], preferred: Char, notPreferred: Char, cycle: Int = 0): String = {
      val found = list.filter(_.apply(cycle) == mostCommon(list, cycle, preferred, notPreferred))
      if (found.length == 1) found.head else {
       partTwoHelper(found, preferred, notPreferred,  cycle +1)
      }
    }

    val oxygenGeneratorRatingBin = partTwoHelper(list, '1', '0')
    val co2ScrubberRatingBin = partTwoHelper(list, '0', '1')

    val oxygenGeneratorRatingDec = Integer.parseInt(oxygenGeneratorRatingBin, 2)
    val co2ScrubberRatingDec = Integer.parseInt(co2ScrubberRatingBin, 2)

    oxygenGeneratorRatingDec * co2ScrubberRatingDec
  }

  println(partTwo(exampleInput))
  println(partTwo(input))
}
