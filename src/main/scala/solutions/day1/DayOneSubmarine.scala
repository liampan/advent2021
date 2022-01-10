package solutions.day1

import utils.InputReader

object DayOneSubmarine extends App {

  val input: List[Int] = InputReader.getInput("day1/sonar.txt")
    .map(_.toInt)

  val exampleInput = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)

  def partOne(list: List[Int]): Int =
    list
      .sliding(2)
      .count{
        case previous :: current :: Nil => current > previous
      }

  println(partOne(exampleInput) == 7) //example answer
  println(partOne(input))


  def partTwo(list: List[Int]): Int =
    partOne(list.sliding(3).map(_.sum).toList)


  println(partTwo(exampleInput) == 5) //example answer
  println(partTwo(input))

}
