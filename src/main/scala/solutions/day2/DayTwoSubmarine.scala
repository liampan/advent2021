package solutions.day2

import utils.InputReader

object DayTwoSubmarine extends App {
  
  final case class Submarine(horizontal: Int, depth: Int, aim: Int){
    def finalPosition: Int = horizontal * depth
  }

  trait Movement {
    def apply(submarine: Submarine): Submarine
  }
  object Movement {
    def apply(s: String): Movement = {
      s.splitAt(s.indexOf(" ")) match {
        case ("forward", amount) => Forward(amount.trim.toInt)
        case ("up", amount) => Up(amount.trim.toInt)
        case ("down", amount) => Down(amount.trim.toInt)
        case _ => throw new Exception(s"unrecognised movement: '$s'")
      }
    }
    final case class Forward(amount: Int) extends Movement {
      override def apply(submarine: Submarine): Submarine = submarine.copy(horizontal = submarine.horizontal + amount)
    }
    final case class Up(amount: Int) extends Movement {
      override def apply(submarine: Submarine): Submarine = submarine.copy(depth = submarine.depth - amount)
    }
    final case class Down(amount: Int) extends Movement {
      override def apply(submarine: Submarine): Submarine = submarine.copy(depth = submarine.depth + amount)
    }
  }

  val exampleInput =
    """|forward 5
      |down 5
      |forward 8
      |up 3
      |down 8
      |forward 2""".stripMargin.split("\n").toList

  val input: List[String] = InputReader.getInput("day2/movements.txt")


      def partOne(list: List[String]) =
        list
          .map(Movement.apply)
          .foldLeft(Submarine(0, 0, 0))((sub, move)  => move(sub))
          .finalPosition

      println(partOne(exampleInput) == 150)
      println(partOne(input))

  //down X increases your aim by X units.
  //up X decreases your aim by X units.
  //forward X does two things:
  //  It increases your horizontal position by X units.
  //  It increases your depth by your aim multiplied by X.

  trait UpdatedMovement {
    def apply(submarine: Submarine): Submarine
  }
  object UpdatedMovement {
    def apply(s: String): UpdatedMovement = {
      s.splitAt(s.indexOf(" ")) match {
        case ("forward", amount) => Forward(amount.trim.toInt)
        case ("up", amount) => Up(amount.trim.toInt)
        case ("down", amount) => Down(amount.trim.toInt)
        case _ => throw new Exception(s"unrecognised movement: '$s'")
      }
    }
    final case class Forward(amount: Int) extends UpdatedMovement {
      override def apply(submarine: Submarine): Submarine =
        submarine.copy(
          horizontal = submarine.horizontal + amount,
          depth = submarine.depth + (submarine.aim * amount)
        )
    }
    final case class Up(amount: Int) extends UpdatedMovement {
      override def apply(submarine: Submarine): Submarine = submarine.copy(aim = submarine.aim - amount)
    }
    final case class Down(amount: Int) extends UpdatedMovement {
      override def apply(submarine: Submarine): Submarine = submarine.copy(aim = submarine.aim + amount)
    }
  }

  def partTwo(list: List[String]) =
    list
      .map(UpdatedMovement.apply)
      .foldLeft(Submarine(0, 0, 0))((sub, move)  => move(sub))
      .finalPosition

  println(partTwo(exampleInput) == 900)
  println(partTwo(input))
}
