package org.hablapps.candy

import scalaz._, Scalaz._

import monocle.Lens
import monocle.macros.Lenses

import CandyUtils._

@Lenses case class Game(
  user: String,
  ups: Int,
  level: Option[Level] = Option.empty[Level],
  current: Int = 0)

@Lenses case class Level(
  targetScore: Long,
  targetMoves: Int,
  board: Board,
  currentScore: Long = 0,
  currentMoves: Int = 0)

@Lenses case class Board(
  height: Int,
  width: Int,
  rng: RNG,
  matrix: Map[Pos, Option[Candy]])

// Alien 1: Nested Class Headcrab
object Alien1 {

  // Given a level: get/modify its current score & matrix

  /* 1.1: Explore the area and locate the alien */

  def getScore(lv: Level): Long =
    lv.currentScore

  def modifyScore(f: Long => Long)(lv: Level): Level =
    lv.copy(currentScore = f(lv.currentScore)) // meh

  def getMatrix(lv: Level): CandyMatrix =
    lv.board.matrix

  def modifyMatrix(f: CandyMatrix => CandyMatrix)(lv: Level): Level =
    lv.copy(board = lv.board.copy(matrix = f(lv.board.matrix))) // ugly

  /* 1.2: Equip new weapons and defeat the alien */

  val _currentScore: Lens[Level, Long] =
    Lens[Level, Long](_.currentScore)(cs => _.copy(currentScore = cs))

  val _board: Lens[Level, Board] =
    Lens[Level, Board](_.board)(b => _.copy(board = b))

  val _matrix: Lens[Board, CandyMatrix] =
    Lens[Board, CandyMatrix](_.matrix)(mx => _.copy(matrix = mx))

  import Level._, Board._

  def getScore2: Level => Long =
    currentScore.get

  def modifyScore2(f: Long => Long): Level => Level =
    currentScore.modify(f)

  def getMatrix2: Level => CandyMatrix =
    (board composeLens matrix).get

  def modifyMatrix2(f: CandyMatrix => CandyMatrix): Level => Level =
    (board composeLens matrix).modify(f)
}

object Level {
  val MINIMAL = 2000
  val INCREMENT = 10
  val MOVES = 30

  def apply(level: Int): Level =
    Level(MINIMAL + INCREMENT * level, MOVES, Board())
}

object Board {
  val HEIGHT = 5
  val WIDTH = 8

  def apply(height: Int = HEIGHT, width: Int = WIDTH): Board = Board(
    HEIGHT,
    WIDTH,
    RNG.simple(0),
    cartesian(height, width)
      .map(ia => Pos(ia._1, ia._2))
      .foldRight[Map[Pos, Option[Candy]]](Map.empty) { (p, m) =>
        m + (p -> None)
      })
}

sealed trait Candy
sealed trait KindedCandy extends Candy
sealed trait StripedCandy extends KindedCandy
case class HorStriped(candy: RegularCandy) extends StripedCandy
case class VerStriped(candy: RegularCandy) extends StripedCandy
case object ColourBomb extends Candy
sealed trait RegularCandy extends KindedCandy
case object Red extends RegularCandy
case object Orange extends RegularCandy
case object Yellow extends RegularCandy
case object Green extends RegularCandy
case object Blue extends RegularCandy
case object Purple extends RegularCandy

object Candy {
  implicit class CandyAux(candy: Candy) {
    def shareKind(other: Candy): Boolean =
      (candy.kind |@| other.kind)(_ == _).getOrElse(false)
    def hasKind(kind: RegularCandy): Boolean =
      candy.kind.fold(false)(_ == kind)
    def morph(f: RegularCandy => StripedCandy): Candy = candy match {
      case _: StripedCandy | ColourBomb => candy
      case c: RegularCandy => f(c)
    }
    import Colour._
    def ansiColour: String = candy match {
      case Red => ANSI_RED
      case Orange => ANSI_YELLOW
      case Yellow => ANSI_GREEN
      case Green => ANSI_CYAN
      case Blue => ANSI_BLUE
      case Purple => ANSI_PURPLE
      case HorStriped(c) => c.ansiColour
      case VerStriped(c) => c.ansiColour
      case _ => ""
    }
    def toIcon: String = candy match {
      case Red => "🍅"
      case Orange => "🍌"
      case Yellow => "🍋"
      case Green => "🍒"
      case Blue => "🍍"
      case Purple => "🍓"
      case ColourBomb => "🍪"
      case HorStriped(c) => s"🢐${c.toIcon} 🢒"
      case VerStriped(c) => s"🢓${c.toIcon} 🢑"
    }
    def kind: Option[RegularCandy] = candy match {
      case HorStriped(candy) => candy.some
      case VerStriped(candy) => candy.some
      case ColourBomb => Option.empty
      case regular: RegularCandy => regular.some
    }
  }
}

object KindedCandy {
  implicit class KindedCandyAux(candy: KindedCandy) {
    def kind: RegularCandy = candy match {
      case k: RegularCandy => k
      case HorStriped(k) => k
      case VerStriped(k) => k
    }
  }
}

object RegularCandy {
  implicit class RegularCandyAux(candy: RegularCandy) {
    def stripe(dir: Dir): StripedCandy = dir match {
      case Up   | Down  => VerStriped(candy)
      case Left | Right => HorStriped(candy)
    }
  }
  implicit class IntAux(i: Int) {
    def toRegularCandy: RegularCandy = (i % 6).abs match {
      case 0 => Red
      case 1 => Orange
      case 2 => Yellow
      case 3 => Green
      case 4 => Blue
      case 5 => Purple
    }
  }
}

sealed trait Dir
case object Up extends Dir
case object Down extends Dir
case object Left extends Dir
case object Right extends Dir

case class Pos(i: Int, j: Int) {
  def move(dir: Dir): Pos = dir match {
    case Up => Pos(i - 1, j)
    case Down => Pos(i + 1, j)
    case Left => Pos(i, j - 1)
    case Right => Pos(i, j + 1)
  }
  lazy val down: Pos = move(Down)
  lazy val up: Pos = move(Up)
  lazy val left: Pos = move(Left)
  lazy val right: Pos = move(Right)
}

object Pos {
  implicit def orderInstance: Order[Pos] =
    Order.orderBy[Pos, (Int, Int)](p => (p.i, p.j))
}

sealed trait SwitchOut
case object NotPlaying extends SwitchOut
case object InvalidMove extends SwitchOut
case object YouLose extends SwitchOut
case object YouWin extends SwitchOut
case object Ok extends SwitchOut
