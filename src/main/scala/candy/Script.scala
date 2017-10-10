package org.hablapps.candy

import scalaz.State, State._

import monocle._
import monocle.function.Each.each
import monocle.function.FilterIndex.filterIndex
import monocle.state.all._
import monocle.std.option.some

object Script {

  /* 0. Candy Crush introduction */

  /* 1. Getting and modifying the matrix (last one is ugly!) */

  type CandyMatrix = Map[Pos, Option[Candy]]

  def getScore(lv: Level): Long =
    lv.currentScore // nice

  def modifyScore(f: Long => Long)(lv: Level): Level =
    lv.copy(currentScore = f(lv.currentScore)) // a little bit ugly

  def modifyMatrix(f: CandyMatrix => CandyMatrix)(lv: Level): Level =
    lv.copy(board = lv.board.copy(matrix = f(lv.board.matrix))) // oops

  // Introducing lens

  import Level._, Board._

  def getScore2: Level => Long =
    currentScore.get

  def modifyScore2(f: Long => Long): Level => Level =
    currentScore.modify(f)

  def modifyMatrix2(f: CandyMatrix => CandyMatrix): Level => Level =
    (board ^|-> matrix).modify(f)

  // Implementing lens, manually

  def board2: Lens[Level, Board] =
    Lens[Level, Board](_.board)(br => _.copy(board = br))

  /* 2. Chaining state: modify matrix, score and return score */

  def crushPos(pos: Pos)(lv: Level): (Level, Long) = {
    val lv2 = modifyMatrix(_.updated(pos, None))(lv)
    val lv3 = modifyScore(_ + 1)(lv2)
    (lv3, lv3.currentScore)
  }

  // Introducing State

  def crushPos2(pos: Pos): State[Level, Long] =
    for {
      _  <- (board ^|-> matrix).mod(_.updated(pos, None))
      sc <- currentScore.mod(_ + 1)
    } yield sc

  /* 3. Level is optional! */

  import Game._

  def modifyGameScore(f: Long => Long): Game => Game =
    // (level ^|-> currentScore).modify(f) // oops
    level.modify(_.map(currentScore.modify(f)))

  // Introducing Prism

  def modifyGameScore2(f: Long => Long): Game => Game =
    (level ^<-? some ^|-> currentScore).modify(f)

  // Implementing `some`, manually

  def some2[A]: Prism[Option[A], A] =
    Prism[Option[A], A](identity)(Some(_))

  // What's the type of the composed optic? Introducing `Optional`

  /* 4. Crushing all candy */

  def crushAll: State[Game, Unit] =
    (level ^<-? some ^|-> board ^|-> matrix).mod_(_.mapValues(_ => None))

  // Introducing Traversal

  val tr: Traversal[Game, Option[Candy]] =
    level ^<-? some ^|-> board ^|-> matrix ^|->> each

  def crushAll2: State[Game, Unit] = tr.assign_(None)

  def columnTr(j: Int): Traversal[Game, Option[Candy]] =
    level ^<-? some ^|-> board ^|-> matrix ^|->> filterIndex((p: Pos) => p.j == j)

  def crushColumn(j: Int): State[Game, Unit] =
    for {
      l <- gets(columnTr(j).length)
      _ <- columnTr(j).assign(None)
      _ <- (level ^<-? some ^|-> currentScore).mod_(_ + l * 5)
    } yield ()
}
