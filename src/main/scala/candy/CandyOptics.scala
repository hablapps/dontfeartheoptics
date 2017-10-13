package org.hablapps.candy

import monocle.{ Iso, Lens, Optional, Traversal, ITraversal }
import monocle.function.At._
import monocle.function.IFilterIndex._
import monocle.function.IEach._
import monocle.std.map._
import monocle.std.option._

import CandyUtils._

object CandyOptics {

  def levelOp: Optional[Game, Level] =
    Game.level ^<-? some

  def boardOp: Optional[Game, Board] =
    levelOp ^|-> Level.board

  def targetScoreOp: Optional[Game, Long] =
    levelOp ^|-> Level.targetScore

  def currentScoreOp: Optional[Game, Long] =
    levelOp ^|-> Level.currentScore

  def targetMovesOp: Optional[Game, Int] =
    levelOp ^|-> Level.targetMoves

  def currentMovesOp: Optional[Game, Int] =
    levelOp ^|-> Level.currentMoves

  def heightOp: Optional[Game, Int] =
    boardOp ^|-> Board.height

  def widthOp: Optional[Game, Int] =
    boardOp ^|-> Board.width

  def rngOp: Optional[Game, RNG] =
    boardOp ^|-> Board.rng

  def matrixOp: Optional[Game, CandyMatrix] =
    boardOp ^|-> Board.matrix

  def matrixITr: ITraversal[Pos, Game, Option[Candy]] =
    matrixOp ^|->> iEach

  def candyOp(pos: Pos): Optional[Game, Option[Option[Candy]]] =
    matrixOp ^|-> at(pos)

  def lineITr(i: Int): ITraversal[Pos, Game, Option[Candy]] =
    matrixOp ^|->> iFilterIndex(_.i == i)

  def columnITr(j: Int): ITraversal[Pos, Game, Option[Candy]] =
    matrixOp ^|->> iFilterIndex(_.j == j)

  def posRangeITr(pos: Pos*): ITraversal[Pos, Game, Option[Candy]] =
    matrixOp ^|->> iFilterIndex(pos contains _)
}
