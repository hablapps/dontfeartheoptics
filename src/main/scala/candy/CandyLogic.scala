package org.hablapps.candy

import Function.const

import scalaz._, Scalaz._

import monocle.{ Lens, Optional, Traversal, ITraversal }
import monocle.state.all._

import CandyOptics._
import CandyUtils._
import RegularCandy.IntAux

object CandyLogic {

  def play: State[Game, Boolean] =
    for {
      ok <- (isIdle |@| nonZeroUps)(_ && _)
      _  <- (loadCurrent >> populate).whenM(ok)
    } yield ok

  def leave: State[Game, Boolean] =
    for {
      ok <- isPlaying
      _  <- (modifyUps(_ - 1) >> unloadCurrent).whenM(ok)
    } yield ok

  def switch(from: Pos, dir: Dir): State[Game, SwitchOut] =
    for {
      _   <- swap(from, dir)
      vld <- (isBombInvolved(from, dir) |@| nonStabilized)(_ || _)
      _   <- bombHandling(from, dir)
      _   <- isSpecial.ifM_(specialCrush(from, dir))
      _   <- if (vld) newMove >> stabilize else undo(from, dir)
      ec  <- exitCondition
      // XXX: great inference job, Scala!
      ok  <- ec.fold((if (vld) Ok else InvalidMove: SwitchOut).point[State[Game, ?]]) { st =>
        unloadCurrent >>
          (if (st) unlockNextLevel >> (YouWin: SwitchOut).point[State[Game, ?]]
           else modifyUps(_ - 1) >> (YouLose: SwitchOut).point[State[Game, ?]])
      }
    } yield ok

  private def isSpecial: State[Game, Boolean] =
    inARowTrSt(4) >>= (tr => gets(tr.nonEmpty))

  private def specialGen(
      min: Int,
      from: Pos,
      dir: Dir, f: Candy => Candy): State[Game, Unit] =
    for {
      tr <- inARowTrSt(min)
      ps <- gets(tr.indices)
      c1 <- candyOp(from).extract
      c2 <- candyOp(from.move(dir)).extract
      _  <- crushMin(min) >>= score
      fun = Functor[Option].compose[Option].compose[Option] // hate this!
      _  <- candyOp(from).assign(fun.map(c1)(f).join).whenM(ps contains from)
      _  <- candyOp(from.move(dir)).assign(fun.map(c2)(f).join)
              .whenM(ps contains from.move(dir))
    } yield ()

  private def specialCrush(from: Pos, dir: Dir): State[Game, Unit] =
    for {
      _ <- specialGen(5, from, dir, _ => ColourBomb)
      _ <- specialGen(4, from, dir, _.morph(_.stripe(dir)))
    } yield ()

  private def isBombInvolved(from: Pos, dir: Dir): State[Game, Boolean] =
    for {
      oc1 <- candyOp(from).extract
      oc2 <- candyOp(from.move(dir)).extract
      res = (oc1.join.join |@| oc2.join.join) {
        case (ColourBomb, _) | (_, ColourBomb) => true
        case _ => false
      }
    } yield res.getOrElse(false)

  private def newMove: State[Game, Unit] =
    currentMovesOp.mod_(_ + 1)

  private def unlockNextLevel: State[Game, Unit] =
    Game.current.mod_(_ + 1)

  private def winCondition: State[Game, Option[Boolean]] =
    for {
      cs <- currentScoreOp.extract
      ts <- targetScoreOp.extract
    } yield (cs |@| ts)(_ >= _)

  private def loseCondition: State[Game, Option[Boolean]] =
    for {
      cm <- currentMovesOp.extract
      tm <- targetMovesOp.extract
    } yield (cm |@| tm)(_ >= _)

  private def exitCondition: State[Game, Option[Boolean]] =
    (winCondition |@| loseCondition) {
      case (Some(true), _) => true.some
      case (_, Some(true)) => false.some
      case _ => None
    }

  private def modifyUps(f: Int => Int): State[Game, Unit] =
    Game.ups.mod_(f)

  private def isIdle: State[Game, Boolean] =
    gets(levelOp.isEmpty)

  private def isPlaying: State[Game, Boolean] =
    isIdle.map(!_)

  private def nonZeroUps: State[Game, Boolean] =
    Game.ups.extracts(_ > 0)

  private def loadCurrent: State[Game, Unit] =
    for {
      n <- Game.current.extract
      _ <- Game.level.assign(Level(n).some)
      _ <- stabilize
    } yield ()

  private def unloadCurrent: State[Game, Unit] =
    Game.level.assign_(None)

  private def stabilize: State[Game, Unit] =
    for {
      _ <- crushMin(3) >>= score
      _ <- gravity
      _ <- populate
      _ <- nonStabilized.ifM_(stabilize)
    } yield ()

  private def nonStabilized: State[Game, Boolean] =
    for {
      tr <- inARowTrSt(3)
      b  <- gets(tr.nonEmpty)
    } yield b

  private def swap(from: Pos, dir: Dir): State[Game, Unit] =
    for {
      c1 <- candyOp(from.move(dir)).extract
      c2 <- candyOp(from).assigno(c1.join)
      _  <- candyOp(from.move(dir)).assign(c2.join)
    } yield ()

  private def undo(from: Pos, dir: Dir): State[Game, Unit] =
    swap(from, dir)

  private def gravity: State[Game, Unit] =
    for {
      tr <- gravityTrSt
      ps <- gets(tr.indices)
      _  <- ps.traverse_[State[Game, ?]](swap(_, Down))
      _  <- if (ps.length > 0) gravity else ().point[State[Game, ?]]
    } yield ()

  private def generateCandy: State[Game, Option[Candy]] =
    rngOp.modo(_.nextInt._2).map(_.map(r => r.nextInt._1.toRegularCandy))

  // private def populate: State[Game, Unit] =
  //   for {
  //     tr <- gapTrSt
  //     ps <- gets(tr.indices)
  //     _  <- ps.traverse_[State[Game, ?]] { p =>
  //             generateCandy >>= (oc => candyOp(p).assign_(Option(oc)))
  //           }
  //   } yield ()

  // import monocle.Indexable.Indexed
  //
  // private def populate: State[Game, Unit] =
  //   for {
  //     tr <- gapTrSt
  //     g  <- gets(tr.modifyF[State[Game, ?], ? => ?](const(generateCandy))).join
  //     _  <- put(g)
  //   } yield ()

  private def populate: State[Game, Unit] =
    for {
      tr <- gapTrSt
      ps <- gets(tr.indices)
      m  <- ps.traverse[State[Game, ?], (Pos, Option[Candy])](generateCandy.strengthL).map(_.toMap)
      _  <- tr.mod(p => const(m.get(p).join))
    } yield ()

  private def stripeKind(
      kind: RegularCandy,
      f: RegularCandy => StripedCandy): State[Game, Unit] =
    kindTrSt(kind) >>= (_.mod_(_ => _ map (_ morph f)))

  private def score(crushed: Int): State[Game, Unit] =
    currentScoreOp.mod_(_ + (crushed * 5))

  private def bombHandling(from: Pos, dir: Dir): State[Game, Unit] =
    for {
      oc1 <- candyOp(from).extract
      oc2 <- candyOp(from.move(dir)).extract
      _   <- ((oc1.join.join |@| oc2.join.join) {
        case (ColourBomb, ColourBomb) =>
          crushAll
        case (ColourBomb, c: RegularCandy) =>
          crushPos(from) >> crushKind(c)
        case (c: RegularCandy, ColourBomb) =>
          crushPos(from.move(dir)) >> crushKind(c)
        case (ColourBomb, sc: StripedCandy) =>
          for {
            _ <- crushPos(from)
            _ <- stripeKind(sc.kind, _.stripe(dir))
            _ <- crushKind(sc.kind)
          } yield ()
        case (sc: StripedCandy, ColourBomb) =>
          for {
            _ <- crushPos(from.move(dir))
            _ <- stripeKind(sc.kind, _.stripe(dir))
            _ <- crushKind(sc.kind)
          } yield ()
        case _ => ().point[State[Game, ?]]
      }).getOrElse(().point[State[Game, ?]])
    } yield ()

  private def crushPos(pos: Pos): State[Game, Int] =
    for {
      oc <- candyOp(pos).extract
      n  <- oc.join.join match {
        case Some(HorStriped(_)) => candyOp(pos).assign(Option(None)) >> crushLine(pos.i)
        case Some(VerStriped(_)) => candyOp(pos).assign(Option(None)) >> crushColumn(pos.j)
        case Some(_) => candyOp(pos).assign(Option(None)) >> 1.point[State[Game, ?]]
        case _ => 0.point[State[Game, ?]]
      }
    } yield n

  private def crushWith(tr: ITraversal[Pos, Game, Option[Candy]]): State[Game, Int] =
    for {
      ps <- gets(tr.indices)
      xs <- ps.traverse[State[Game, ?], Int](crushPos)
    } yield xs.sum

  private def crushKind(kind: RegularCandy): State[Game, Int] =
    kindTrSt(kind) >>= crushWith

  private def crushLine(i: Int): State[Game, Int] =
    crushWith(lineITr(i))

  private def crushColumn(j: Int): State[Game, Int] =
    crushWith(columnITr(j))

  private def crushMin(n: Int): State[Game, Int] =
    inARowTrSt(n) >>= crushWith

  private def crushAll: State[Game, Int] =
    crushWith(matrixITr)

  /* state-based optics */

  private def kindTrSt(kind: RegularCandy): State[Game, ITraversal[Pos, Game, Option[Candy]]] =
    gets(matrixITr.foldMap { i => oc =>
      oc.fold[List[Pos]](List.empty) { c =>
        if (c.shareKind(kind)) List(i) else List.empty
      }
    }).map(ps => posRangeITr(ps: _*))

  private def inARowTrSt(n: Int): State[Game, ITraversal[Pos, Game, Option[Candy]]] =
    matrixOp.extracts[List[Pos]] { mx =>
      mx.filter { case (p, oc) =>
        def check(f: Pos => Pos): Int =
          iterateWhile(p)(f, pos => oc.fold(false)(c => mx.get(pos).join.fold(false)(_.shareKind(c)))).size
        (check(_.left) + check(_.right) > n) || (check(_.up) + check(_.down) > n)
      }.keys.toList
    }.map(_.fold(posRangeITr())(posRangeITr(_: _*)))

  private def gapTrSt: State[Game, ITraversal[Pos, Game, Option[Candy]]] =
    gets(matrixITr.foldMap { i => oc =>
      oc.fold[List[Pos]](List(i))(const(List.empty))
    }).map(posRangeITr(_: _*))

  private def gravityTrSt: State[Game, ITraversal[Pos, Game, Option[Candy]]] =
    matrixOp.extracts[List[Pos]] { mx =>
      mx.filter {
        case (p, Some(_)) => mx.get(p.down) == Option(None)
        case _ => false
      }.keys.toList
    }.map(_.fold(posRangeITr())(posRangeITr(_: _*)))
}
