package org.hablapps.candy

import scalaz._, Scalaz._
import monocle.{ Lens, Traversal }
import monocle.state.all._

trait CandyLogic { this: CandyOptics with CandyState with CandyUtils =>

  def play: State[Game, Boolean] =
    for {
      ok <- (isIdle |@| nonZeroUps)(_ && _)
      _  <- loadCurrent.whenM(ok)
      _  <- setPlaying.whenM(ok)
    } yield ok

  def leave: State[Game, Boolean] =
    for {
      ok <- isPlaying
      _  <- modifyUps(_ - 1).whenM(ok)
      _  <- setIdle.whenM(ok)
    } yield ok

  def switch(from: Pos, dir: Dir): State[Game, SwitchOut] =
    isPlaying.ifM(
      for {
        _   <- swap(from, dir)
        vld <- (isBombInvolved(from, dir) |@| nonStabilized)(_ || _)
        _   <- bombHandling(from, dir)
        _   <- isSpecial.ifM_(specialCrush(from, dir))
        _   <- if (vld) newMove >> stabilize else undo(from, dir)
        win <- checkWinningCondition
        _   <- unlockNextLevel.whenM(win)
        los <- checkLosingCondition
        _   <- modifyUps(_ - 1).whenM(!win && los)
        _   <- setIdle.whenM(win || los)
        ok = if (win) YouWin
             else if (los) YouLose
             else if (vld) Ok
             else InvalidMove
      } yield ok,
      (NotPlaying: SwitchOut).pure[State[Game, ?]])

  private def bombHandling(from: Pos, dir: Dir): State[Game, Unit] =
    for {
      oc1 <- candyLn(from).extract
      oc2 <- candyLn(from.move(dir)).extract
      _    <- ((oc1 |@| oc2) {
        case (ColourBomb, ColourBomb) => crushAll
        case (ColourBomb, c: RegularCandy) =>
          crushPos(from) >> crushKind(c)
        case (c: RegularCandy, ColourBomb) =>
          crushPos(from.move(dir)) >> crushKind(c)
        case (ColourBomb, sc: StripedCandy) =>
          for {
            _ <- crushPos(from)
            _ <- stripeKind(sc.kind, dirToStripe(dir))
            _ <- crushKind(sc.kind)
          } yield ()
        case (sc: StripedCandy, ColourBomb) =>
          for {
            _ <- crushPos(from.move(dir))
            _ <- stripeKind(sc.kind, dirToStripe(dir))
            _ <- crushKind(sc.kind)
          } yield ()
        case _ => ().point[State[Game, ?]]
      }).getOrElse(().point[State[Game, ?]])
    } yield ()

  private def isSpecial: State[Game, Boolean] =
    for {
      h <- heightLn.extract
      w <- widthLn.extract
      b <- gets(inarowTr(4)(h, w).length).map(_ > 0)
    } yield b

  // TODO: this is pretty ugly, refactor it!
  private def specialGen(
      min: Int,
      from: Pos,
      dir: Dir,
      f: Candy => Candy): State[Game, Unit] =
    for {
      h  <- heightLn.extract
      w  <- widthLn.extract
      c1 <- candyLn(from).extract
      c2 <- candyLn(from.move(dir)).extract
      cs <- gets(inarowTr(min)(h, w).getAll).map(_.map(_._1))
      _  <- crushMin(min) >>= score
      _  <- candyLn(from).assign(c1.map(f)).whenM(cs contains from)
      _  <- candyLn(from.move(dir)).assign(c2.map(f))
              .whenM(cs contains from.move(dir))
    } yield ()

  // TODO: move to state `morph(dir)`
  private def dirToStripe(dir: Dir): RegularCandy => StripedCandy =
    dir match {
      case Up | Down => VerStriped
      case Left | Right => HorStriped
    }

  private def specialCrush(from: Pos, dir: Dir): State[Game, Unit] =
    for {
      _ <- specialGen(5, from, dir, _ => ColourBomb)
      _ <- specialGen(4, from, dir, _.morph(dirToStripe(dir)))
    } yield ()

  private def newMove: State[Game, Unit] =
    currentMovesLn.mod_(_ + 1)

  private def unlockNextLevel: State[Game, Unit] =
    Game.last.mod_(_ + 1)

  private def checkWinningCondition: State[Game, Boolean] =
    (gets(currentScoreLn.get) |@| gets(targetScoreLn.get))(_ >= _)

  private def checkLosingCondition: State[Game, Boolean] =
    (gets(currentMovesLn.get) |@| gets(targetMovesLn.get))(_ >= _)

  private def modifyUps(f: Int => Int): State[Game, Unit] =
    Game.ups.mod_(f)

  private def setIdle: State[Game, Unit] =
    Game.idle.assign_(true)

  private def setPlaying: State[Game, Unit] =
    Game.idle.assign_(false)

  private def isIdle: State[Game, Boolean] =
    Game.idle.extract

  private def isPlaying: State[Game, Boolean] =
    isIdle.map(!_)

  private def nonZeroUps: State[Game, Boolean] =
    Game.ups.extract.map(_ > 0)

  private def loadCurrent: State[Game, Unit] =
    for {
      _ <- gets(Game.last.get) <*> gets(Game.levels.get)
      _ <- stabilize
      _ <- currentScoreLn.assign(0)
    } yield ()

  private def stabilize: State[Game, Unit] =
    for {
      _ <- crushMin(3) >>= score
      _ <- gravity
      _ <- populate
      _ <- nonStabilized.ifM_(stabilize)
    } yield ()

  private def isBombInvolved(from: Pos, dir: Dir): State[Game, Boolean] =
    for {
      oc1 <- candyLn(from).extract
      oc2 <- candyLn(from.move(dir)).extract
      res = (oc1 |@| oc2) {
        case (ColourBomb, _) | (_, ColourBomb) => true
        case _ => false
      }
    } yield res.getOrElse(false)

  private def nonStabilized: State[Game, Boolean] =
    for {
      h <- heightLn.extract
      w <- widthLn.extract
      c <- gets(inarowTr(3)(h, w).length)
    } yield c > 0

  private def swap(from: Pos, dir: Dir): State[Game, Unit] =
    for {
      mx <- matrixLn.extract
      _  <- candyLn(from).assign(mx.get(from.move(dir)))
      _  <- candyLn(from.move(dir)).assign(mx.get(from))
    } yield ()

  private def undo(from: Pos, dir: Dir): State[Game, Unit] =
    swap(from, dir)

  private def gravity: State[Game, Unit] =
    for {
      h <- heightLn.extract
      w <- widthLn.extract
      _ <- modify(gravityTr(h, w).modify(kv => (kv._1.down, kv._2)))
             .whileM_(gets(gravityTr(h, w).length(_) > 0))
    } yield ()

  private def generateCandy: State[Game, RegularCandy] =
    for {
      r <- rngLn.extract
      (i, r2) = r.nextInt
      _ <- rngLn.assign(r2)
    } yield RegularCandy.fromInt(i)

  private def generateCandy(n: Int): State[Game, List[RegularCandy]] =
    iterateN[State[Game, ?], RegularCandy](generateCandy, n)

  private def populate: State[Game, Unit] =
    for {
      mx <- matrixLn.extract
      h  <- heightLn.extract
      w  <- widthLn.extract
      gaps = cartesian(h, w).map(k => Pos(k._1, k._2)).filter(! mx.isDefinedAt(_))
      xs <- generateCandy(gaps.size).map(_.zip(gaps))
      _  <- xs.traverse_[State[Game, ?]](x => modify(candyLn(x._2).set(x._1.some)))
    } yield ()

  private def stripeKind(
      kind: RegularCandy,
      f: RegularCandy => StripedCandy): State[Game, Unit] =
    for {
      h <- heightLn.extract
      w <- widthLn.extract
      _ <- modify(kindTr(kind)(h, w).modify(kv => (kv._1, kv._2.map(_.morph(f)))))
    } yield ()

  private def score(crushed: Int): State[Game, Unit] =
    currentScoreLn.mod_(_ + (crushed * 20))

  private def crushPos(pos: Pos): State[Game, Int] =
    for {
      oc <- candyLn(pos).extract
      _  <- candyLn(pos).assign(None)
      ln <- oc match {
        case Some(HorStriped(_)) => crushLine(pos.i)
        case Some(VerStriped(_)) => crushColumn(pos.j)
        case _ => 1.point[State[Game, ?]]
      }
    } yield ln

  private def crushWith(
      f: (Int, Int) => Traversal[Game, (Pos, Option[Candy])]): State[Game, Int] =
    for {
      h  <- heightLn.extract
      w  <- widthLn.extract
      ps <- gets(f(h, w).getAll)
      xs <- ps.map(_._1).traverse[State[Game, ?], Int](crushPos)
    } yield xs.sum

  private def crushKind(kind: RegularCandy): State[Game, Int] =
    crushWith(kindTr(kind))

  private def crushLine(i: Int): State[Game, Int] =
    crushWith(lineTr(i))

  private def crushColumn(j: Int): State[Game, Int] =
    crushWith(columnTr(j))

  private def crushMin(n: Int): State[Game, Int] =
    crushWith(inarowTr(n))

  private def crushAll: State[Game, Int] =
    crushWith(allTr)
}
