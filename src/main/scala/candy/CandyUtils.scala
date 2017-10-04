package org.hablapps.candy

import scalaz._, Scalaz._
import monocle._

object CandyUtils {

  /* scala */

  object Colour {
    val ANSI_RESET = "\u001B[0m"
    val ANSI_BLACK = "\u001B[30m"
    val ANSI_RED = "\u001B[31m"
    val ANSI_GREEN = "\u001B[32m"
    val ANSI_YELLOW = "\u001B[33m"
    val ANSI_BLUE = "\u001B[34m"
    val ANSI_PURPLE = "\u001B[35m"
    val ANSI_CYAN = "\u001B[36m"
    val ANSI_WHITE = "\u001B[37m"
  }

  trait RNG {
    def nextInt: (Int, RNG)
  }

  object RNG {
    def simple(seed: Long): RNG = new RNG {
      def nextInt = {
        val seed2 = (seed*0x5DEECE66DL + 0xBL) &
                    ((1L << 48) - 1)
        ((seed2 >>> 16).asInstanceOf[Int],
         simple(seed2))
      }
    }
  }

  def cartesian(h: Int, w: Int): List[(Int, Int)] =
    (for {
      i <- 1 to h
      j <- 1 to w
    } yield (i, j)).toList

  /* scalaz */

  implicit class IfMHelper(mb: State[Game, Boolean]) {
    def ifM_(mu: State[Game, Unit]): State[Game, Unit] =
      mb.ifM(mu, ().point[State[Game, ?]])
  }

  // XXX: not in scalaz?
  def iterateWhile[A](a: A)(f: A => A, p: A => Boolean): List[A] =
    if (p(a)) a :: iterateWhile(f(a))(f, p) else Nil
}
