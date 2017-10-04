package org.hablapps.candy

import scala.io.StdIn.readLine
import scala.util.Random

import scalaz._, Scalaz._

import CandyLogic._
import CandyOptics._
import CandyUtils._

object CandyConsole extends App {

  val switchPat =
    """\s*\(\s*([0-9]+)\s*,\s*([0-9]+)\s*\)\s+(up|down|left|right)""".r

  var game: Game = Game("jesus", 2)

  def welcome: Unit =
    println("""
        |   _____                _          _____                _
        |  / ____|              | |        / ____|              | |
        | | |     __ _ _ __   __| |_   _  | |     _ __ _   _ ___| |__
        | | |    / _` | '_ \ / _` | | | | | |    | '__| | | / __| '_ \
        | | |___| (_| | | | | (_| | |_| | | |____| |  | |_| \__ \ | | |
        |  \_____\__,_|_| |_|\__,_|\__, |  \_____|_|   \__,_|___/_| |_|
        |                           __/ |
        |                          |___/
        |""".stripMargin)

  def loop: Unit = {
    readLine("candy> ").trim match {
      case "exit" => {
        game = leave.exec(game)
        showGame
      }
      case "" => loop
      case "leave" => {
        val res = leave.run(game)
        game = res._1
        if (!res._2) println("err: can't leave in the current state")
        showGame
        loop
      }
      case "play" => {
        val res = play.run(game)
        game = res._1
        if (res._2)
          showLevel
        else {
          println("err: can't play in the current state")
          showGame
        }
        loop
      }
      case switchPat(i, j, s) => {
        val pos = Pos(i.toInt, j.toInt)
        val dir = s match {
          case "up" => Up
          case "down" => Down
          case "left" => Left
          case "right" => Right
        }
        val res = switch(pos, dir).run(game)
        game = res._1
        res._2 match {
          case NotPlaying => println(s"err: not playing"); showGame; loop
          case InvalidMove => println(s"err: invalid switch: '$pos -> $dir'"); loop
          case YouLose => println("SUCH A LOSER..."); showGame; loop
          case YouWin => println("YOU WIN!"); showGame; loop
          case Ok => showLevel; loop
        }
      }
      case wrong => println(s"err: unknown order '$wrong'"); loop
    }
  }

  def showGame: Unit = {
    val up = Game.ups.get(game)
    val lv = Game.current.get(game)
    val st = if (levelOp.isEmpty(game)) "idle" else "playing"
    println(s"# Status: $st")
    println(s"# Level:  $lv")
    println(s"# Ups:    $up")
  }

  def showLevel: Unit = {
    val oh  = heightOp.getOption(game)
    val ow  = widthOp.getOption(game)
    val omx = matrixOp.getOption(game)
    val ots = targetScoreOp.getOption(game)
    val ocs = currentScoreOp.getOption(game)
    val otm = targetMovesOp.getOption(game)
    val ocm = currentMovesOp.getOption(game)
    (oh |@| ow |@| omx |@| ots |@| ocs |@| otm |@| ocm) { (h, w, mx, ts, cs, tm, cm) =>
      println()
      print("   ")
      println((1 to w).mkString("    "))
      println()
      (1 to h) foreach { i =>
        print(s"$i ")
        print(((1 to w) map { j =>
          mx.get(Pos(i, j)).join.fold("-") { c =>
            val s = if (c.toIcon.size == 2) s" ${c.toIcon}  " else c.toIcon
            s"${c.ansiColour}$s${Colour.ANSI_RESET}"
          }
        }).mkString(" "))
        println(); println()
      }
      println(s"# Score: ($cs / $ts)")
      println(s"# Moves: ($cm / $tm)")
    }
  }

  welcome
  loop
}
