/*
 * This file is part of COMP332 Assignment 1.
 *
 * Copyright (C) 2019 Dominic Verity, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.mq.frogsandtoads

import doodle.core._
import doodle.syntax._
import doodle.image._

/**
  * A puzzle state is given as a 1-dimensional array of cell values.
  */
class PuzzleState private (
    board: Vector[PuzzleState.Cell],
    loc: Int
) {

  import PuzzleState._

  val size = board.size
  val emptyLoc = loc

  def isTerminalState(): Boolean = {
    board.slice(0, emptyLoc).forall(_ == Toad) &&
    board(emptyLoc) == Empty &&
    board.slice(emptyLoc + 1, size).forall(_ == Frog)
  }

  def isInitialState(): Boolean = {
    board.slice(0, emptyLoc).forall(_ == Frog) &&
    board(emptyLoc) == Empty &&
    board.slice(emptyLoc + 1, size).forall(_ == Toad)
  }

  def getBoard(): Vector[PuzzleState.Cell] = {
    board
  }

  def getBoardState(fort: Int): PuzzleState.Cell = {
    board(fort)
  }

  def slideToad(): Option[PuzzleState] = {
    val emptyIndex: Int = board.indexOf(Empty)
    if (board(emptyIndex + 1) == Toad) {
      Some(
        new PuzzleState(
          board
            .take(loc)
            .++(Vector(Toad))
            .++(Vector(Empty))
            .++(board.takeRight(size - loc - 2)),
          loc + 1
        )
      )
    } else {
      None
    }
  }

  def slideFrog(): Option[PuzzleState] = {
    val emptyIndex: Int = board.indexOf(Empty)
    if (board(emptyIndex - 1) == Frog) {
      Some(
        new PuzzleState(
          board
            .take(loc - 1)
            .++(Vector(Empty))
            .++(Vector(Frog))
            .++(board.takeRight(size - loc - 2)),
          loc + 1
        )
      )
    } else {
      None
    }
  }

  def jumpToad(): Option[PuzzleState] = {
    val emptyIndex: Int = board.indexOf(Empty)
    if (board(emptyIndex + 1) == Frog && board(emptyIndex + 2) == Toad) {
      Some(
        new PuzzleState(
          board
            .take(loc - 2)
            .++(Vector(Toad))
            .++(Vector(Empty))
            .++(Vector(Frog))
            .++(board.takeRight(size - loc - 1)),
          loc + 1
        )
      )
    } else {
      None
    }
  }

  def jumpFrog(): Option[PuzzleState] = {
    val emptyIndex: Int = board.indexOf(Empty)
    if (board(emptyIndex - 1) == Toad && board(emptyIndex - 2) == Frog) {
      Some(
        new PuzzleState(
          board
            .take(loc - 2)
            .++(Vector(Toad))
            .++(Vector(Empty))
            .++(Vector(Frog))
            .++(board.takeRight(size - loc - 1)),
          loc + 1
        )
      )
    } else {
      None
    }
  }
}

/**
  * Companion object for the [[PuzzleState]] class, provides a public constructor.
  */
object PuzzleState {

  /**
    * Case class for case objects to represent the possible contents of a
    * cell in the puzzle.
    */
  sealed abstract class Cell
  case object Frog extends Cell
  case object Toad extends Cell
  case object Empty extends Cell

  val squareFrog = Image.square(100).fillColor(Color.green).strokeWidth(2)
  val squareToad = Image.square(100).fillColor(Color.brown).strokeWidth(2)
  val squareEmpty = Image.square(100).fillColor(Color.white)
  val states = Seq()

  /**
    * Construct a [[PuzzleState]] object in the initial state for a
    * puzzle with specified numbers of frogs and toads.
    *
    * @param frogs number of frogs to place on the left of the [[PuzzleState]]
    * to be constructed
    * @param toads number of toads top place on the right of the [[PuzzleState]]
    * to be constructed
    */
  def apply(frogs: Int, toads: Int): PuzzleState = {
    if (frogs <= 0 || frogs > 10)
      throw new Exception("The number of frogs must be between 1 and 10.")

    if (toads <= 0 || toads > 10)
      throw new Exception("The number of frogs must be between 1 and 10.")

    new PuzzleState(
      Vector.fill(frogs)(Frog) ++ Vector(Empty) ++
        Vector.fill(toads)(Toad),
      frogs
    )
  }

  /**
    * Find a sequence of legal moves of the frogs and toads puzzle from a specified starting
    * [[PuzzleState]] to the terminal [[PuzzleState]].
    *
    * @param start the starting [[PuzzleState]]
    * @return the sequence of [[PuzzleState]] objects passed through in the transit from
    * state `start` to the terminal state (inclusive). Returns the empty sequence if no solution
    * is found.
    */
  def solve(start: PuzzleState): Seq[PuzzleState] = {
    // FIXME add your frogs and toads solver code here.
    println(start.getBoard())
    if (start.isTerminalState()) {
      return states
    }

    val moves = Seq(
      start.slideFrog(),
      start.slideToad(),
      start.jumpFrog(),
      start.jumpToad()
    )

    moves.foreach(move => {
      if (move != None) {
        val next = move.getOrElse(start)
        states :+ next
        solve(next)
      }
    })

    return states
  }

  /**
    * Call [[solve]] to generate a sequence of legal moves from a specified
    * starting [[PuzzleState]] to the terminal [[PuzzleState]]. Render each state in that solution as
    * an image and return the resulting sequence of images.
    *
    * @param start the starting [[PuzzleState]]
    * @return the sequence of [[Image]] objects depicting the sequence of puzzle states
    * passed through in the transit from the `start` state to the terminal state.
    */
  def animate(start: PuzzleState): Seq[Image] = {
    // FIXME add your code here to generate the animation frame sequence.
    val states = solve(start)

    states.map { state =>
      builder(state.size - 1, state)
    }
  }

  /**
    * [[builder]] is used to generate an image based on the vector of images from
    * the [[PuzzleState]] which is then put in the Sequence to animate
    *
    * @param count size of [[PuzzleState]]
    * @param board the current [[PuzzleState]]
    * @return [[Image]] object that represent the current [[PuzzleState]]
    */
  def builder(count: Int, board: PuzzleState): Image =
    count match {
      case 0 => squareFrog
      case n if board.getBoardState(n) == Toad =>
        val here = squareToad
        builder(n - 1, board).beside(here)

      case n if board.getBoardState(n) == Frog =>
        val here = squareFrog
        builder(n - 1, board).beside(here)

      case n if board.getBoardState(n) == Empty =>
        val here = squareEmpty
        builder(n - 1, board).beside(here)
    }

  /**
    * Create an animation of a solution to the frogs and toads puzzle, starting from the initial
    * [[PuzzleState]] and ending at the terminal [[PuzzleState]].
    *
    * @param frogs the number of frogs in the puzzle (between 1 and 10 inclusive)
    * @param toads the number of toads in the puzzle (between 1 and 10 inclusive)
    */
  def animate(frogs: Int, toads: Int): Seq[Image] =
    animate(PuzzleState(frogs, toads))
}
