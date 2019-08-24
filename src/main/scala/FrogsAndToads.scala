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
import doodle.image.Image.Elements.Beside
import scala.util.{Failure, Success, Try}

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

  def checkState(index: Int, item: PuzzleState.Cell): Boolean = {
    if (index < 0 || index >= size) {
      false
    } else {
      board(index) == item
    }
  }

  def getBoardState(fort: Int): PuzzleState.Cell = {
    board(fort)
  }

  def slideToad(): Option[PuzzleState] = {
    val emptyIndex: Int = board.indexOf(Empty)
    if (checkState(emptyIndex + 1, Toad)) {
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
    if (checkState(emptyIndex - 1, Frog)) {
      Some(
        new PuzzleState(
          board
            .take(loc - 1)
            .++(Vector(Empty))
            .++(Vector(Frog))
            .++(board.takeRight(size - loc - 1)),
          loc - 1
        )
      )
    } else {
      None
    }
  }

  def jumpToad(): Option[PuzzleState] = {
    val emptyIndex: Int = board.indexOf(Empty)
    if (checkState(emptyIndex + 1, Frog) && checkState(emptyIndex + 2, Toad)) {
      Some(
        new PuzzleState(
          board
            .take(loc)
            .++(Vector(Toad))
            .++(Vector(Frog))
            .++(Vector(Empty))
            .++(board.takeRight(size - loc - 3)),
          loc + 2
        )
      )
    } else {
      None
    }
  }

  def jumpFrog(): Option[PuzzleState] = {
    val emptyIndex: Int = board.indexOf(Empty)
    if (checkState(emptyIndex - 1, Toad) && checkState(emptyIndex - 2, Frog)) {
      Some(
        new PuzzleState(
          board
            .take(loc - 2)
            .++(Vector(Empty))
            .++(Vector(Toad))
            .++(Vector(Frog))
            .++(board.takeRight(size - loc - 1)),
          loc - 2
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
  def solve(start: Seq[PuzzleState]): Seq[PuzzleState] = {
    // FIXME add your frogs and toads solver code here.
    println(start.last.getBoard())

    if (start.last.isTerminalState()) {
      println("Found solution")
      return start
    }

    val moves = Seq(
      start.last.slideFrog(),
      start.last.slideToad(),
      start.last.jumpFrog(),
      start.last.jumpToad()
    )

    moves.foreach(move => {
      if (move != None) {
        val next = move.getOrElse(start.last)
        solve(start :+ next)
      }
      throw new Exception("No solution found");
    })

    return start
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
    //FIXME add your code here to generate the animation frame sequence.
    val states = solve(Seq(start))

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
    * @return the [[Image]] object that represent the current [[PuzzleState]]
    */
  def builder(count: Int, state: PuzzleState): Image = {
    if (count < 0) {
      return Image.empty
    }

    state.getBoardState(count) match {
      case Toad  => builder(count - 1, state).beside(squareToad)
      case Frog  => builder(count - 1, state).beside(squareFrog)
      case Empty => builder(count - 1, state).beside(squareEmpty)
    }
  }

  /**
    * Create an animation of a solution to the frogs and toads puzzle, starting from the initial
    * [[PuzzleState]] and ending at the terminal [[PuzzleState]].
    *
    * @param frogs the number of frogs in the puzzle (between 1 and 10 inclusive)
    * @param toads the number of toads in the puzzle (between 1 and 10 inclusive)
    */
  def animate(frogs: Int, toads: Int): Seq[Image] =
    animate(
      PuzzleState(frogs, toads)
      // .slideFrog()
      // .jumpToad()
      // .slideToad()
      // .jumpFrog()
      // .jumpFrog()
      // .slideFrog()
      // .jumpToad()
      // .jumpToad()
      // .jumpToad()
      // .slideFrog()
      // .jumpFrog()
      // .jumpFrog()
      // .slideToad()
      // .jumpToad()
      // .slideFrog()
    )
}
