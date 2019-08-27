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

/**
  * A puzzle state is given as a 1-dimensional array of cell values.
  */
class PuzzleState private (board: Vector[PuzzleState.Cell], loc: Int) {

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

  // FIXME you might want to add more methods here.

  def isToadsTurn(): Boolean = {
    (checkState(emptyLoc - 1, Frog) ||
    emptyLoc <= 0) &&
    checkState(emptyLoc, Empty) &&
    checkState(emptyLoc + 1, Frog) &&
    checkState(emptyLoc + 2, Toad)
  }

  def isFrogsTurn(): Boolean = {
    checkState(emptyLoc - 2, Frog) &&
    checkState(emptyLoc - 1, Toad) &&
    checkState(emptyLoc, Empty) &&
    (checkState(emptyLoc + 1, Toad) ||
    size - 1 <= emptyLoc)
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

  def moveToad(): Option[PuzzleState] = {
    if (isFrogsTurn()) {
      return None
    }
    if (checkState(loc + 1, Frog) && checkState(loc + 2, Toad)) {
      val newState = new PuzzleState(
        board
          .take(loc)
          .++(Vector(Toad))
          .++(Vector(Frog))
          .++(Vector(Empty))
          .++(board.takeRight(size - loc - 3)),
        loc + 2
      )
      Some(newState)
    } else if (checkState(loc + 1, Toad)) {
      val newState = new PuzzleState(
        board
          .take(loc)
          .++(Vector(Toad))
          .++(Vector(Empty))
          .++(board.takeRight(size - loc - 2)),
        loc + 1
      )
      Some(newState)
    } else {
      None
    }
  }

  def moveFrog(): Option[PuzzleState] = {
    if (isToadsTurn()) {
      return None
    }
    if (checkState(loc - 1, Toad) && checkState(loc - 2, Frog)) {
      val newState = new PuzzleState(
        board
          .take(loc - 2)
          .++(Vector(Empty))
          .++(Vector(Toad))
          .++(Vector(Frog))
          .++(board.takeRight(size - loc - 1)),
        loc - 2
      )
      Some(newState)
    } else if (checkState(loc - 1, Frog)) {
      val newState = new PuzzleState(
        board
          .take(loc - 1)
          .++(Vector(Empty))
          .++(Vector(Frog))
          .++(board.takeRight(size - loc - 1)),
        loc - 1
      )
      //println("slideFrog")
      Some(newState)
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

  /**
    * Doodle elements for the Puzzle states, each is mapped with a
    * cell in the puzzle.
    */
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
    * Find a sequence of legal moves of the frogs and toads puzzle from a
    * specified starting [[PuzzleState]] to the terminal [[PuzzleState]].
    *
    * @param start the sequence of [[PuzzleState]] with the states leading to the solution
    * @param pref Defaults to true, if false, gives prefference to the toads.
    * @return the sequence of [[PuzzleState]] objects passed through in the
    * transit from state `start` to the terminal state (inclusive).
    * Returns the empty sequence if no solution is found.
    */
  def solve(start: Seq[PuzzleState], pref: Boolean = true): Seq[PuzzleState] = {
    // FIXME add your frogs and toads solver code here.
    if (start.last.isTerminalState()) {
      println("Found solution")
      return start
    }

    if (pref) {
      val moveFrogs = start.last.moveFrog()
      if (moveFrogs != None) {
        return solve(start :+ moveFrogs.getOrElse(start.last))
      }
    }

    val moveToads = start.last.moveToad()
    if (moveToads != None) {
      return solve(start :+ moveToads.getOrElse(start.last), false)
    }

    return solve(start)
  }

  /**
    * Call [[solve]] to generate a sequence of legal moves from a specified
    * starting [[PuzzleState]] to the terminal [[PuzzleState]]. Render each
    * state in that solution as an image and return the resulting sequence of images.
    *
    * @param start the starting [[PuzzleState]]
    * @return the sequence of [[Image]] objects depicting the sequence of puzzle states
    * passed through in the transit from the `start` state to the terminal state.
    */
  def animate(start: PuzzleState): Seq[Image] = {
    // FIXME add your code here to generate the animation frame sequence.
    val states = solve(Seq(start))

    states.map { state =>
      builder(state.size - 1, state)
    }
  }

  /**
    * Create an animation of a solution to the frogs and toads puzzle,
    * starting from the initial [[PuzzleState]] and ending at the
    * terminal [[PuzzleState]].
    *
    * @param frogs the number of frogs in the puzzle (between 1 and 10 inclusive)
    * @param toads the number of toads in the puzzle (between 1 and 10 inclusive)
    */
  def animate(frogs: Int, toads: Int): Seq[Image] = {
    animate(PuzzleState(frogs, toads))
  }

  //FIXME You might want to add some (private) auxiliary functions here.

  /**
    * [[createState]] is used to generate a custom puzzle state for
    * testing custom senarios
    *
    * @param vector composed of Frogs, Toads, or Empty
    * @return the [[PuzzleState]] of the vector to perform tests on
    */
  def createState(vector: Vector[PuzzleState.Cell]): PuzzleState = {
    new PuzzleState(vector, vector.indexOf(Empty))
  }

  /**
    * [[builder]] is used to generate an image based on the vector of images from
    * the [[PuzzleState]] which is then put in the Sequence to animate
    *
    * @param count size of [[PuzzleState]]
    * @param state the current [[PuzzleState]]
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
}
