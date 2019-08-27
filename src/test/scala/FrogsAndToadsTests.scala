/*
 * This file is part of COMP332 Assignment 1.
 *
 * Copyright (C) 2019 Dominic Verity, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Tests of the Frogs and Toads puzzle solver.
 * Uses the ScalaTest `FlatSpec` style for writing tests. See
 *
 *      http://www.scalatest.org/user_guide
 *
 * For more info on writing ScalaTest tests.
 */

package org.mq.frogsandtoads

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class FrogsAndToadsTests extends FlatSpec with Matchers {

  import PuzzleState._

  // Tests of an empty B-tree

  "A puzzle state with 5 frogs and 8 toads:" should
    "have 5 + 8 + 1 = 14 cells" in {
    assert(PuzzleState(5, 8).size == 14)
  }

  it should "have its empty cell at position 5" in {
    assertResult(5) {
      PuzzleState(5, 8).emptyLoc
    }
  }

  it should "be constructed in the initial puzzle state" in {
    assert(PuzzleState(5, 8).isInitialState())
  }

  it should "not be constructed in the terminal puzzle state" in {
    assert(!PuzzleState(5, 8).isTerminalState())
  }

  // FIXME Add more tests here.

  it should "resolve to a terminal state when solved" in {
    assert(solve(Seq(PuzzleState(5, 8))).last.isTerminalState())
  }

  "A puzzle state:" should "produce Exception when invoked with 11 frogs" in {
    intercept[Exception] {
      PuzzleState(11, 8)
    }
  }

  it should "produce Exception when invoked with 11 toads" in {
    intercept[Exception] {
      PuzzleState(8, 11)
    }
  }

  it should "produce Exception when invoked with 0 frogs" in {
    intercept[Exception] {
      PuzzleState(0, 8)
    }
  }

  it should "produce Exception when invoked with 0 toads" in {
    intercept[Exception] {
      PuzzleState(8, 0)
    }
  }

  "The Frogs:" should
    "stop moving if (Toad, Frog, Empty, Frog, Toad)" in {
    assert(createState(Vector(Toad, Frog, Empty, Frog, Toad)).isToadsTurn())
  }

  it should
    "stop moving if (Empty, Frog, Toad, Frog, Toad)" in {
    assert(createState(Vector(Empty, Frog, Toad, Frog, Toad)).isToadsTurn())
  }

  "The Toads:" should
    "stop moving if (Frog, Toad, Empty, Toad, Frog)" in {
    assert(createState(Vector(Frog, Toad, Empty, Toad, Frog)).isFrogsTurn())
  }

  it should
    "stop moving if (Frog, Toad, Frog, Toad, Empty)" in {
    assert(createState(Vector(Frog, Toad, Frog, Toad, Empty)).isFrogsTurn())
  }

}
