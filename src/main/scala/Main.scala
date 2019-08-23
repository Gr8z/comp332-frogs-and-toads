package org.mq.frogsandtoads

object Main {
  def legalMoves(board: List[Int]): List[List[Int]] = {
    var moves = List[List[Int]]()
    for ((piece, pos) <- board.zipWithIndex) {
      val jumpmove = pos + (piece * 2)
      val move = pos + (piece)
      if (piece != 0) {
        if (!((jumpmove < 0) || (jumpmove >= board.size))) {
          if (board(jumpmove) == 0) {
            var t = board
            t = t.patch(pos, List(0), 1)
            t = t.patch(jumpmove, List(piece), 1)
            moves = moves :+ t
          }
        }
        if (!((move < 0) || (move >= board.size))) {
          if (board(move) == 0) {
            var t = board
            t = t.patch(pos, List(0), 1)
            t = t.patch(move, List(piece), 1)
            moves = moves :+ t
          }
        }
      }
    }
    return moves
  }

  def evalAll(
      current: List[List[List[Int]]],
      target: List[Int]
  ): List[List[List[Int]]] = {
    var next = List[List[List[Int]]]()
    for (a <- current) {
      val n = legalMoves(a.last)
      for (q <- n) {
        var t = a
        t = t :+ q
        if (q == target) {
          return List(t)
        }
        next = next :+ t
      }
    }
    return next
  }

  def solve(start: List[Int]): List[List[List[Int]]] = {
    var temp = List(List(start))
    val end = start.reverse
    while (temp.last.last != end) {
      temp = evalAll(temp, end)
    }
    return temp
  }

  def main(args: Array[String]) {
    println(solve(List(1, 1, 1, 0, -1, -1, -1)))
  }
}
