package org.mq.frogsandtoads

object Main {
  def legalMoves(board: Seq[Int]): Seq[Seq[Int]] = {
    var moves = Seq[Seq[Int]]()
    for ((piece, pos) <- board.zipWithIndex) {
      val jumpmove = pos + (piece * 2)
      val move = pos + (piece)
      if (piece != 0) {
        if (!((jumpmove < 0) || (jumpmove >= board.size))) {
          if (board(jumpmove) == 0) {
            var t = board
            t = t.patch(pos, Seq(0), 1)
            t = t.patch(jumpmove, Seq(piece), 1)
            moves = moves :+ t
          }
        }
        if (!((move < 0) || (move >= board.size))) {
          if (board(move) == 0) {
            var t = board
            t = t.patch(pos, Seq(0), 1)
            t = t.patch(move, Seq(piece), 1)
            moves = moves :+ t
          }
        }
      }
    }
    return moves
  }

  def evalAll(
      current: Seq[Seq[Seq[Int]]],
      target: Seq[Int]
  ): Seq[Seq[Seq[Int]]] = {
    var next = Seq[Seq[Seq[Int]]]()
    for (a <- current) {
      val n = legalMoves(a.last)
      for (q <- n) {
        var t = a
        t = t :+ q
        if (q == target) {
          return Seq(t)
        }
        next = next :+ t
      }
    }
    return next
  }

  def solve(start: Seq[Int]): Seq[Seq[Int]] = {
    var temp = Seq(Seq(start))
    val end = start.reverse
    while (temp.last.last != end) {
      temp = evalAll(temp, end)
    }
    return temp.last
  }

  def main(args: Array[String]) {
    println(solve(Seq(1, 1, 1, 0, -1, -1, -1)))
  }
}
