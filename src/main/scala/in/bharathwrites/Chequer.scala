package in.bharathwrites

import scala.annotation.tailrec
import scala.util.control.NonFatal

object Chequer extends App {

  case class Position(x: Int, y: Int)
  case class Cell(p: Position, mark: Boolean)

  class Board(start_pos: Position) {

    val moves: Array[Position => Option[Position]] = Array(
      markNorth, markSouth, markEast, markWest,
      markNorthEast, markNorthWest, markSouthEast, markSouthWest)

    val pos: Array[Position => Position] = Array(
      northPos, southPos, eastPos, westPos,
      northEastPos, northWestPos, southEastPos, southWestPos)

    // this a single threaded program; so mutable variables are fine
    private var cells: Map[Position, Cell] =
    (for {i <- 0 to 9; j <- 0 to 9}
      yield {
        val p = Position(i, j)
        p -> Cell(p, mark = false)
      }).toMap

    val backtracking_threshold = 8 // totally max 8 moves are possible from any position. If num of attempts exceed this for a cell,
    // then, it is caught in a loop and the traversal has to be backtracked on level higher

    var backtrack_counter: (Position, Int) = _
    var prev_pos: List[Position] = List()
    val move_count = moves.length
    var last_unmarked: Position = _

    mark(start_pos, null)

    def complete(): Boolean = {
      val x = cells.forall { case (p, cell) => cell.mark }
      if (x) println(cells) //cells.foreach(println)
      x && prev_pos.size == 99
    }

    private def marked(p: Position): Boolean = cells(p).mark

    private def mark(new_pos: Position, curr_pos: Position): Option[Position] = {
      cells.get(new_pos) match {
        case None =>
          //println(s"Got no cell for position $new_p")
          None
        case Some(c) =>
          marked(new_pos) match {
            case true =>
              //println(s"$new_p is already marked")
              None
            case false =>
              if (curr_pos != null) {
                prev_pos = curr_pos +: prev_pos
              }

              val new_cell = c.copy(mark = true)
              println(s"going to mark $new_cell, prev_pos size = ${prev_pos.size}")
              cells += (new_pos -> new_cell)
              Option(new_pos)
          }
      }
    }

    def unmark(p: Position): Unit = {
      last_unmarked = p
      val cell = Cell(p, mark = false)
      cells += (p -> cell)
      prev_pos = prev_pos.tail
      println(s"unmarking $p, prev_pos size = ${prev_pos.size}")
    }

    // treating the top left corner as origin
    private def markNorth(curr_pos: Position): Option[Position] = mark(northPos(curr_pos), curr_pos)
    private def markSouth(curr_pos: Position): Option[Position] = mark(southPos(curr_pos), curr_pos)
    private def markEast(curr_pos: Position): Option[Position] = mark(eastPos(curr_pos), curr_pos)
    private def markWest(curr_pos: Position): Option[Position] = mark(westPos(curr_pos), curr_pos)

    private def markNorthEast(curr_pos: Position): Option[Position] = mark(northEastPos(curr_pos), curr_pos)
    private def markNorthWest(curr_pos: Position): Option[Position] = mark(northWestPos(curr_pos), curr_pos)
    private def markSouthEast(curr_pos: Position): Option[Position] = mark(southEastPos(curr_pos), curr_pos)
    private def markSouthWest(curr_pos: Position): Option[Position] = mark(southWestPos(curr_pos), curr_pos)

    private def northPos(curr_pos: Position) = Position(curr_pos.x, curr_pos.y - 3)
    private def southPos(curr_pos: Position) = Position(curr_pos.x, curr_pos.y + 3)
    private def eastPos(curr_pos: Position) = Position(curr_pos.x + 3, curr_pos.y)
    private def westPos(curr_pos: Position) = Position(curr_pos.x - 3, curr_pos.y)

    private def northEastPos(curr_pos: Position) = Position(curr_pos.x + 2, curr_pos.y - 2)
    private def northWestPos(curr_pos: Position) = Position(curr_pos.x - 2, curr_pos.y - 2)
    private def southEastPos(curr_pos: Position) = Position(curr_pos.x + 2, curr_pos.y + 2)
    private def southWestPos(curr_pos: Position) = Position(curr_pos.x - 2, curr_pos.y + 2)
  }

  // move all ways; after every move check if the board is complete
  // if all ways fail, then backtrack - use a different movement than the one used last time. if the last move was north, then this time skip north
  // if backtracking leads us back to initial position on the board then there is no way out. end it
  def compute(x: Int, y: Int): (Boolean, List[Position]) = {

    val p = Position(x, y)
    val board = new Board(p)

    // position 'p' is from where traversal on the board is continued
    @tailrec def traverse(p: Position): Boolean = {

      /*
      moveid: to pick one of the 8 movement directions
      first return element: if None then game is still ON; if Some(x) then if x = false, solution not possible. true = solution possible
      second return element: ending position from last move
       */
      @tailrec def mover(moveid: Int): (Option[Boolean], Option[Position]) = {
        if (moveid >= board.move_count) {

          // backtrack
          def backtrack(): (Option[Boolean], Option[Position]) = {
            board.prev_pos.isEmpty match {
              case true =>
                (Option(false), None)
              case false =>
                val prev_pos = board.prev_pos.head
                println(s"backtracked to $prev_pos")
                board.unmark(p)
                if(board.prev_pos.isEmpty) {
                  (Option(false), None)
                } else {
                  if (board.backtrack_counter != null && board.backtrack_counter._1 == prev_pos) {
                    println(s"backtrack status = ${board.backtrack_counter}")
                    board.backtrack_counter = (prev_pos, board.backtrack_counter._2 + 1)
                    if (board.backtrack_counter._2 > board.backtracking_threshold) {
                      backtrack()
                    }
                  } else {
                    board.backtrack_counter = (prev_pos, 1)
                  }
                  (None, Option(prev_pos)) // prev_pos should be skipped in the next iter of traverse
                }
            }
          }
          backtrack()

        } else {
          if (board.last_unmarked != null && board.pos(moveid)(p) == board.last_unmarked) {
            // skip this move
            mover(moveid + 1)
          } else {
            board.moves(moveid)(p) match {
              case None =>
                //println(s"incrementing moveid from $moveid")
                mover(moveid + 1)

              case Some(new_p) =>
                board.last_unmarked = null
                if (board.complete()) {
                  (Option(true), None)
                } else {
                  // game on
                  (None, Option(new_p))
                }
            }
          }
        }
      }
      mover(0) match {
        case (None, Some(pos)) => traverse(pos)
        case (Some(res), None) => res
        case _ => throw new Exception(s"how the hell did it land here")
      }
    }

    val result = traverse(p)
    (result, board.prev_pos.reverse)
  }

  def visit_all() = {
    val starting_positions: IndexedSeq[Position] = for {i <- 0 to 9; j <- 0 to 9} yield Position(i, j)
    val visits: IndexedSeq[(Boolean, List[Position])] = starting_positions.map { start_pos =>
      compute(start_pos.x, start_pos.y)
    }
    val visit_all = visits.forall(_._1)
    val routes: IndexedSeq[(Position, List[Position])] = starting_positions.zip(visits.map(x => x._2))
    val answer = if (visit_all) Console.GREEN + "YES!" + Console.RESET else Console.RED + "NO :(" + Console.RESET
    println(Console.BLUE + s"Is it possible for the pawn to visit all tiles on the board following the above rules? "
      + s"Answer: $answer")
    println(Console.BLUE + "Routes\n" + Console.RESET)
    routes.foreach { case (start, transit) =>
      println(Console.BLUE + s"Starting pos: $start, Transit size: ${transit.size}" + Console.GREEN + s" Transit: $transit")
    }
  }

  if (args.isEmpty) {
    visit_all()
  } else {
    try {
      val x = args(0).toInt
      val y = args(1).toInt
      compute(x, y) match {
        case (true, transit) => println(Console.GREEN + s"yes! from that initial position all cells can be traversed in the order: $transit" + Console.RESET)
        case (false, _) => println(Console.RED + "no! from that initial position not all cells can be traversed :(" + Console.RESET)
      }
    } catch {
      case NonFatal(e) =>
        e.printStackTrace()
        println("invalid input argument. enter valid x and y coordinates")
    }
  }
}

