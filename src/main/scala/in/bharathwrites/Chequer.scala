package in.bharathwrites

import scala.annotation.tailrec
import scala.util.control.NonFatal

object Chequer extends App {
  case class Position(x: Int, y: Int)
  case class Cell(p: Position, mark: Boolean)

  class Board(start_pos: Position) {

    private var cells: Map[Position, Cell] =
      (for {i <- 0 to 9; j <- 0 to 9 }
        yield {
          val p = Position(i, j)
          p -> Cell(p, mark = false)
        }).toMap

    var prev_pos: List[Position] = List()

    val moves: Array[Position => Option[Position]] = Array(
      markNorth, markSouth, markEast, markWest,
      markNorthEast, markNorthWest, markSouthEast, markSouthWest)

    val move_count = moves.length

    var last_unmarked: Position = _

    mark(start_pos.x, start_pos.y)

    def complete(): Boolean = cells.forall { case (pos, cell) => cell.mark}

    private def marked(p: Position): Boolean = cells(p).mark

    private def mark(i: Int, j: Int, curr_pos: Position = null): Option[Position] = {
      val new_p = Position(i, j)
      cells.get(new_p) match {
        case None =>
          //println(s"Got no cell for position $new_p")
          None
        case Some(c) =>
          marked(new_p) match {
            case true =>
              //println(s"$new_p is already marked")
              None
            case false =>
              if(curr_pos != null)
                prev_pos = curr_pos +: prev_pos

              val new_cell = c.copy(mark = true)
              //println(s"going to mark cell $new_cell")
              cells += (new_p -> new_cell)
              Option(new_p)
          }
      }
    }

    def unmark(p: Position): Unit = {
      //println(s"unmarking position $p")
      last_unmarked = p
      val cell = Cell(p, mark = false)
      cells += (p -> cell)
      prev_pos = prev_pos.tail
    }

    // treating the top left corner as origin
    private def markNorth(curr_pos: Position): Option[Position] = mark(curr_pos.x, curr_pos.y - 3, curr_pos)
    private def markSouth(curr_pos: Position): Option[Position] = mark(curr_pos.x, curr_pos.y + 3, curr_pos)
    private def markEast(curr_pos: Position): Option[Position] = mark(curr_pos.x + 3, curr_pos.y, curr_pos)
    private def markWest(curr_pos: Position): Option[Position] = mark(curr_pos.x - 3, curr_pos.y, curr_pos)

    private def markNorthEast(curr_pos: Position): Option[Position] = mark(curr_pos.x + 2, curr_pos.y - 2, curr_pos)
    private def markNorthWest(curr_pos: Position): Option[Position] = mark(curr_pos.x - 2, curr_pos.y - 2, curr_pos)
    private def markSouthEast(curr_pos: Position): Option[Position] = mark(curr_pos.x + 2, curr_pos.y + 2, curr_pos)
    private def markSouthWest(curr_pos: Position): Option[Position] = mark(curr_pos.x - 2, curr_pos.y + 2, curr_pos)
  }

  // similarly move all ways
  // after every move check if the board is complete
  // if all ways fail, then backtrack - use a different movement than the one tried last time. if the last move was north, then this time try south
  // if backtracking leads us back to initial position on the board then there is no way out. end it
  def compute(x: Int, y: Int): (Boolean, List[Position]) = {

    val p = Position(x, y)
    val board = new Board(p)
    //var counter = 0

    @tailrec def traverse(p: Position): Boolean = {

      @tailrec def mover(moveid: Int): (Option[Boolean], Option[Position]) = {
        if(moveid >= board.move_count) {
          // backtrack
          //println(s"previous positions = ${board.prev_pos}")
          board.prev_pos.isEmpty match {
            case true =>
              (Option(false), None)
            case false =>
              val prev_pos = board.prev_pos.head
              //println(s"backtracked to position $prev_pos")
              board.unmark(p)
              // unmarked p should be skipped in the next iter of traverse
              (None, Option(prev_pos))
          }
        } else {
          board.moves(moveid)(p) match {
            case None => // wrong move
              //println(s"incrementing moveid from $moveid")
              mover(moveid + 1)

            case Some(new_p) =>
              if(board.last_unmarked != null && new_p == board.last_unmarked) {
                // skip this one
                mover(moveid + 1)
              } else {
                board.last_unmarked = null
                if(board.complete()) {
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
        case (None, Some(pos)) =>
          //counter += 1
          //if(counter > 1000) System.exit(0)
          traverse(pos)
        case (Some(result), None) => result
        case _ => throw new Exception(s"how the hell did it land here")
      }
    }

    val result = traverse(p)
    (result, board.prev_pos.reverse)
  }

  def visit_all() = {
    val starting_positions: IndexedSeq[Position] = for {i <- 0 to 9; j <- 0 to 9 } yield Position(i, j)
    val visit_all = starting_positions.forall { start_pos =>
      compute(start_pos.x, start_pos.y)._1
    }
    val answer = if(visit_all) (Console.GREEN + "YES!" + Console.RESET) else (Console.RED + "NO :(" + Console.RESET)
    println(Console.BLUE + s"Is it possible for the pawn to visit all tiles on the board following the above rules? "
      + s"Answer: $answer")
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

