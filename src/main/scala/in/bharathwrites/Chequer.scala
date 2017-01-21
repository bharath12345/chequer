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

    mark(start_pos.x, start_pos.y)

    def complete(): Boolean = cells.forall { case (pos, cell) => cell.mark}

    private def marked(p: Position): Boolean = cells(p).mark

    private def mark(i: Int, j: Int, curr_pos: Position = null): Option[Position] = {
      if(curr_pos != null)
        prev_pos = curr_pos +: prev_pos

      val new_p = Position(i, j)
      cells.get(new_p) match {
        case None =>
          None
        case Some(c) =>
          marked(new_p) match {
            case true =>
              None
            case false =>
              val new_cell = c.copy(mark = true)
              cells += (new_p -> new_cell)
              Option(new_p)
          }
      }
    }

    def unmark(p: Position): Unit = {
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

  if (args.isEmpty) {
    println(s"program needs starting position argument. enter the x and y coordinates of starting position")
  } else {
    try {
      val x = args(0).toInt
      val y = args(1).toInt
      compute(x, y) match {
        case true => println("yes! from that initial position all cells can be traversed")
        case false => println("no! from that initial position not all cells can be traversed")
      }
    } catch {
      case NonFatal(e) =>
        e.printStackTrace()
        println("invalid input argument. enter valid x and y coordinates")
    }
  }

  // similarly move all ways
  // after every move check if the board is complete
  // if all ways fail, then backtrack - use a different movement than the one tried last time. if the last move was north, then this time try south
  // if backtracking leads us back to initial position on the board then there is no way out. end it
  def compute(x: Int, y: Int): Boolean = {

    val p = Position(x, y)
    val board = new Board(p)

    @tailrec def traverse(p: Position): Boolean = {

      @tailrec def mover(moveid: Int): (Option[Boolean], Option[Position]) = {
        if(moveid >= board.move_count) {
          // backtrack
          board.prev_pos.isEmpty match {
            case true =>
              (Option(false), None)
            case false =>
              val prev_pos = board.prev_pos.head
              board.unmark(p)
              (None, Option(prev_pos))
          }
        } else {
          board.moves(moveid)(p) match {
            case None => // wrong move
              mover(moveid + 1)

            case Some(new_p) =>
              if(board.complete()) {
                (Option(true), None)
              } else {
                // game on
                (None, Option(new_p))
              }
          }
        }
      }
      mover(0) match {
        case (None, Some(pos)) => traverse(pos)
        case (Some(result), None) => result
        case _ => throw new Exception(s"how the hell did it land here")
      }
    }

    traverse(p)
  }
}

