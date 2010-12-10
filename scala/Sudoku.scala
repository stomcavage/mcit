/**
 * A sudoku solver written in Scala.
 * 
 * Requires a sudoku puzzle which is square and contains sub-squares whose
 * length and height are the square-root of the length and height of the puzzle.
 * 
 * @author Steven Tomcavage (stomcava@seas.upenn.edu)
 * @version 1.0
 */
import scala.swing._
import scala.swing.event._
import scala.swing.Swing._

object Sudoku {

	val PUZZLE_SIDE = 9
	val BOX_SIDE    = 3
	val CELL_VALUES = List("1", "2", "3", "4", "5", "6", "7", "8", "9")
	val BLANK_VALUE = "0"

	/**
	 * Runs the sudoku solver.
	 */
	def main (args: Array[String]) {
		val sudoku = loadSudoku(Console.readLine("Enter the filename of an easy Sudoku puzzle: "))
		println("\nInput:")
		printSudoku(sudoku)
		println("\nSolution:")
		printSudoku(solveEasyCells(sudoku))
		if (Console.readLine("\nWould you like to solve another Sudoku [y/n]? ") == "y") {
			main(new Array(0))
		}
		else {
			println("\nThanks for playing!")
		}
	}

	/**
	 * Returns a list of the column numbers in this Sudoku.
	 */
	def colNumbers : List[Int] = List.range(0, PUZZLE_SIDE)

	/**
	 * Returns a list of the row numbers in this Sudoku.
	 */
	def rowNumbers : List[Int] = List.range(0, PUZZLE_SIDE)
	
	/**
	 * Gets the sudoku rows from an input file. Does some simple error checking.
	 * Filters out blank lines.
	 */
	def loadSudoku (fileName: String) : List[List[String]] = {
		val sudokuIn = scala.io.Source.fromFile(fileName).getLines.toList map (
			line => {
				val cols = line.split("\\W+").toList.filter(cell => {
					CELL_VALUES.contains(cell) || cell == BLANK_VALUE
				})
				if (cols.size > 0) assert(cols.size == PUZZLE_SIDE) 
				cols
			}
		)
		val sudoku = sudokuIn filter (_.nonEmpty)
		assert(sudoku.size == PUZZLE_SIDE)
		sudoku
	}

	/**
	 * Gets a list of values from the row at rowNum in this Sudoku puzzle.
	 */
	def getRow (sudoku: List[List[String]], rowNum: Int) : List[String] = {
		require(rowNumbers contains rowNum)
		sudoku(rowNum)
	}

	/**
	 * Gets a list of values from the column at colNum in this Sudoku puzzle.
	 */
	def getCol (sudoku: List[List[String]], colNum: Int) : List[String] = {
		require(colNumbers contains colNum)
		colNumbers map(sudoku(_)(colNum))
	}
	
	/**
	 * Gets a list of values the box containing the cell at (row, col).
	 */
	def getBox (sudoku: List[List[String]], row: Int, col: Int) : List[String] = {
		require(rowNumbers contains row)
		require(colNumbers contains col)
		val startRow = row - (row % BOX_SIDE)
		val endRow   = startRow + BOX_SIDE
		val startCol = col - (col % BOX_SIDE)
		val endCol   = startCol + BOX_SIDE
		List.range(startRow, endRow) flatMap(rowNum => {
			List.range(startCol, endCol) map(colNum => {
				sudoku(rowNum)(colNum)
			})
		})
	}
	
	/**
	 * Returns a list of possible values for the blank cell at (row, column).
	 */
	def getPossibleValues (sudoku: List[List[String]], row: Int, col: Int) : List[String] = {
		require(sudoku(row)(col) == BLANK_VALUE)
		val values = 
			getRow(sudoku, row) ::: 
			getCol(sudoku, col) ::: 
			getBox(sudoku, row, col)
		CELL_VALUES diff(values.distinct filter(_ != BLANK_VALUE)) toList
	}

	/**
	 * Completes all the blank cells that have only one possible value.
	 * Repeats until no cells with only one possible value remain.
	 */
	def solveEasyCells (sudoku: List[List[String]]) : List[List[String]] = {
		val out = rowNumbers map (row => {
			colNumbers map (col => {
				val cellValue = sudoku(row)(col)
				cellValue match {
					case BLANK_VALUE => {						
						val values = getPossibleValues(sudoku, row, col) 
						values.size match {
							case 1 => values head
							case _ => cellValue
						}
					}
					case _ => cellValue
				}
			})
		})
		if (out equals(sudoku)) out
		else solveEasyCells(out)
	}

	/**
	 * Prints the Sudoku grid for the user to see.
	 */
	def printSudoku (sudoku: List[List[String]]) {
		rowNumbers map (row => {
			if (row % BOX_SIDE == 0) println
			colNumbers map (col => {
				if (col % BOX_SIDE == 0) print(' ')
				print(sudoku(row)(col))
			})
			println
		})
	}	
}
