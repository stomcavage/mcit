/**
 * SchemeInterpreter is an interactive Scheme interpreter written in Scala.
 * 
 * Currently not implmented:
 * 	- character literals for control and meta characters
 *  - dotted lists
 *  - non-integer numbers
 * 
 * I am grateful to Jonathan Tang's "Write Yourself a Scheme in 48 Hours" for
 * guidance on this project. "Write Yourself a Scheme" is a how-to for 
 * writing a Scheme interpreter in Haskell. You can find it online at:
 * http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
 * 
 * @author  Steven Tomcavage (stomcava@seas.upenn.edu)
 * @version 1.0.0
 */
import scala.util.parsing.combinator._

// Use this case class to delineate Scheme symbols from Scheme strings
case class Symbol(name: String)

//---------------------------------------------------------------------------

/**
 * SchemeParsers provides the parsers needed for handling Scheme expressions.
 */
class SchemeParsers extends RegexParsers {
	
	// Define a helper parser to handle whitespace
	private def space = regex("[ \\n\\t]*".r)
	
	// Scheme boolean #t and #f translate to Scala's true and false
	private def bool : Parser[Boolean] = 
		("#t" | "#f") ^^ {case "#t" => true; case "#f" => false}
	
	// A character literal in Scheme has the prefix #\
	private def chr : Parser[Char] = """#\""" ~> ".".r ^^ {case s => s head}
	
	// A Scheme symbol allows alphanumeric chars, some symbols, and 
	// can't start with a digit
	private def id : Parser[Symbol] = 
		"""[a-zA-Z=*+/-<>!?][a-zA-Z=*+/-<>!?0-9]*""".r ^^ {case s => Symbol(s)}
	
	// This interpreter only accepts numbers as integers
	private def num : Parser[Int] = """-?\d+""".r ^^ {case s => s toInt}
	
	// A string can have any character except ", and is wrapped in "
	private def str : Parser[String] = 
		"\"" ~> """[^""]*""".r <~ "\"" ^^ {case s => s}

	// A Scheme list is a series of expressions wrapped in ()
	private def list : Parser[List[Any]] = 
		"(" ~> rep(expr) <~ ")" ^^ {s: List[Any] => s}
		
	// Provide the syntactic sugar to quote expressions
	private def quot : Parser[List[Any]] = 
		"'" ~> expr ^^ {case s => List(Symbol("quote"), s)}

	// A Scheme expression contains any of the above constructs
	private def expr : Parser[Any] = 
		bool | chr | id | num | str | list | quot ^^ {case s => s}
	
	/**
	 * Parses a Scheme expression into a corresponding Scala data structure
	 * @return ParserResult[Any]
	 */
	def parseExpr(input: String) : ParseResult[Any] = {
		parse(expr, input)
	}
}

//---------------------------------------------------------------------------

/**
 * SchemeInterpreter provides the Scheme REPL at the command line
 */
object SchemeInterpreter extends SchemeParsers {

	/**
	 * Starts the Scheme interpreter running
	 */
	def main(args : Array[String]) : Unit = {
		println
		println("-------------------------------------------------------------")
		println("| Welcome to the Scheme in Scala interpreter!               |")
		println("| This is a mini-implementation of Scheme written in Scala. |")
		println("-------------------------------------------------------------")
		usage
		repl
	}

	/**
	 * Prints commands that can be passed to the Scheme interpreter
	 */
	def usage : Unit = {
		println
		println("Available commands:")
		println("\t:quit will quit the interpreter")
		println("\t:help will print this list of commands")
		println
	}
	
	/** 
	 * Defines the REPL (read-eval-print loop) for the interpreter
	 */
	def repl() : Unit = {
		val input = Console.readLine("scheme> ")
		input match {
			case ":quit" => return
			case ":help" => usage; repl
			case _       => {				
				parseExpr(input) match {
					case Success(result, _) => println(stringVal(eval(result)))
					case Failure(msg, _)    => println("Failure: " + msg); None
					case Error(msg, _)      => println("Error: " + msg); None
				}
				repl
			}
		}
	}
	
	def eval(ast: Any) = {
		ast match {
			case x: Boolean           => x
			case x: Char              => x
			case x: Symbol            => x
			case x: Int               => x
			case x: String            => x
			case Symbol("quote") :: x => x head
//			case Symbol(func) :: args => 
		}
		/*
		 * To support:
		 * (define var value)
		 * (if cond true false)
		 * (lambda)
		 * storage and lookup of identifiers in global space
		 * function calls
		 * + - * = on ints
		 * cons, car, cdr
		 * boolean? number? string? pair?
		 * display
		 */
	}
	
	def stringVal(contents: Any) : String = {
		contents match {
			case x: Boolean   => if (x == true) "#t" else "#f"
			case x: Char      => """#\""" + x
			case x: Symbol    => x.name
			case x: Int       => x toString
			case x: String    => "\"" + x + "\""
			case x: List[Any] => "(" + x.map(stringVal(_)).reduceLeft(_ + " " + _) + ")"
			case _            => "Undefined value"
		}
	}
}
