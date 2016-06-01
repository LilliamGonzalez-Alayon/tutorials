package scala.lms.tutorial

import scala.virtualization.lms.common._
import scala.reflect.SourceContext
import scala.collection.mutable._
import scala.collection.immutable.Vector 
import scala.language.postfixOps
import java.awt.{Color}
import java.awt.image.BufferedImage
import java.awt.{Graphics2D, Color, Font, BasicStroke}
import java.awt.geom._
import java.io.File
import javax.imageio.ImageIO
import javax.swing.ImageIcon
import scala.math._

/* TODO
** Test basic schedule functions:
** (Produces a brigther image?)
**    ✓ Fuse sequencially (x and y) 
**    ✓ Fuse sequencially (split x and y, and then fuse back together) 
**    ✓ Split sequencially (only one Var) 
**    ✓ Split sequencially (two Vars) 
**    ✓ Split, fuse, split again 
**    ✓ Split when does not divide img dimesion (extent)
**    ✓ Tiling sequencially
**    ✓ Tiling sequencially with shortcut
**      - test more!
**
** _Track functions inside xyEval; get rid of ugly code replication! ...in progress
** ✓ Change the ._#s for readability 
** _ comments and organize code <- next!
** _Vectorize ...in progress <- next!
** _Parallelize - later! (Delite; work sequencially for now)
** _Generate C code
**/ 

class ShonanTest extends TutorialFunSuite { self =>
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Image Array
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  val under = ""

  val inImg: BufferedImage = ImageIO.read(new File("C:\\Users\\LilliamI\\Pictures\\Img\\cat.png"))
  //val inImg: BufferedImage = ImageIO.read(new File("C:\\Users\\LilliamI\\Pictures\\Img\\small.png"))
  val width: Int = inImg.getWidth 
  val height: Int = inImg.getHeight
  val imgArray = Array.ofDim[Int](width, height)
  // TODO: declare here?
  //val slArray: Array[Array[Int]]

  for (x <- 0 until width; y <- 0 until height) {
    val colour: Int = inImg.getRGB(x, y)
    imgArray(x)(y) = colour 
  } 

  val checkImage: BufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)

  for (x <- 0 until width; y <- 0 until height) {
    checkImage.setRGB(x, y, imgArray(x)(y))
  }

  println("Checking the imgArray! Saving the picture.")
  println("")
  ImageIO.write(checkImage, "png", new File("originalArray.png") )

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Classes
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
 	sealed abstract class Expr()

  	// Operands
  	case class Var(val nm: String) extends Expr {
	    // x -> 0, y -> 1, fused -> 2 (no longer x, y)
	    var code: Option[Int] = None 
	    // for a fused Var: initially false, then set to true by scheduling functions
	    var codeFused: Boolean = false 
	    // for a split Var: (first set equal to parent), inner -> 0, outer -> 1
	    var codeSplit: Option[Int] = None 
	    var codeInject: Boolean = false
  	}
	case class Input(val inputImg: BufferedImage, val vars: List[Expr]) extends Expr 
	case class Operand(val number: Double) extends Expr

	// Operators
	case class Sum(x: Expr, y: Expr) extends Expr
	case class Subst(x: Expr, y: Expr) extends Expr
	case class Mult(x: Expr, y: Expr) extends Expr  
	case class Div(x: Expr, y: Expr) extends Expr

	class ExprPrinter(expr: Expr) extends Expr {
		val print: Expr => String
		    = _ match {
		        case Var(nm) => nm.toString
		        case Input (inImg , vars) => "input ( " + vars + " )"
		        case Operand (value) => value.toString
		        case Sum(left, right) => "( " + print(left) + " + " + print(right) + " )"
		        case Subst(left, right) => "( " + print(left) + " - " + print(right) + " )"
		        case Mult(left, right) => "( " + print(left) + " * " + print(right) + " )"
		        case Div(left, right) => "( " + print(left) + " / " + print(right) + " )" 
		    }
	} 

	class PullVars (e : Expr) extends Expr { // FIX: only works if there is one Var in the expr!
		val parameters : Expr => String
			= _ match {
			    case Var( nm ) => nm
			    case Operand( number ) => ""
			    case Sum( left, right ) => {
			      parameters( left )  + "" +
			      parameters( right )

			    } case Subst( left, right ) => {
			      parameters( left ) + "" +
			      parameters( right )

			    } case Mult( left, right) => {
			      parameters( left )  + "" +
			      parameters( right ) 

			    } case Div( left, right) => {
			      parameters( left )  + "" +
			      parameters( right ) } 
			} 
	} 

/*
	def detectEdge(func: Func): (Boolean, which edge) = {
	// Edges matter only if it comes from an input,

	}
*/
	class Func(val nm: String, val args: List[Var], val expr: Expr) {
		/* args: List[Var] -- Original list of Vars before any scheduling.
		**
		** buffer: ListBuffer[Var] -- Scheduled Vars and ordering.
		**
		** bounds: Map[Var, (Int,Int)] -- Map the scheduled Var to its bound.
		** The Pair of Ints are later used by completeBounds function.
		**
		** If it finds a 0, then the Var is the original x or y (was not split
		** or fused). The bounds are then the image dimensions. If it finds a -1
		** further calculation is need. Or it can be the bound itself if finds a number > 0. 
		** The second Int will carry the splitFactor used in calculations during 
		** completeBounds call. By default it's -1.
		**
		** track: Map[Var, (ListBuffer[Var], Boolean, Boolean, Int)] -- Map Var
		** to a List of nested Vars (tracks how the original Vars have been split
		** or fused). When a new Var is created it will also have its entry in the Map.
		** (Not empty:).
		**
		** par: Map[Var, Boolean] -- Map a Var to true if has been parallelized,.
		** or false otherwise.
		**/ 

		// Var code to no depend on Strings like Var("x_inner")...
		// Restricting first Var to x (width) and second to y (height)...
		args(0).code = Some(0)
		args(1).code = Some(1)

		// Initially consume from no other function
		// var code: Option[Int] = None 
		var consumeFrom: Option[Func] = None

		// Ordering of vars from consumer
		var orderColMajor: Boolean = false

		/* 
		   Store producer scanline values from scanline function within the function
		   to avoid sending it to genloop as long list of parameters, and to avoid computing them
		   over and over again everytime genloop is recursively called (for each pixel!)
		*/

		// Store as pair of pairs or individually for readability!
		var extX: Option[Int] = None
		var extY: Option[Int] = None
		var stX: Option[Int] = None
		var stY: Option[Int] = None

		var prodW: Option[Int] = None
		var prodH: Option[Int] = None

		// don't have dimensions yet, need to wait....
		//var slArray = new Array[Array[Nothing]](2) 
		//var emptyArray =  Array[Array.empty[Int]]
		//var slArray = new Array[Array[Int]](2) = Array(null, null)

		var buffer = new ListBuffer[Var]()

		// Remove the color channel, since it has nothing to do with the order of evaluation.
		/* 
		    FIX -> We are removing the last Var, assuming it's always the third Var.
		    But right now we are always removing the last Var, which is incorrect if there is no color channel.
		*/
		def initialScheduleList(lst: List[Var]): ListBuffer[Var] = lst match {
		    case h::Nil => buffer
		    case h::t => {
		    buffer += h
		    initialScheduleList(t)
		  	}
		}

		initialScheduleList(args)
		println("Func Args: " + args)
		println("Initial Func Scheduled Args: " + buffer)
		println("")
		println(nm + " Function's Maps...")

		// Bounds Map -- Map[Var, (Int,Int)]
		val bounds = scala.collection.mutable.Map() ++ (buffer map (p => (p, (0,-1) )))
		println("Bounds Map: " + bounds)

		// Track Map -- Map[Var, (ListBuffer[Var], Boolean, Boolean, Int)] 
		// first Boolean split, second Boolean fuse
		val track = scala.collection.mutable.Map() ++ (buffer map (p => (p, (ListBuffer(p), false, false, -1) )))
		println("Track Map: " + track)

		// Parallel Map -- Map[Var, Boolean]
		val par = scala.collection.mutable.Map() ++ (buffer map (p => (p, (false))) )
		println("Parallel Map: " + par)
		println("")

		val inst: ExprPrinter = new ExprPrinter(expr)
		println("Evaluation of Func Expr: " + inst.print(expr))
		println("")

	} // end of Class Func 

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// 1. Scheduling Functions (Multi-stage Pipelines)											
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	def scanline(func: Func, array: Array[Array[Int]]): ((Int, Int), (Int, Int), (Int, Int)) = {
		/* Purpose - Know which section or "chunk" of the pipeline producer to compute
		** for consumer to use. Given an input object (an image with channels), 
		** the x and y Vars can be Exprs like x+1, y-1. 
		*/ 

		println(" ")
		println("***********************************************************")
		println("Testing Scanline Function")
		println("***********************************************************")
		println(" ")

		val imgH = array.length 
		val imgW = array(0).length 

		// identify which Var is in the given expression
		var varFlagX: Boolean = false
		var varFlagY: Boolean = false

		// save the start and extent of both x and y
		var startX: Int = 0 // by default it starts at first row 
		var extentX: Int = 1 // by default it is just one row
		var startY: Int = 0 // by default it starts at first col
		var extentY: Int = 1 // by default it is just one column

		// for X Vars top will change with a minus
		// for Y Vars top will change with a minus
		// if slide does occur then the final extent will need to have a + 1
		var slideX: Boolean = false
		var slideY: Boolean = false
		// horizontally...
		var slideAmountPositiveX: Int = 0 // How much I have moved right? 
		var slideAmountNegativeX: Int = 0 // How much I have moved left? 
		// vertically...
		var slideAmountPositiveY: Int = 0 // How much I have moved? up 
		var slideAmountNegativeY: Int = 0 // How much I have moved down? 

		println("func.nm: " + func.nm)
		println("func.buffer: " + func.buffer)
		// 0 - colMajor, 1 - rowMajor, 2 - fused outer loop
		val codeVar: Int = func.buffer(0).code.get
		println("codeVar of first Var in order: " + codeVar)

		def scan(ex: Expr): Unit = ex match {
			// outer cases
			case Div (e1: Expr, o: Operand) => {
				println("Found outer div case.")
				scan(e1) // go deeper if left is an Expr

			} case Sum (e1: Expr, e2: Expr) => {
				println("Found outer sum case.")
				scan(e1) // go deeper if left is an Expr
				scan(e2) // go deeper if left is an Expr

			} case Subst (e1: Expr, e2: Expr) => {
				println("Found outer subst case.")
				scan(e1) // go deeper if left is an Expr
				scan(e2) // go deeper if left is an Expr

			} case Input (inImg, vars) => { 
				// the vars shift in X and Y if any
				// inner cases input (Sum and Subst, no division or multiplication)
				println("Found input case.")  

				def matchInnerCases(varsElem: Expr): Unit = varsElem match { 
					case Var(nm) => {
					  	// just a Var, no shift
					  	// doesn't change the extent or the start

					  	// means that the start default is really zero!
					  	// if all variable are Expr then the start if shifted
					  	println("Found inner Var case.")

					} case Sum(vr@Var(_), op@Operand(_)) => {
					  	// check if it's x or y (no outer or inner vars just yet)
					    println("Found inner sum case.")
					    /* 
					       If one of the Inputs has x+1, and another Input has x+2, 
					       the extent is not is not 3, it's 2. 
					       If one Input has x+1, and another Input has x+1,
					       the extent is not 2, it's 1.
					       They cannot be added on top of each other.
					    */

					    if (vr.code.get == 0) { // moving right
					    	// extent
					    	var int = ((op.number - slideAmountPositiveX).abs).toInt
					    	slideAmountPositiveX = int
					    	slideX = true
					    } else if (vr.code.get == 1) { // moving up
					    	// extent
					    	var int = ((op.number - slideAmountPositiveY).abs).toInt
					    	slideAmountPositiveY = int
					    	slideY = true
					    } 
					    // if channel is color, ignore

						println("slideAmountPositiveY: " + slideAmountPositiveY)

					} case Subst(vr@Var(_), op@Operand(_)) => {
					    // check if it's x or y (no outer or inner vars just yet)
						// top will change
						println("Found inner minus case.")

						if (vr.code.get == 0) { // moving left
							// extent
							var int = ((op.number - slideAmountNegativeX).abs).toInt
							slideAmountNegativeX = int
							// top
							var top = ((op.number - startX).abs).toInt
							startX = top
							slideX = true
						} else if (vr.code.get == 1) { // moving down
							//extent
							var int = ((op.number - slideAmountNegativeY).abs).toInt
							slideAmountNegativeY = int
							// top
							var top = ((op.number - startY).abs).toInt
							startY = top
							slideY = true
						} 
						// if channel is color ignore
					}
				} // end of def matchInnerCases

				matchInnerCases(vars(0)) 
				matchInnerCases(vars(1))

			} // end of cases
		} // end of def scan

		scan(func.expr)

		println(" ")
		println("slideAmount X: (positive) " + slideAmountPositiveX + " , (negative) " + slideAmountNegativeX)
		println("slideAmount Y: (positive) " + slideAmountPositiveY + " , (negative) " + slideAmountNegativeY)

		extentX = (slideAmountPositiveX + slideAmountNegativeX)
		extentY = (slideAmountPositiveY + slideAmountNegativeY)

		if (slideX) 
			extentX = extentX + 1
		if (slideY)
		    extentY = extentY + 1 

		println("topX and extentX: " + startX + " , " + extentX)
		println("topY and extentY: " + startY + " , " + extentY)
		println(" ")

		// calculate the size of scanline (with no optimization)
		var slH: Int = 0
		var slW: Int = 0
		
		if (func.orderColMajor) {
			// width
			if (extentX == 0) 
				slW = 1
			else 
				slW = extentX
			
			// height
			if (slideAmountPositiveY + slideAmountPositiveY == 0) {
				slH = imgH
			} else {
				var buffer = slideAmountPositiveY + slideAmountNegativeY
				slH = imgH + buffer
			}
			
		} else if (!func.orderColMajor) {
			// width
			if (slideAmountPositiveX + slideAmountPositiveX == 0) {
				slW = imgW
			} else {
				var buffer = slideAmountPositiveX + slideAmountNegativeX
				slW = imgW + buffer
			}

			// height
			if (extentY == 0) 
				slH = 1
			else 
				slH = extentY
		} 
		println("func.isColMajor: " + func.orderColMajor)
		println("Scanline Height: " + slH)
		println("Scanline Width: " + slW)
		println(" ")

		println("***********************************************************")
		println("End of Scanline Function Test")
		println("***********************************************************")
		println(" ")

		((startX, extentX), (startY, extentY), (slH, slW))

	} // end of def scanline

/*
  	def testFuncSchedule(func: Func, ) : Int = {
  		/* Determine the domain scheduling of a function. 
    	** We need it to know how are we going to detect that we need to calculate another scanline
    	**/ 
  	} //end of testFuncSchedule
*/

	def computeAt(consumer: Func, producer: Func, variable: Var, array: Array[Array[Int]]): Unit = {
		/* To inject the two for loops of producer into consumer's loopMap.
		** User will call it, and then call realize on the consumer only, not the producer.
		** User must identify the code of the Var before calling the function, 
		** else it will throw an exception.
		*/ 

		consumer.consumeFrom = Some(producer)

		// x -> 0, y -> 1
		if (variable.code == None)
			throw new IllegalArgumentException("Compute At Var does not have code and rowMajor or colMajor ordering can't be determined...")
		else if (variable.code.get == 0)
			consumer.orderColMajor = true

		// Call scanline
		val slValues: ((Int, Int), (Int, Int), (Int, Int)) = scanline(consumer, array)
		consumer.stX = Some((slValues._1)._1)
		consumer.extX = Some((slValues._1)._2) 
		consumer.stY = Some((slValues._2)._1)  
		consumer.extY = Some((slValues._2)._2)  

	} // end of def computeAt 

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// 2. Scheduling Functions (Order of Domain Evaluation)
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	// insert the fused Var along the Vars that are fused.
	def insert[A](e: ListBuffer[A], n: Int, ls: ListBuffer[A]): ListBuffer[A] = {
		ls.splitAt(n) match {
	  		case (pre, post) => pre ++ e ++ post }
	}

// TODO: order doesn't have to be a ListBuffer 
    def reorder (func: Func, order: ListBuffer[Var]): Unit = { 
		/* gradient.reorder(x_inner, y_inner, x_outer, y_outer); [innermost -> outermost]
		** List works outermost -> innermost
		** 
		** Check if the order ListBuffer has the same length as the func's ListBuffer,
		** and has the same Vars (is a subset).
		** If not, an illegal arguments exception is thrown.
		*/ 

		println(" -> Reorder <- ")
		println("func.buffer before reordering: " + func.buffer)

		// ScheduleList function at Func class takes care of dropping the color channel Var.
		val newOrderMap = Map() ++ (order map (p => (p, order.indexOf(p)) )) 
		//println("New order of buffer: " + newOrderMap)

		val oldOrderMap = Map() ++ (func.buffer map (p => (p, func.buffer.indexOf(p)) ))
		//println("Original order of buffer: " + oldOrderMap)

		val isSubset = newOrderMap.keySet subsetOf oldOrderMap.keySet
		val equalSz = newOrderMap.size == oldOrderMap.size

		if (isSubset && equalSz)
			println("Correct number of arguments in reorder.") // number and also args...
		else
			throw new IllegalArgumentException("The list of arguments in the reorder is incorrect...")

		func.buffer = order
		println("Reorder func.buffer: " + func.buffer)
		println(" -> End of Reorder <- ")
      	println(" ")

    } //end of reorder func

    def fuse (func: Func, variable1: Var, variable2: Var, fused: Var): Unit = { 
		/* func.buffer -> Inserts the new fused Var into the func.buffer, and removes the previous Vars.
		** func.bounds -> Inserts the new fused Var into func.bounds, and removes the previous Vars.
		** func.track -> Inserts a new entry into the func.track, that represents the new fused variable.
		** The var1 and var2 two entries of this map are modified, the second boolean (representing
		** if the Var has been fused) set to true, anf the listbuffer will contain the new fused Var.
		**/ 

		fused.code = Some(2)

		println(" -> Fuse <- ")
		println("Initial func.buffer = " + func.buffer)
		println("Initial func.bounds = " + func.bounds)
		println("Initial func.track = " + func.track)
		println("")
		val index1 = func.buffer.indexOf(variable1)
		val index2 = func.buffer.indexOf(variable2)

		println(variable1 + " is at index " + index1 + " and " + variable2 + " is at index " + index2)

		func.buffer = insert(ListBuffer(fused), index1, func.buffer)
		func.bounds += (fused -> (-1,-1))

		// What if it's fusing a previously split Var?
		val value1 = func.track(variable1)
		val value2 = func.track(variable2)
		// split Var bool
		val prevBool1 = value1._2 
		val prevBool2 = value2._2

/* 		TODO: the prevBool should always be false. If the var was previously split, then the Var no longer "exists",
		the two new split Vars do. If I am going to fused it, it's because it hasn't been split. 
*/
		func.track += (fused -> (ListBuffer(), false, false, -1))
		func.track(variable1) = (ListBuffer(fused), prevBool1, true, -1) 
		func.track(variable2) = (ListBuffer(fused), prevBool2, true, -1)

		println("Intermediate func.buffer = " + func.buffer)
		println("Intermediate func.bounds = " + func.bounds)
		println("Intermediate func.tracks = " + func.track)
		println("")

		func.buffer -= variable1
		func.buffer -= variable2
		func.bounds -= variable1
		func.bounds -= variable2

		println(variable1 + " and " + variable2 + " where fused into: " + fused) 
		println("New func.buffer: " + func.buffer)
		println("New func.bounds: " + func.bounds)
		println("New func.track: " + func.track)
		println(" -> End of Fuse <- ")
		println("")

    } //end of fuse func

    def split(func: Func, variable: Var, outer: Var, inner: Var, splitFactor: Int): Unit = {
		/* func.buffer -> Inserts the new two Vars into the func.buffer, and removes the previous Var.
		** func.bounds -> Inserts the new split Vars into func.bounds, and removes the previous Var.
		** The inner split Var's bounds is know at this point. It will be the splitFactor.
		** Unlike the outer's bound which depends both on the splitFactor and the image dimensions.
		** The split factor will be stored in the second Int of the Map for further computation
		** during completeBounds function call.
		** func.track -> Inserts two new entries into the func.track, that represent the new split variables.
		** The original variable's entry in this map is modified, the first boolean (representing
		** if the Var has been split) set to true.
		**/ 

		// The split Var will have the same code as it's parent Var.
		inner.code = variable.code
		outer.code = variable.code
		// But the codeSplit will indicate if it's a the inner or outer split Var.
		inner.codeSplit = Some(0)
		outer.codeSplit = Some(1)

		println(" -> Split <- ")
		println("Initial func.buffer = " + func.buffer)
		println("Initial func.bounds = " + func.bounds)
		println("Initial func.track = " + func.track)
		println("")

		val index1 = func.buffer.indexOf(variable)
		println(variable + " is at index " + index1)

		func.buffer = insert(ListBuffer(outer,inner), index1, func.buffer)
		func.bounds += (outer -> (-1,splitFactor))
		func.bounds += (inner -> (splitFactor,-1))

		// What if it's split a previously fused Var?
		val value = func.track(variable)
		println("Value associated with variable1 key: " + value)
		println("boolean: " + value._3)
		val prevBool = value._3

/* 		Remember: limiting the nested numbers of Vars to only two? Which cases can they have more than two? 
		No limitations here; func.track(variable) = (ListBuffer(outer,inner), true, prevBool, splitFactor)
		it's done on each split Var; which can only be split into two vars.
*/

/*      TODO: This way around is true. We can split back a fused Var. 
		However, the problem now is that when we want to assign the variable.code; fused Var have code 2, not the x or y.
		The info for fuse has to be moved, it can't be on code. Need to add codeFuse to Var case class.
*/
		func.track(variable) = (ListBuffer(outer,inner), true, prevBool, splitFactor)
		func.track += (outer -> (ListBuffer(), false, false, splitFactor))
		func.track += (inner -> (ListBuffer(), false, false, splitFactor))

		println("Intermediate func.buffer = " + func.buffer)
		println("Intermediate func.bounds = " + func.bounds)
		println("Intermediate func.tracks = " + func.track)
		println("")

		func.buffer -= variable
		func.bounds -= variable

		println(variable + " was split into: " + outer + ", " + inner)
		println("New func.buffer: " + func.buffer)
		println("New func.bounds: " + func.bounds)
		println("New func.track: " + func.track)
		println(" -> End of Split <- ")
		println(" ")

    } //end of split func

    def tile(func: Func, variable1: Var, variable2: Var, outer1: Var, outer2: Var, inner1: Var, inner2: Var, splitFactor1: Int, splitFactor2: Int): Unit = { 
		// This function is a shortcut, for two splits and a reorder.

		println(" -> Tile <- ")
		// Split 1
		val splitTest = split(func, variable2, outer2, inner2, splitFactor2)
		// Split 2
		val splitTest2 = split(func, variable1, outer1, inner1, splitFactor1)
		// Reorder
		val reorderTest = reorder(func, ListBuffer(outer1, outer2, inner1, inner2))

		println(" -> End of Tile <- ")
		println(" ")
    } 

    def parallel(func: Func, variable: Var): Unit = { 
		// Maps a Var to a Boolean that represents if it has been parallelized.
		
		println(" -> Parallelizing <- ")
		func.par += (variable -> true)
		println("New func.par: " + func.par)
		println(" ")
    } 

/* TODO: Vectorization */
    //how to support both ways of vectorizing, the short and long way?
    def vectorize(func: Func, variable: Var, splitFactor: Int): Unit = { 
		/* gradient.vectorize(x, 4);
		** which is equivalent to:
		** gradient.split(x, x, x_inner, 4);
		** gradient.vectorize(x_inner);
		** The inner loop of the splitted Var is replaced by the vectorized computation.
		**/

		println(" -> Vectorizing <- ")

		// Where to put inner and outer Vars?
		val vec_outer = new Var("vec_outer")
		val vec_inner = new Var("vec_inner") 

		split(func, variable, vec_outer, vec_inner, splitFactor)

		/* Bounds need calculation 
		this happens inside the split call
		func.bounds += (vec_outer -> (-1,splitFactor))
		func.track += (vec_outer -> (ListBuffer(), false, false, splitFactor)) */

		// Remove the inner, since the vector will compute all the data at once.
		func.buffer -= vec_inner
		func.bounds -= vec_inner
		func.track -= vec_inner

		// Get the prevBool...
		// What if it's split a previously fused Var?
		val value = func.track(variable)
		println("Value associated with variable1 key: " + value)
		println("boolean: " + value._3)
		val prevBool = value._3
		func.track(variable) = (ListBuffer(vec_outer), true, prevBool, splitFactor) // inefficient!
		// Remove from ListBuffer in: func.track(variable) = (ListBuffer(outer,inner), true, prevBool, splitFactor).

		println(" -> End of Tile <- ")
		println(" ")

    } // end of vectorize def

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Realize Helper Functions (...with no Rep Types)
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    def completeBounds(func: Func, w: Int, h: Int): Unit = {
		/* Calculate the missing bounds in func.bounds.
		** It's called from within realize.
		** The outer loop of a split Var and a fused Var depend on image dimension.
		** the image dimension are arguments on realize.
		**/

		println("Completing Bounds...")
		println("Current func.bounds map: " + func.bounds)

		func.bounds foreach ((p) => {
	
			val key = p._1
			println("key: " + key)
			val pair = p._2
			val needCalc = pair._1
			val split = pair._2

			//none split or fused need only image dimensions
			if (needCalc == 0) {
				println(key + " has not been split or fused. Only needs image dimensions.")

				if (key.code.get == 0) 
					func.bounds(key) = (w, -1)
				else if (key.code.get == 1) 
					func.bounds(key) = (h, -1)

			} else if (needCalc == -1) {
			    // Fused variables, and split outter...
			    println("Bounds need to be calculated.")

			    if (split == -1) {

					println(key + " was fused.")
					val newBound = w * h
					func.bounds(key) = (newBound, -1)

			    } else if (split != -1) {
			      	println(key + " was split.")
			        // What if it is vectorized?

			        if (key.code.get == 0) {
						// Verify if the split divides the extent.
						if ((w % split) == 0) {

							println("split: " + split)
							val newBound = (w / split)
							println("w: " + w + ", newBound: " + newBound)
							func.bounds(key) = (newBound, -1)

						} else {

							val newBound: Int = ((w + split - 1)/ split)
							println("w: " + w + ", newBound: " + newBound)
							func.bounds(key) = (newBound, -1)
						}

			        } else if (key.code.get == 1) { 
			          // Verify if the split divides the extent.

						if ((h % split) == 0) {

							println("split: " + split)
							val newBound = (h / split)
							println("h: " + h + ", newBound: " + newBound)
							func.bounds(key) = (newBound, -1)

						} else {

							val newBound: Int = ((h + split - 1)/ split)
							println("h: " + h + ", newBound: " + newBound)
							func.bounds(key) = (newBound, -1)
						}
			        } // end of if (key.code.get == 0) {
/* TODO   
					  // else if (x_or_y == "v") { // TODO: Vector codes???
			          // we vectorized only x? for now...
			          // can we vectorize the y?
			          println("split: " + split)
			          val newBound = (w / split)
			          println("h: " + h + ", newBound: " + newBound)
			          func.bounds(key) = (newBound, -1)
			        } 
*/  
			    } // end of if (split == -1) { 
			} // end of if (needCalc == 0) {
		})  // end of foreach statement
		                                  
		println("New bounds map: " + func.bounds)
		println("Bounds Completed...")
		println("")

    } //end of def completeBounds

/*    
	def finalList (xs: List[Var], flist: ListBuffer[(Var, Option[Int])], fn: Func, position: Int, scheduling: Var): Unit = { 
		/* Called from within realize function.
		** It's purpose is to create a final list with the scheduled order and the completed bounds.
		** Send scheduling Vars separately because I will loose them as finalList recurses.
		**/

/* TODO: Finish cases!
		-> If Vars have not been split or fused:
			Insert at second position, and third position ...
		-> If Vars have been split (tiling):
			Insert after two outer Vars ...
		-> If Vars have been fused (vectorization):
*/

		xs match {
			case Nil => 
				println("Final List with order and bounds: " + flist)
			case x::xs1 => {
				if ( position == 2 || position == 3 ) {
				// Insert Vars of injected loops
				// TODO: Testing first Var may not be enough
				// So, create a function that precomputes the scheduling scheme before hand!

					if ( scheduling.codeFused == false && scheduling.codeSplit == None ) {
						// TODO: Var codes?
						val injectVar = new Var("Injected Var: " + position.toString)
						injectVar.codeInject = true
						flist += { (injectVar, None) }
					}
				} else { 
					// Insert Vars of the consumer
					val varCurr = x
					val bound = fn.bounds(varCurr)
					val bound1 = bound._1
					flist += { (x, Some(bound1)) }
				} // end of if ( position == 2 || position == 3 ) {

				var incrementedPos = position + 1
		    	finalList(xs1, flist, fn, incrementedPos, scheduling)
			}
		} 

    } // end of finalList
*/
	def finalList (xs: List[Var], flist: ListBuffer[(Var, Int, Boolean)], fn: Func): Unit = { 
		/* Called from within realize function.
		** It's purpose is to create a final list with the scheduled order and the completed bounds.
		*/

		xs match {
			case Nil => {
				println("Final List with order and bounds: " + flist)
			} case x::xs1 => {
				val varCurr = x
				val bound = fn.bounds(varCurr)
				val bound1 = bound._1

				flist += { (x, bound1, false) }
				finalList(xs1,flist,fn)
			}
		} 

    } // end of finalList

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Halide Tutorials
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	val x = new Var("x")
	val y = new Var("y") 
	var c = new Var("c")
	val gradientExpr = new Sum(x , y)

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// 1. Brighter
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	// The value's list stays the same for the exprPrinter
	val value = new Input (inImg, List(x, y, c))
	val brighterExpr = new Mult (value, new Operand(1.2))

	// The function's list gets modified with the schedule
	// Restrict to (x, y), in that order always???
	val brighter = new Func("brighter", List(x, y, c), brighterExpr)  

	// Blur
	// Operator Overload
	// Arrayout of bounds because of the x+1, x-1

	//case class no new
	val temp1 = Input (inImg, List(Subst(x, Operand(1)), y, c) )
	val temp2 = Input (inImg, List(x, y, c))
	val temp3 = Input (inImg, List(Sum(x, Operand(1)), y, c) )

	val hExpr1 = Sum (temp2, temp3)
	val hExpr2 = Sum (temp1, hExpr1)
	val hExpr3 = Div (hExpr2, Operand(3.0))
	val hBlur = new Func("horizontal_blur" , List(x, y, c), hExpr3)

	val temp4 = Input (inImg, List(x, Subst(y, Operand(1)), c) )
	val temp5 = Input (inImg, List(x, y, c))
	val temp6 = Input (inImg, List(x, Sum(y, Operand(1)), c) )
	//val temp6 = Input (inImg, List(x, y, c) )

	val hExpr4 = Sum (temp5, temp6)
	val hExpr5 = Sum (temp4, hExpr4)
	val hExpr6 = Div (hExpr5, Operand(3.0))
	val vBlur = new Func("vertical_blur" , List(x, y, c), hExpr6)

	// Test Basic Scheduling Functions 
	println("Applying a Schedule...")

	/*// Split two variables... (does not work when factor does not divide the extent)
	// Split 1
	val x_inner = new Var("x_inner")
	val x_outer = new Var("x_outer")
	val splitTest = split(brighter, x, x_outer, x_inner, 7)
	// Split 2
	val y_inner = new Var("y_inner")
	val y_outer = new Var("y_outer")
	val splitTest2 = split(brighter, y, y_outer, y_inner, 7)
	// Reorder
	val reorderTest = reorder(brighter, ListBuffer(y_outer, x_outer, y_inner, x_inner))*/

	/*// Split only one variable... (works when factor does not divide the extent)
	// Split
	val x_inner = new Var("x_inner")
	val x_outer = new Var("x_outer")
	val splitTest = split(hBlur, x, x_outer, x_inner, 5)*/

	/*// Split, fuse and split again... 
	// Split
	val x_inner = new Var("x_inner")
	val x_outer = new Var("x_outer")
	val splitTest = split(brighter, x, x_outer, x_inner, 4)
	// Fuse
	val fused = new Var("fused")
	val fuseTest = fuse(brighter, x_outer, x_inner, fused)
	// Split again
	val y_inner = new Var("y_inner")
	val y_outer = new Var("y_outer")
	val splitTest2 = split(brighter, fused, x_outer, x_inner, 4)
	val splitTest3 = split(brighter, y, y_outer, y_inner, 4)
	// Reorder
	val reorderTest = reorder(brighter, ListBuffer(y_outer, x_outer, y_inner, x_inner))*/

	/*// Fuse x and y variables
	val fused = new Var("fused")
	val fuseTest = fuse(brighter, x, y, fused)*/

	/*// Split x and y variables, then fuse back to x and y
	 // Split 1
	val x_inner = new Var("x_inner")
	val x_outer = new Var("x_outer")
	val splitTest = split(brighter, x, x_outer, x_inner, 4)
	// Split 2
	val y_inner = new Var("y_inner")
	val y_outer = new Var("y_outer")
	val splitTest2 = split(brighter, y, y_outer, y_inner, 4)
	// Reorder
	val reorderTest = reorder(brighter, ListBuffer(y_outer, x_outer, y_inner, x_inner))
	// Fuse back together
	val fused = new Var("fused")
	val fuseTest1 = fuse(brighter, x_inner, x_outer, x)
	val fuseTest2 = fuse(brighter, y_inner, y_outer, y)*/

	/*// Tiling
	val x_inner = new Var("x_inner")
	val x_outer = new Var("x_outer") 
	val y_inner = new Var("y_inner")
	val y_outer = new Var("y_outer")
	val tilingTest = tile(brighter, y, x, y_outer, x_outer, y_inner, x_inner, 3, 3)*/

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Snippet
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
 	test("shonan-hmm1c") {
    	val snippet = new DslDriver[Unit, Boolean] {

      	def snippet(clr: Rep[Unit]) = {

        	type Var = self.Var
        	//////////////////////////////////////////////////////////////////////////////////////////////////////

        	def minRep(pixelChl: Rep[Int]): Rep[Int] = {
				/* Works with Rep types.
				** Called by genLoop. Clamps the color channel to 255.
				**/

          		var num = 0
          		if (pixelChl >= 255) 
            		num = 255
          		else 
            		num = pixelChl
          
          		readVar(num)
        	} 

			//////////////////////////////////////////////////////////////////////////////////////////////////////
			// xyEval Helper Functions 
			//////////////////////////////////////////////////////////////////////////////////////////////////////

        	// Only for testing the shifting of last tile in the case of splitFactor not diving the img extent.
        	def minExt(num1: Rep[Int], num2: Rep[Int]): Rep[Int] = {
          		var num = 0

          		if (num1 >= num2) 
            		num = num2
          		else 
            		num = num1
          
         		readVar(num)
        	}

        	// Only for testing the shifting of last tile in the case of splitFactor not diving the img extent.
        	def safeBounds(b: Rep[Boolean], value: Rep[Int], imgDim: Int, split: Rep[Int]): Rep[Int] = {
          		if (b) (imgDim - split) else value
        	}
            
            //////////////////////////////////////////////////////////////////////////////////////////////////////
        	def trackNestedVar(func: Func, xs: List[Var], trackMap: Map[Var, (ListBuffer[Var], Boolean, Boolean, Int)], loop: Map[Var, Rep[Int]], vr: Var, imgW: Int, imgH: Int): Rep[Int] = {
          		var varVal = 0
          		//use the new code to know if it's x or y, and the codeSplit to know if it's inner or outer

				var img = 0
					if (vr == Var("x")) {
						img = imgW
						img
					} else if (vr == Var("y")) {
						img = imgH
						img
					}

				val imgDim = readVar(img)

				var outer: String = ""
				var inner: String = ""

				xs foreach ((variable) => { 
					if (variable.codeSplit.get == 0) {
				    	inner = variable.nm
				  	} else if (variable.codeSplit.get == 1) {
				    	outer = variable.nm
				  	} 
				})

				xs foreach ((variable) => { 
					val vars = func.track(variable)
					val splitBool = vars._2
					val fusedBool = vars._3
					val splitFactor = vars._4

					// Case when split more than once?
					if (splitBool == false && fusedBool == false) {
					  	if (imgDim % splitFactor == 0) {
						    //println("var not splitted or fused. the map will have outer * factor + inner.")
						    // look for this outer end this inner on map
						    varVal = loop(Var(outer)) * splitFactor + loop(Var(inner))
					  	} else {
					    	// Splitting by factors that don't divide the extent.
					      	val shiftTile: Rep[Int] = minExt((loop(Var(outer)) * splitFactor), (imgDim - splitFactor))
					      	varVal = shiftTile + loop(Var(inner))
					  	} 

					} else if (splitBool == false && fusedBool == true) {
					    var outerInt = 0
					    // TODO: what if the vars where fused in two different Vars? 
					    // Or if two fused Vars share only one Var, here we look at only one Var and we wouldnt be able to tell
					    // Halide examples do not show more than one variable

					    val lookUpFused = trackMap(variable)._1.toList
					    val lenght = lookUpFused.length

					    if (variable.code.get == 0) {
					      if (lenght == 1)
					        outerInt = (loop(lookUpFused(0)) / imgW) // In fused cased it will always be the img width!
					    } else if (vr.code.get == 1) {
					      if (lenght == 1)
					        outerInt = (loop(lookUpFused(0)) % imgW) 
					    }

				    	varVal = (outerInt * splitFactor + loop(Var(inner)))
					}
				}) // end of foreach 

				readVar(varVal)
        	} //end of def trackNestedVar 

        	//////////////////////////////////////////////////////////////////////////////////////////////////////
        	def trackVar (func: Func, varString: String, trackMap: Map[Var, (ListBuffer[Var], Boolean, Boolean, Int)], loop: Map[Var, Rep[Int]], w: Int, h: Int): Rep[Int] = {

	            var varVal = 0
	            val vr = new Var(varString)
	            val track = func.track(vr)
	            val splitBool: Boolean = track._2
	            val fusedBool: Boolean = track._3

	            if (splitBool == false && fusedBool == false) {
	              	varVal = loop(vr)
	            } else if (splitBool == false && fusedBool == true) {

	              	if (vr.code.get == 0) {

	                	val lookUpFused = trackMap(vr)._1.toList
	               	 	println("lookup map : " + lookUpFused)
	               		val lenght = lookUpFused.length
	                	println("lookup map length : " + lenght)

	                	if (lenght == 1) 
	                  		varVal = ((loop(lookUpFused(0)) / w)).toInt //fused -> will always be img_w!
	                
	              	} else if (vr.code.get == 1) {
	                 
	                	val lookUpFused = trackMap(vr)._1.toList
	                	val lenght = lookUpFused.length

	                	if (lenght == 1) 
	                  		varVal = ((loop(lookUpFused(0)) % w)).toInt //fused -> will always be img_w!
	              	}

	            } else if (splitBool == true && fusedBool == false) {
	              // The outer depends if there was further fusing.
	              // Ex. case where outer are fuse to create parallel tiles.

	              val nestedxs = track._1.toList
	              varVal = trackNestedVar(func, nestedxs, trackMap, loop, vr, w, h)
	           
	            } // end of if (splitBool == false && fusedBool == false) {

	            readVar(varVal)

	        } // end of def trackVar 

        	//////////////////////////////////////////////////////////////////////////////////////////////////////
        	def xyEval (func: Func, loopMap: Map[Var, Rep[Int]], imgWidth: Int, imgHeight: Int): (Rep[Int], Rep[Int]) = {
				/* The Loopmap holds the bounds of the for loops.
				** Find the x and y that will mapped in EvaluateExpr.
				** The x and y can be different depending of what scheduling is used.
				** For example, if it was fused it will be: varVal = ((loop(lookUpFused(0)) % w)).toInt 
				**
				** For each, x and y, calls trackVar function, which in turn calls trackNestedVar (if necessary).
				** Returns a pair of Rep[Int], the x and y
				*/ 

				val xVal: Rep[Int] = trackVar(func, "x", func.track, loopMap, imgWidth, imgHeight)
				val yVal: Rep[Int] = trackVar(func, "y", func.track, loopMap, imgWidth, imgHeight)

				(xVal, yVal)
			} 

      		//////////////////////////////////////////////////////////////////////////////////////////////////////
      		// New Evaluate class [has to be here because of rep]
// TODO: add scanlineArray Option Rep[Array[Array[Int]]]
      		def evaluateExpr(expr: Expr, env: String => Rep[Double], array: Rep[Array[Array[Int]]]): Rep[Double] = { 

        		def eval(expr: Expr): Rep[Double] = expr match {
          			case Var(nm) => env(nm)
          			case Input (inImg, vars) => { 
	            		// Vars can be Vars or Expr
			            //apply (i : Int) : A , The element at given index.
			            //val color: Rep[Int] = img(1).apply(2) // Source Context error
	            
			            var x = unit(0) // FYI: var Rep[Int] - staging variable; won't generate code!
			            var y = unit(0) 
					    val varsArray = new Array[Expr](3)
					    varsArray = vars.toArray.dropRight(1) // drop color channel Var

					    for (i <- (0 until 2): Range) { 
							/* The order in which the were sent determines if they are x or y. 
							Only two axis, x and y. */

					      	//var str: String = vars(i).nm // No; if it's not a Var it can't get the string...
					      	if (varsArray(i).isInstanceOf[Var]) {  
						        // Check Var code to decide which one gets assigned: 0 -> x, 1 -> y
						        // Not by code, but with position here...

					        	if (i == 0) {
					          		val vr: Var = varsArray(0).asInstanceOf[Var] 
					          		x = (env(vr.nm)).toInt

					        	} else if (i == 1) {
					          		val vr: Var = varsArray(1).asInstanceOf[Var] 
					          		y = (env(vr.nm)).toInt
					       		}

					        } else if (varsArray(i).isInstanceOf[Expr]) { 
						        // TODO: Determine its a correct Expr? 
						        // If it's not a Var, but an Expr, have to eval (the operation with env( vars( 0 ).nm)).toInt)

						        // inf loop?? , no because it's not an input its just the expr... 
						        // It will look for the value of the var en the env and then do the arit...

						        // OutofBounds array
						        // How to access w and h?
						        // Get the array dimesions for testing if the evaluated Expr is out of bounds. 
						        val imgH = array.length // gives me y (height), not x (width)
						        val imgW = array(0).length // Array of two dimesions is an array inside an array. 

						        val varNM: PullVars = new PullVars(varsArray(i))
						        val NM: String = (varNM.parameters (varsArray(i)) )
						

					        	if (i == 0) {
					          		x = (eval(vars(0))).toInt
					          
					          		if (x < 0 || x >= imgW)
					            		x = (env(NM)).toInt 
					           			// how to access the var from within the expr
					                	// nm is not member of expr

					        	} else if (i == 1) {
					         		y = (eval(vars(1))).toInt

					          		if (y < 0 || y >= imgH)
					            		y = (env(NM)).toInt
					        	}
					      	}

					    } // end of for (i <- (0 until 2): Range) { 
					    
					    // x and y should now have the correct value (modified or unmodified)
					    val colour: Rep[Int] = array(readVar(x)).apply(readVar(y))
					    var colorChl = 0
					    var colorNm: String = ""

					    if (!vars(2).isInstanceOf[Var]) {  
					     	// No valid Expr in c (color channel Var)
					      	throw new IllegalArgumentException("The color channel can not have an Expr.")
					    }

					    val vrColor: Var = vars(2).asInstanceOf[Var] 

					    if (env(vrColor.nm) == 0) {
					        //println("red channel")
					        colorChl = (colour & 0x00FF0000) >>> 16  
					        colorNm = "R"
					    } else if (env(vrColor.nm) == 1) {
					       // println("green channel")
					        colorChl = ( colour & 0x0000FF00 ) >>> 8
					        colorNm = "G"
					    } else if (env(vrColor.nm) == 2) {
					        //println("blue channel")
					        colorChl = colour & 0x000000FF
					        colorNm = "B"
					    }

					    //println("color channel at input " + colorChl.toDouble)
					    colorChl.toDouble
					    

				  	} // end of Input Case
				  	case Operand( number ) => number
				  	case Sum( left , right ) => eval( left ) + eval( right )
				   
				      /* The loop for c takes care of this
				         eval lR + eval RR
				         eval lG + eval RG
				         eval lB + eval RB
				      */
				   
				  	case Subst( left , right ) => eval( left ) - eval( right )
				  	case Mult( left , right ) => eval( left ) * eval( right ) 
				  	case Div( left , right ) => eval( left ) / eval( right ) 
				} // end of def eval

      			val number: Rep[Double] = eval(expr)
     			number

      		} // end of def evaluateExpr 

			def injectedList (xs: List[Var], flist: ListBuffer[(Var, Int, Boolean)], fn: Func): Unit = { 
				/* Called from within genloop function.
				** It's purpose is to create a list with the injected vars in the correct order and bounds.
				*/

				xs match {
					case Nil => 
						println("Injected List with order and bounds: " + flist)
					case x::xs1 => {
						val varCurr = x
						val bound = fn.bounds(varCurr)
						val bound1 = bound._1
						flist += { (x, bound1, true) }
						finalList(xs1,flist,fn)
					}
				} 
    		} // end of injectedList

    		//////////////////////////////////////////////////////////////////////////////////////////////////////
      		def genLoop(func: Func, xs: List[(Var, Int, Boolean)], xs2: List[(Var, Rep[Int])], array: Array[Array[Int]], arraySL: Option[Array[Array[Int]]], initialCall: Boolean): Unit = { 
		        /* xs - final list with order and bounds
		        ** xs2 - empty list in which we will generate the loopmap
		        ** initial - one way to identify when to calculate first scanline!
		        ** Added: arraySL which will be sent from realize
		        */

		        val img = staticData(array)
		        // Get the array dimesions for testing if the evaluated Expr is out of bounds. 
		        val imgH = array.length // gives me y (height), not x (width)
		        val imgW = array(0).length // Array of two dimesions is an array inside an array. 

		        var needToComputeSL = false
		          
		        // create loopMap from List, a single map per pixel
		        xs match {
		            case Nil => {
		            	if (func.consumeFrom != None) { 
/*			            	Then there is multilevel scheduling, and we need to test where we are...
			              	Must check which scheduling it's beign used, sequencial or tiling...
			              	TODO: implement sequencial first! then move to tiling...

			              	We can only determine if we have to calculate a scanline inside the for loop, or
			              	in the Nil case, where we can examine the xs2 with second Var (in case of sequencial traversal)
			              	
*/
			              	// HOW TO DETERMINE IF WE NEED TO CALCULATE SCANLINE, AND THEN CREATE THE LIST...
								// Row Major, y outer loop, x inner loop; need to check the value of the x 
								// x will be the second Var, how to manage the first iteration where xs2 still doesn't have the x value?
								// Col Major, x outer loop, y inner loop; need to check the value of y
								// y will be the second Var, how to manage the first iteration where xs2 still doesn't have the y value?
								// xs2 will be empty initially, at the beggining of every pixel map...

			              	if (!xs2.isEmpty) { 
			              		// If xs2 is empty, then it means we changed pixel, not necessarily a row!
			              		// So we need to determine if need 
				              	if (xs2.length == 2) {
				              	// Then check if the lenght of xs2 is two, and then we can look at the value, and test...	
									val current2ndVar = xs2(1)._2

									val current1stVar = xs2(1)._1

									val var2ndModImgW = (current2ndVar) - (current2ndVar/imgW)* imgW
									val var2ndModImgH = (current2ndVar) - (current2ndVar/imgH)* imgH

									if (func.orderColMajor) {
										if (var2ndModImgH == 0)
											needToComputeSL = true

									} else if (!func.orderColMajor) {
										if (var2ndModImgW == 0)
											needToComputeSL = true

									} else if (initialCall) {
										needToComputeSL = true
									}

				                } // end of if (xs2.lenght == 2) {
			                } // end of if (!xs2.isEmpty) {

			            } // end of if (func.consumeFrom.get != None) { 

		              	// TODO: calculate real numbers for injected Vars
		              	// from def scanlines

			          	val outerOriginal = new Var("outerOriginal")
			          	val outerInjected = new Var("outerInjected")
			          	val innerInjected = new Var("innerOriginal")

			          	val injectedVars = List(outerOriginal, outerInjected, innerInjected) 
			          	// TODO: why define if func.cosumeFrom.get == None
			          	val injectedListBuffer = new ListBuffer[(Var, Int, Boolean)]()  
			          
			          	var provisional: Rep[Int] = unit(2)
			          	var injY: Rep[Int] = unit(0)
			          	var injX: Rep[Int] = unit(0)

			          	// TODO determine which parts of the scanline can be reuse!!!! DONT FORGET. NO RECOMPUTATION
			          	if (needToComputeSL) { 
			          		// populate the injected List

			          		// get bounds 
		          			

			          		injectedListBuffer = ListBuffer((outerOriginal, provisional, true), (x, injX, true), (y, injY, true))
							
							val injectedList = injectedListBuffer.toList
			          		// initial call, like the realize call
			          		println("Initial call to a scanline!")
			          		genLoop(func, injectedList, Nil, array, arraySL, false)
			          		//genLoop(func, xs1, xs2 ++ List((x._1, x0)), array, arraySL, false)
			          	}

			          	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
						// For each pixel we will enter the nil case once and evaluate the pixel color.
						// xs2 will have one of these maps (corresponding to the single pixel).
						//> Map(y -> 0, x -> 0, c -> 0) - Nil
						//| Map(y -> 0, x -> 0, c -> 1) - Nil
						//| Map(y -> 0, x -> 1, c -> 0) - Nil ...etc
		           
		              	val loops = Map() ++ (xs2 map (pairs => (pairs._1, pairs._2)))

						// Get the array dimesions for fused Var case. 
						val arrayH = array.length // gives me y (height), not x (width)
						val arrayW = array(0).length // Array of two dimesions (an array inside an array). 
						val xyPair = xyEval(func, loops, arrayW, arrayH)
						val xValue = xyPair._1
						val yValue = xyPair._2
		              
		                // println("--------------------------------------------------------------------------------------")
		                if (func.args.length == 2) { // Function without an input image

		                    val pixelColor = (evaluateExpr(func.expr, Map( func.args( 0 ).nm -> xValue, func.args( 1 ).nm -> yValue ), img)).toInt
		                    img(xValue)(yValue) = pixelColor // unmodified x and y
		           
		                } else { 
		                	// Function with an input image
		                    val newRGB = new Array[Rep[Int]](3)

		                    for (c <- (0 until 3): Range) {
		                    	// Function with an input image 
		                    	//println( "3 chls For loop" )
		                      	val pixelChl = (evaluateExpr(func.expr, Map( func.args( 0 ).nm -> xValue, func.args( 1 ).nm -> yValue, func.args( 2 ).nm -> unit(c) ), img)).toInt
		                      	//newRGB( c ) = minRep(pixelChl) 
		                      	//val pixel = staticData(pixelChl)
		                
		                      	newRGB(c) = pixelChl // clamp to 255 
		                      	//newRGB(c) = minRep(pixelChl) // clamp to 255
		                      	//println( "CC after eva: " + newRGB( c ) )
		                      	//println( " " )
		                    }

		                    //FIX: clamp outside loop?
		                    //println( "Out of 3 chls For loop" )
		                    val newR = newRGB( 0 )
		                    val newG = newRGB( 1 )
		                    val newB = newRGB( 2 )

		                    //println( "5. Type Int | red: " + newR + " - green: " + newG + " - blue: " + newB /*+ " - alpha: " + a*/ )
		                    val alpha = 255 << 24
		                    val alpha2 = unit(alpha)
		                    //println("alpha...")
		                    // ( 255 << 24 )
		                    //val pixelColor : Rep[Int] = ( 255 << 24 ) | ( newR << 16 ) | ( newG << 8 ) | newB
		                    val pixelColor = unit(alpha) | ( newR << 16 ) | ( newG << 8 ) | newB
		                    //println("pixel color " + pixelColor)
		                    img(xValue)(yValue) = pixelColor
		                    //img(x).update(y, pixelColor)
		                    //println( "final color: " + fnl )
		                    //outImg.setRGB(x, y, fnl)
		                
		                } //end of if (func.args.length == 2) {
/* 
		               Add a case where the x var has a code for injected loop! 
					   genLoops needs the Lists sent to be the same type, no use using ClassTag
					   Can't test for lenght and arguments at the same time! 
					   So can't have them in separete outer cases.
*/
		            } case x::xs1 => { // xs: List[(Var, Int, Boolean)], xs2: List[(Var, Rep[Int])]

/* 						1. Check if consumeFrom is None, if None we dont have to check img sizing or anything.
			            		a. Check if we need to compute a new scanline; set a bool flag.
			            			- Proceed checking only if xs2 is not empty and lenght is two (in the case of sequential traversal!)
			            			- If the the value at the second Var is % img height or width = 0
						   2. If bool flag true,
						   		- Then create new list with first Var, and py, px...
						        - The func has internally if it is on col major or row major traversal
							    - Call genloop with this new List!	CAREFUL HERE!
							    	- In the recursion, I cannot call another genloops (original again!)
						   3. Call genloops on original List
*/
			           	// Now recursevily call genloops 
			           	// check Boolean value
		            	x._3 match {   
		            		case true => {
								// List has first Var of original consumer loop, and the two Vars py and px 
								for (x0 <- (0 until x._2): Rep[Range]) 
									genLoop(func, xs1, xs2 ++ List((x._1, x0)), array, arraySL, false)
		            		} case false => {
								// List has original Vars of the consumer 
								for (x0 <- (0 until x._2): Rep[Range]) 
									genLoop(func, xs1, xs2 ++ List((x._1, x0)), array, arraySL, false)
		            		}
		            	} // end of match x._3

/*		                
		                // x._2 has bound
		              	val parBool = func.par getOrElse (x._1, false)
		             	// parBool = parPair._2

		              	if (!parBool) {
		                	// For each Var create a for loop with the bound. 
		                	// First Var on List will be outer loop, and so on...
		                	for (x0 <- (0 until x._2): Rep[Range]) {
		                  		val list = xs2 ++ List((x._1, x0))
		                  		genLoop(func, xs1, xs2 ++ List((x._1, x0)), array, arraySL, false)
		                	}
		             	} else {
		                 	// parallel .par
		                 	for (x0 <- (0 until x._2): Rep[Range]) 
		                  	genLoop(func, xs1, xs2 ++ List((x._1, x0)), array, arraySL, false)
		              	}
*/		              	

		          	} // end of case
		        } // end of xs match
		    } // end of def genLoop

			////////////////////////////////////////////////////////////////////////////////////////
			// Realize Function
			////////////////////////////////////////////////////////////////////////////////////////

        	def realize(func: Func, w: Int, h: Int, array: Array[Array[Int]]/*, arrayS: Option[Array[Array[Int]]]*/): Unit = { 
          		println("Realizing " + func.nm + " Func") // will be printed in generated code...

          		val width: Int = w 
          		val height: Int = h 
          		//reflectMutableSym(img.asInstanceOf[Sym[Any]])

	          	var producerW: Int = 0
			  	var producerH: Int = 0
       
            	// Determine the size of the producer allocation and save it within the func itself.
	        	if (func.consumeFrom != None) {  
	          		// TODO: classify a function strategy! here -> if (func.Scheduling = sequencialComputation)
	          		//val slArray2: Array[Array[Int]]
	          		//val slArray2 = Array.ofDim[Int](100, 100)

			        // Allocate space (correct amount of space for the strip of producer!)
			        // Sequencial order: row or col size plus extents 
			        // Tiling tile size + plus extents
			        // vectorization?
					// Get the array dimesions for testing if the evaluated Expr is out of bounds. 
			        val imgH = array.length // gives me y (height), not x (width)
			        val imgW = array(0).length // Array of two dimesions is an array inside an array. 

		        	if (!func.orderColMajor) { 
						// rowMajor - default case
						producerW = width + func.extX.get
						if (func.extY.get != 0) {
							producerH = 1 + func.extY.get
						} else 
							producerH = 1

					} else { 
					// colMajor
						producerH = height + func.extY.get
						if (func.extX.get != 0) {
							producerW = 1 + func.extX.get
						} else 
							producerW = 1
					}
					
					func.prodH = Some(producerH)
				    func.prodW = Some(producerW) 

	        	} // end of if (func.consumer != None)

		  		val injArray = scala.Array.ofDim[Int](producerW, producerH)
          		// Complete func's bounds map with the image dimensions.
          		completeBounds(func, width, height)

          		// Convert the order ListBuffer of Vars from func to List.
          		val funcList = func.buffer.toList
          		// None means it's an injected var
          		val finalListBuffer = new ListBuffer[(Var, Int, Boolean)]() 

          		// Modify the newly (empty) created ListBuffer[(Var, Int)].
          		// This ListBuffer will now have the order and all the loop bounds.
          		finalList(funcList, finalListBuffer, func) 
				// TODO: func.buffer(0) not enough to test first Var!

				// edit final? No, can't put the producer's vars in the final list because
				// genloops will stop at Nil and then evaluate the expression.
				val fnlList = finalListBuffer.toList
          
           		println("Generating loopMap /////////////////////////////////////////////////////////////")
            	if (func.consumeFrom != None) { 
            		//val arrayS: Option[Array[Array[Int]]] = Some(Nil)
          			genLoop(func, fnlList, Nil, array, Some(injArray)/*Some(func.slArray)*/, true) 
          		} else 
            		genLoop(func, fnlList, Nil, array, None, true)

        	} // end of def realize

		/////////////////////////////////////////////////////////////////////////////////////////////////
		//realize(brighter, width, height, imgArray)
		//realize(hBlur, width, height, imgArray)

		// Test various functions for start and extents ...................
		scanline(vBlur, imgArray)
		scanline(hBlur, imgArray)

		// computeAt
		// send as Some(array) or None if there are no 
		//computeAt(vBlur, hBlur, y, imgArray)
		//realize(vBlur, width, height, imgArray)

		//val instExpr: ExprPrinter = new ExprPrinter(expr)
		//println("test: " + inst.print(expr))

		
		// Test consumer and producer Schedule!

		val outputImage: BufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)

		for (x <- (0 until width): Range ; y <- (0 until height): Range) {
		//checkingImage.setRGB(x, y, {imgArray(x).apply(y)} )
		outputImage.setRGB(x, y, {imgArray(x)(y)} )
		}

		println("Checking the output image array! Saving the picture.")
		ImageIO.write(outputImage, "png", new File("outputImgArray.png"))
		// write returns: false if no appropriate writer is found
		// change snippet to Boolean

		} // end of def snippet
	} // end of val snippet

    //snippet.eval()
    check("shonan-hmm1c", snippet.code)

  } // end of test Halide
} // end of class HalideTest