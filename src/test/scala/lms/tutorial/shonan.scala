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
**    ✓ Tiling sequencially without shortcut
**    ✓ Tiling sequencially with shortcut
**      _ Test all again with new xyEval!
**
** ✓ Track functions inside xyEval; get rid of ugly code replication!
**      ✓ Eliminate the if and else for Var x and Var y; just used variables outer and inner
**      _ Depends on specific Strings!
** ✓ Change the ._#s for readability 
** _comments and organize code 
** _Get rid of mutability
** _Vectorize ...in progress 
** _Parallelize - later! (Delite; work sequencially for now)
** _Generate C code
**/ 

class ShonanTest extends TutorialFunSuite { self =>
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Image Array
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  val under = ""

  val inImg: BufferedImage = ImageIO.read(new File("C:\\Users\\LilliamI\\Pictures\\Img\\small.png"))
  //val inImg: BufferedImage = ImageIO.read(new File("C:\\Users\\LilliamI\\Pictures\\Img\\image.jpg"))
  val width: Int = inImg.getWidth 
  val height: Int = inImg.getHeight
  val imgArray = Array.ofDim[Int](width, height)

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
  ImageIO.write(checkImage, "png", new File("originaArray.png") )

////////////////////////////////////////////////////////////////////////////////////////////
// Classes
////////////////////////////////////////////////////////////////////////////////////////////
  sealed abstract class Expr ()
  // Operands
  case class Var(val nm: String) extends Expr 
  case class Input(val inputImg: BufferedImage, val vars: List[Var]) extends Expr
  case class Operand(val number: Double) extends Expr
  // Operators
  case class Sum(x: Expr, y: Expr) extends Expr
  case class Subst(x: Expr, y: Expr) extends Expr
  case class Mult(x: Expr, y: Expr) extends Expr

  class ExprPrinter(expr: Expr) extends Expr {
    val print: Expr => String
        = _ match {
            case Var(nm) => nm.toString
            case Input (inImg , vars) => "input ( " + vars + " )"
            case Operand (value) => value.toString
            case Sum(left, right) => "( " + print(left) + " + " + print(right) + " )"
            case Subst(left, right) => "( " + print(left) + " - " + print(right) + " )"
            case Mult(left, right) => "( " + print(left) + " * " + print(right) + " )" 
          }
  } 

  class Func(val nm: String, val args: List[Var], val expr: Expr) {
    /* args: List[Var] -- Original list of Vars before any scheduling.
    **
    ** buffer: ListBuffer[Var] -- Scheduled Vars and ordering.
    **
    ** bounds: Map[Var, (Int,Int)] -- Map the scheduled Var to its bound.
    ** The Pair of Ints are later used by completeBounds function.
    ** If it finds a 0, then the Var is the original x or y (was not split
    ** or fused). The bounds are then the image dimensions. If it finds a -1
    ** further calculation is need. Or it can be the bound itself if finds a number > 0. 
    ** The second Int will carry the splitFactor used in calculations during 
    ** completeBounds call. By default its -1
    **
    ** track: Map[Var, (ListBuffer[Var], Boolean, Boolean, Int)] -- Map Var
    ** to a List of nested Vars (tracks how the original Vars have been split
    ** or fused). When a new Var is created it will also have its entry in the Map.
    **
    ** par: Map[Var, Boolean] -- Map a Var to true if has been parallelized,.
    ** or false otherwise.
    **/ 
    var buffer = new ListBuffer[Var]()

    def ScheduleList(lst: List[Var]): ListBuffer[Var] = lst match {
      case h::Nil => buffer
      case h::t => {
        buffer += h
        ScheduleList(t)
      }
    }

    ScheduleList(args)
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

  ////////////////////////////////////////////////////////////////////////////////////////////
  // Scheduling Functions
  ////////////////////////////////////////////////////////////////////////////////////////////
    def insert[A](e: ListBuffer[A], n: Int, ls: ListBuffer[A]): ListBuffer[A] = {
      ls.splitAt(n) match {
      case (pre, post) => pre ++ e ++ post }
    }

    def reorder (func: Func, order: ListBuffer[Var]): Unit = { 
      /* gradient.reorder(x_inner, y_inner, x_outer, y_outer); [innermost -> outermost]
      ** List works outermost -> innermost
      ** 
      ** Check if the order ListBuffer has the same length as the func's ListBuffer,
      ** and has the same Vars (is a subset).
      ** If not an illegal arguments exception is thrown.
      **/ 
      println(" -> Reorder <- ")
      println("func.buffer befor reordering: " + func.buffer)

      val newOrderMap = Map() ++ (order map (p => (p, order.indexOf(p)) ))
      //println("New order of buffer: " + newOrderMap)
      val oldOrderMap = Map() ++ (func.buffer map (p => (p, func.buffer.indexOf(p)) ))
      //println("Original order of buffer: " + oldOrderMap)

      val isSubset = newOrderMap.keySet subsetOf oldOrderMap.keySet
      val equalSz = newOrderMap.size == oldOrderMap.size

      if (isSubset && equalSz)
        println("Correct arguments in reorder.")
      else
        throw new IllegalArgumentException("The list of arguments in the reorder is incorrect...")

      func.buffer = order
      println("Reorder func.buffer: " + func.buffer)
      println(" -> End of Reorder <- ")
      println(" ")
    } //end of reorder func

    def fuse (func: Func, variable1: Var, variable2: Var, fused: Var): Unit = { 
      /* Inserts the new fused Var into the func.buffer, and removes the previous Vars.
      ** Inserts the new fused Var into func.bounds, and removes the previous Vars.
      ** Inserts a new entry into the func.track, that represents the new fused variable.
      ** The var1 and var2 two entries of this map are modified, the second boolean (representing
      ** if the Var has been fused) set to true.
      **/ 
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
      //println("Value associated with variable1 key: " + value)
      //println("boolean: " + value1._2)
      val value1 = func.track(variable1)
      val value2 = func.track(variable2)
      val prevBool1 = value1._2
      val prevBool2 = value2._2

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
      /* Inserts the new two Vars into the func.buffer, and removes the previous Var.
      ** Inserts the new split Vars into func.bounds, and removes the previous Var.
      ** The inner split Var's bounds is know at this point. It will be the splitFactor.
      ** Unlike the outer's bound which depends part from the image dimensions.
      ** The split factor will be stored in the second Int of the Map for further computation
      ** during completeBounds function call.
      ** Inserts two new entries into the func.track, that represent the new split variables.
      ** The original variable's entry in this map is modified, the first boolean (representing
      ** if the Var has been split) set to true.
      **/   
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

      /* TODO: limiting the nested numbers of Vars to only two
         Which cases can they have more than two? */
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
      // (x,y) vs (y,x) 
      // ListBuffer(y_outer, x_outer, y_inner, x_inner); default for tiling
      //val tilingTest = tile(brighter, y, x, y_outer, x_outer, y_inner, x_inner, 4, 4)*/

      /*// Split two variables...  
        // Split 1
        val x_inner = new Var("x_inner")
        val x_outer = new Var("x_outer")
        val splitTest = split(brighter, x, x_outer, x_inner, 4)
        // Split 2
        val y_inner = new Var("y_inner")
        val y_outer = new Var("y_outer")
        val splitTest2 = split(brighter, y, y_outer, y_inner, 4)
        // Reorder
        val reorderTest = reorder(brighter, ListBuffer(y_outer, x_outer, y_inner, x_inner))*/

      println(" -> Tile <- ")
        // Split 1
        val splitTest = split(func, variable2, outer2, inner2, splitFactor2)
        // Split 2
        val splitTest2 = split(func, variable1, outer1, inner1, splitFactor1)
        // Reorder
        val reorderTest = reorder(func, ListBuffer(outer1, outer2, inner1, inner2))
      /*val split1 = split(func, variable1, outer1, inner1, splitFactor1)
      val split2 = split(func, variable2, outer2, inner2, splitFactor2)
      val order = reorder(func, ListBuffer(outer1, outer2, inner1, inner2))*/
      println(" -> End of Tile <- ")
      println(" ")
    } 

    def parallel(func: Func, variable: Var): Unit = { 
      /* Maps a Var to a Boolean that represents if it has been parallelized.
      **/
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
      func.track(variable) = (ListBuffer(vec_outer /*,null?*/), true, prevBool, splitFactor) // inefficient!
      // Remove from ListBuffer in: func.track(variable) = (ListBuffer(outer,inner), true, prevBool, splitFactor).

      println(" -> End of Tile <- ")
      println(" ")
    } // end of vectorize def

  ////////////////////////////////////////////////////////////////////////////////////////////
  // Realize Helper Functions (...with no Rep Types)
  ////////////////////////////////////////////////////////////////////////////////////////////
    def completeBounds(func: Func, w: Int, h: Int): Unit = {
      /* Calculate the missing bounds in func.bounds.
      ** It's called from within realize.
      ** The outer loop of a split Var and a fused Var depend on image dimension.
      ** the image dimension are arguments on realize.
      **/
      println("Completing Bounds...")
      println("Current func.bounds map: " + func.bounds)
      
      func.bounds foreach ((p) => {
        //println(p)
        //println("key " + p._1)
        val key = p._1
        println("key: " + key)
        val pair = p._2
        val needCalc = pair._1
        val split = pair._2
        //println("pair " + pair)
        //println(("code " + pair._1))
        //println(("information to compute " + pair._2))
        //none split or fused need only image dimensions

        if (needCalc == 0) {
          println(key + " has not been split or fused. Only needs image dimensions.")
          if (key == x) {
            //necesito saber si es y o x de alguna manera, y la unica es identificando por el nombre
            // puede estar excrito con mayuscula, o minuscula, la x y la y de los split Vars 
            // pueden estar al principio o al final.
            func.bounds(key) = (w, -1)
          } else if (key == y) 
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
              val keyString = key.toString
              val x_or_y = keyString.charAt(4).toString 
                // what is its not there; use contains perhaps or some kind of marker
                // What if it is vectorized?
                // Can't rely on char
                // TODO: Fix this to always work and not rely on Strings
              println("x_or_y: " + x_or_y) // or vectorized
              
                if (x_or_y == "x") {
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
                } else if (x_or_y == "y") {
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
                } else if (x_or_y == "v") { 
                  // we vectorized only x? for now...
                  // can we vectorize the y?
                  println("split: " + split)
                  val newBound = (w / split)
                  println("h: " + h + ", newBound: " + newBound)
                  func.bounds(key) = (newBound, -1)
                }   
            } 
        } 
      } )  // end of foreach statement
                                          
      println("New bounds map: " + func.bounds)
      println("Bounds Completed...")
      println("")
    } //end of def getBounds

    def newList (xs: List[Var], flist: ListBuffer[(Var, Int)], fn: Func): Unit = { 
      xs match {
        case Nil => 
          println("Final List with order and bounds: " + flist)
        case x::xs1 => {
          val var1 = x
          val bound = fn.bounds(var1)
          val bound1 = bound._1
          flist += {(x, bound1)}
          newList(xs1,flist,fn)
        }
      } 
    } // end of newList

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Halide Tutorials
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      val x = new Var("x")
      val y = new Var("y") 
      var c = new Var("c")
      val gradientExpr = new Sum(x , y)

      // The value's list stays the same for the exprPrinter
      val value = new Input (inImg, List(x, y, c))
      val brighterExpr = new Mult (value, new Operand(1.2))

      // The function's list gets modified with the schedule
      val brighter = new Func("brighter" , List(x, y, c), brighterExpr)  

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
        val splitTest = split(brighter, x, x_outer, x_inner, 5)*/

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

      // Tiling
        val x_inner = new Var("x_inner")
        val x_outer = new Var("x_outer") 
        val y_inner = new Var("y_inner")
        val y_outer = new Var("y_outer")
        val tilingTest = tile(brighter, y, x, y_outer, x_outer, y_inner, x_inner, 5, 5)

  ////////////////////////////////////////////////////////////////////////////////////////////
  // Snippet
  ////////////////////////////////////////////////////////////////////////////////////////////
  test("shonan-hmm1c") {
    val snippet = new DslDriver[Unit, Boolean] {
      def snippet(clr: Rep[Unit]) = {

        type Var = self.Var
        ////////////////////////////////////////////////////////////////////////////////////////////
        def minRep(pixelChl: Rep[Int]): Rep[Int] = {
          var num = 0
          if (pixelChl >= 255) 
            num = 255
          else 
            num = pixelChl
          
          readVar(num)
        } 

        ////////////////////////////////////////////////////////////////////////////////////////////
        // xyEval Helper Functions 
        ////////////////////////////////////////////////////////////////////////////////////////////
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

        def trackNestedVar(func: Func, xs: List[Var], loop: Map[Var, Rep[Int]], vr: Var, imgW: Int, imgH: Int): Rep[Int] = {
          var varVal = 0
          var pair: (String, String) = ("","")
          if (vr == Var("x")) {
            pair = ("x_outer", "x_inner")
            pair
          }
          else if (vr == Var("y")) {
            pair = ("y_outer", "y_inner")
            pair
          }

          var img = 0
          if (vr == Var("x")) {
            img = imgW
            img
          }
          else if (vr == Var("y")) {
            img = imgH
            img
          }
          val imgDim = readVar(img)

          val outer: String = pair._1
          val inner: String = pair._2

          xs foreach ((variable) => { 
            val vars = func.track(variable)
            val splitBool = vars._2
            val fusedBool = vars._3
            val splitFactor = vars._4

            if (splitBool == false && fusedBool == false) {
              if (imgDim % splitFactor == 0) {
                //println("var not splitted or fused. the map will have outer * factor + inner.")
                varVal = loop(Var(outer)) * splitFactor + loop(Var(inner))
              } else {
                // Splitting by factors that don't divide the extent.
                /*xVal = loopMap(Var("x_outer")) * vars._4
                  val bool: Rep[Boolean] = (xVal > (imgWidth - vars._4))
                  val temp: Rep[Boolean] = safeBounds(bool, readVar(xVal), imgWidth, vars._4)
                  xVal = temp + loopMap(Var("x_inner"))*/
                  val shiftTile: Rep[Int] = minExt((loop(Var(outer)) * splitFactor), (imgDim - splitFactor))
                  varVal = shiftTile + loop(Var(inner))
              }    
            } else if (splitBool == false && fusedBool == true) {
                var outerInt = 0
                //println("var was fused. the outer = fused / or % img_W")
                if (vr == Var("x"))
                  outerInt = (loop(Var("fused")) / imgW) // In fused cased it will always be the img width!
                else if (vr == Var("y"))
                  outerInt = (loop(Var("fused")) % imgW) 

                varVal = (outerInt * splitFactor + loop(Var(inner)))
          }}) // end of foreach 

          readVar(varVal)
        } //end of def trackNestedVar 

        def trackVar (func: Func, varString: String, trackMap: Map[Var, (ListBuffer[Var], Boolean, Boolean, Int)], loop: Map[Var, Rep[Int]], w: Int, h: Int): Rep[Int] = {
            var varVal = 0
            val vr = new Var(varString)
            val track = func.track(vr)
            val splitBool: Boolean = track._2
            val fusedBool: Boolean = track._3

            if (splitBool == false && fusedBool == false) {
              //println(func.track(x) + " Var was not splitted or fused. Evaluation map will point to the x0 of the loop.")
              varVal = loop(vr)
            } else if (splitBool == false && fusedBool == true) {
              //println(func.track(x) + " Var was fused. Evaluation mas will point to var = fused / or % img_width. The fused comes from loops map.")
              if (vr == Var("x")) 
                varVal = ((loop(Var("fused")) / w)).toInt //fused -> will always be img_w!
              else if (vr == Var("y"))
                varVal = ((loop(Var("fused")) % w)).toInt 
            } else if (splitBool == true && fusedBool == false) {
              //println(func.track(x) + " Var was split. Var = outer * split + inner") 
              // The outer depends if there was further fusing.
              // Ex. case where outer are fuse to create parallel tiles.

              //track._1 is ListBuffer[Var]
              val nestedxs = track._1.toList

              if (varString == "x")
                varVal = trackNestedVar (func, nestedxs, loop, vr, w, h)
              else if (varString == "y")
                varVal = trackNestedVar (func, nestedxs, loop, vr, w, h)  
            } // end of second else if
            readVar(varVal)
          } // end of def trackVar 

        ////////////////////////////////////////////////////////////////////////////////////////////
        def xyEval (func: Func, loopMap: Map[Var, Rep[Int]], imgWidth: Int, imgHeight: Int): (Rep[Int], Rep[Int]) = {
          /* The Loopmap holds the bounds of the for loops.
          ** Find the x and y that will mapped in EvaluateExpr.
          ** For each, x and y, calls trackVar function, which in turn calls trackNestedVar.
          ** Returns a pair of Rep[Int], the x and y
          */ 
          val xVal: Rep[Int] = trackVar(func, "x", func.track, loopMap, imgWidth, imgHeight)
          val yVal: Rep[Int] = trackVar(func, "y", func.track, loopMap, imgWidth, imgHeight)
          (xVal, yVal)
        } 

      ////////////////////////////////////////////////////////////////////////////////////////////
      // New Evaluate class [has to be here because of rep]
      def evaluateExpr(expr: Expr, env: String => Rep[Double], array: Rep[Array[Array[Int]]]): Rep[Double] = { 
        def eval(expr: Expr): Rep[Double] = expr match {
          case Var(nm) => env(nm)
          case Input (inImg , vars) => { 
            //apply (i : Int) : A , The element at given index.
            //val color : Rep[Int] = img(1).apply(2) //Source Context error
            val x: Rep[Int] = (env( vars( 0 ).nm)).toInt
            val y: Rep[Int] = (env( vars( 1 ).nm)).toInt
            val colour: Rep[Int] = array( x ).apply( y )

            var colorChl = 0
            var colorNm : String = ""
            println("RGB channels if, else if")

            if (env(vars(2).nm) == 0 ) {
                println("red channel")
                colorChl = (colour & 0x00FF0000) >>> 16  
                colorNm = "R"
            } else if (env(vars(2).nm) == 1 ) {
                println("green channel")
                colorChl = ( colour & 0x0000FF00 ) >>> 8
                colorNm = "G"
            } else if ( env(vars( 2 ).nm ) == 2 ) {
                println("blue channel")
                colorChl = colour & 0x000000FF
                colorNm = "B"
            }
            colorChl.toDouble

          } // end of Input Case
          case Operand( number ) => number
          case Sum( left , right ) => eval( left ) + eval( right )
          case Subst( left , right ) => eval( left ) - eval( right )
          case Mult( left , right ) => eval( left ) * eval( right ) 
      } // end of def eval

      val number: Rep[Double] = eval(expr)
      number
      } // end of def evaluateExpr 

      ////////////////////////////////////////////////////////////////////////////////////////////
      def genLoop(func: Func, xs: List[(Var, Int)], xs2: List[(Var, Rep[Int])], array: Array[Array[Int]]): Unit = { 
        val img = staticData(array)
          xs match {
            case Nil => {
              val loops = Map() ++ (xs2 map (pairs => (pairs._1, pairs._2)))
              println("Loops Generated with schedule: " + loops)

              // Get the array dimesion for fused Var case. 
              val arrayH = array.length // gives me y (height), not x (width)
              val arrayW = array(0).length // Array of two dimesions is an array inside an array. 
              val xyPair = xyEval(func, loops, arrayW, arrayH)
              val xValue = xyPair._1
              val yValue = xyPair._2
              
              println("--------------------------------------------------------------------------------------")
              /////////////////////////////////////////////////////////////////////////////////////////////////////////
                  if (func.args.length == 2) { //Function without an input image
                    val pixelColor = (evaluateExpr(func.expr, Map( func.args( 0 ).nm -> xValue, func.args( 1 ).nm -> yValue ), img)).toInt
                    img(xValue)(yValue) = pixelColor 
                    //img(x).update(y, pixelColor)
                  } else { // Function with an input image
                    //val newRGB = NewArray[Int](3)
                    val newRGB = new Array[Rep[Int]](3)

                    for (c <- (0 until 3): Range) { 
                      println( "3 chls For loop" )
                      //Rep[Int]
                      val pixelChl = (evaluateExpr(func.expr , Map( func.args( 0 ).nm -> xValue, func.args( 1 ).nm -> yValue, func.args( 2 ).nm -> unit(c) ), img)).toInt

                      //newRGB( c ) = minRep(pixelChl) 
                      //val pixel = staticData(pixelChl)
                      newRGB( c ) = minRep(pixelChl)
                      //println( "CC after eva: " + newRGB( c ) )
                      //println( " " )
                    }

                    println( "Out of 3 chls For loop" )
                    val newR = newRGB( 0 )
                    val newG = newRGB( 1 )
                    val newB = newRGB( 2 )

                    //println( "5. Type Int | red: " + newR + " - green: " + newG + " - blue: " + newB + " - alpha: " + a )
                    val alpha = 255 << 24
                    val alpha2 = unit(alpha)
                     println("alpha...")
                    // ( 255 << 24 )
                    //val pixelColor : Rep[Int] = ( 255 << 24 ) | ( newR << 16 ) | ( newG << 8 ) | newB
                    val pixelColor = unit(alpha) | ( newR << 16 ) | ( newG << 8 ) | newB
                    //println(pixelColor)
                    img(xValue)(yValue) = pixelColor
                    //img(x).update(y, pixelColor)
                    //println( "final color: " + fnl )
                    //outImg.setRGB(x, y, fnl)
                
                } //end of else
            }
            case x::xs1 => { 
              // x._2 has bound
              val parBool = func.par getOrElse (x._1, false)
              // parBool = parPair._2
              if (!parBool) {
                for (x0 <- (0 until x._2): Rep[Range]) 
                  genLoop(func, xs1, xs2 ++ List((x._1, x0)),array)
              } else {
                 for (x0 <- (0 until x._2): Rep[Range]) // parallel .par
                  genLoop(func, xs1, xs2 ++ List((x._1, x0)), array)
              }
          } // end of case
        } // cdend of xs match
      } // end of def genLoop

      ////////////////////////////////////////////////////////////////////////////////////////
      // Realize Function
      ////////////////////////////////////////////////////////////////////////////////////////
        def realize(func: Func, w: Int, h: Int, array: Array[Array[Int]]): Unit = { 
          println("Realizing " + func.nm + " Func") // will be printed in generated code
          val width: Int = w 
          val height: Int = h 
          //reflectMutableSym(img.asInstanceOf[Sym[Any]])

          // Complete func's bounds map with the image dimensions.
          completeBounds(func, width, height)

          // Convert the order ListBuffer of Vars from func to List.
          val funcList = func.buffer.toList
          val finalListBuffer = new ListBuffer[(Var, Int)]()

          // Modify the newly (empty) created ListBuffer[(Var, Int)].
          // This ListBuffer will now have the order and all the loop bounds.
          newList(funcList, finalListBuffer, func) // error here
          val finalList = finalListBuffer.toList
          
          println("Generating loopMap")
          genLoop(func, finalList, Nil, array)
        } // end of def realize

      /////////////////////////////////////////////////////////////////////////////////////////////////
      realize(brighter, width, height, imgArray)

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

    snippet.eval()
    check("shonan-hmm1c", snippet.code)

  } // end of test Halide
} // end of class HalideTest
