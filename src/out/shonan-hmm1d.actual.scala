/*****************************************
Emitting Generated Code
*******************************************/
class Snippet(px6:Array[Int],px16:Array[Int]) extends ((Array[Int])=>(Array[Int])) {
  def apply(x0:Array[Int]): Array[Int] = {
    val x2 = new Array[Int](5)
    val x6 = px6 // static data: Array(1,1,1,1,1)
    var x4 : Int = 0
    val x13 = while (x4 < 5) {
      val x5 = x2(0)
      val x7 = x6(x4)
      val x8 = x0(x4)
      val x9 = x7 * x8
      val x10 = x5 + x9
      val x11 = x2(0) = x10
      x4 = x4 + 1
    }
    val x16 = px16 // static data: Array(0,1,1,1,1)
    var x14 : Int = 0
    val x23 = while (x14 < 5) {
      val x15 = x2(1)
      val x17 = x16(x14)
      val x18 = x0(x14)
      val x19 = x17 * x18
      val x20 = x15 + x19
      val x21 = x2(1) = x20
      x14 = x14 + 1
    }
    val x24 = x2(2)
    val x27 = x2(2) = x24
    val x32 = x2(3)
    val x34 = x2(3) = x32
    val x35 = x2(4)
    val x37 = x2(4) = x35
    val x29 = x0(2)
    val x38 = x35 + x29
    val x39 = x2(4) = x38
    val x31 = x0(4)
    val x40 = x38 + x31
    val x41 = x2(4) = x40
    x2
  }
}
/*****************************************
End of Generated Code
*******************************************/
