/*****************************************
Emitting Generated Code
*******************************************/
class Snippet(px3:Array[Array[Int]]) extends ((Unit)=>(Boolean)) {
  def apply(x164:Unit): Boolean = {
    val x165 = println("Realizing brighter Func")
    val x166 = println("Generating loopMap")
    val x15 = 16 % 5
    val x16 = x15 == 0
    val x3 = px3 // static data: Array([I@580f1a5a,[I@478b37e0,[I@50177d2,[I@23617e8f,[I@55bbf06e,[I@25a5f1bb,[I@31a13f4c,[I@5142b008,[I@713306e5,[I@4d09de72,[I@42eb35e1,[I@601e6919,[I@6c86199b,[I@57fb46c6,[I@31e39e4a,[I@56ad25dc)
    var x167 : Int = 0
    val x320 = while (x167 < 4) {
      val x211 = x167 * 5
      val x216 = x211 >= 11
      var x168 : Int = 0
      val x318 = while (x168 < 4) {
        val x176 = x168 * 5
        val x181 = x176 >= 11
        var x169 : Int = 0
        val x316 = while (x169 < 5) {
          val x212 = x211 + x169
          var x170 : Int = 0
          val x314 = while (x170 < 5) {
            val x171 = println("Loops Generated with schedule: Map(Var(x_inner) -> Sym(170), Var(y_inner) -> Sym(169), Var(x_outer) -> Sym(168), Var(y_outer) -> Sym(167))")
            var x172: Int = 0
            var x173: Int = 0
            var x174: Int = 0
            x174 = 16
            val x191 = if (x16) {
              val x177 = x176 + x170
              x173 = x177
              ()
            } else {
              var x180: Int = 0
              val x186 = if (x181) {
                x180 = 11
                ()
              } else {
                x180 = x176
                ()
              }
              val x187 = x180
              val x188 = x187 + x170
              x173 = x188
              ()
            }
            val x204 = if (x16) {
              val x177 = x176 + x170
              x173 = x177
              ()
            } else {
              var x194: Int = 0
              val x199 = if (x181) {
                x194 = 11
                ()
              } else {
                x194 = x176
                ()
              }
              val x200 = x194
              val x201 = x200 + x170
              x173 = x201
              ()
            }
            val x205 = x173
            x172 = x205
            var x207: Int = 0
            var x208: Int = 0
            var x209: Int = 0
            x209 = 16
            val x226 = if (x16) {
              x208 = x212
              ()
            } else {
              var x215: Int = 0
              val x221 = if (x216) {
                x215 = 11
                ()
              } else {
                x215 = x211
                ()
              }
              val x222 = x215
              val x223 = x222 + x169
              x208 = x223
              ()
            }
            val x239 = if (x16) {
              x208 = x212
              ()
            } else {
              var x229: Int = 0
              val x234 = if (x216) {
                x229 = 11
                ()
              } else {
                x229 = x211
                ()
              }
              val x235 = x229
              val x236 = x235 + x169
              x208 = x236
              ()
            }
            val x240 = x208
            x207 = x240
            val x242 = println("--------------------------------------------------------------------------------------")
            val x243 = println("3 chls For loop")
            var x250: Int = 0
            val x251 = println("RGB channels if, else if")
            val x252 = println("red channel")
            val x248 = x3(x205)
            val x249 = x248(x240)
            val x253 = x249 & 16711680
            val x254 = x253 >>> 16
            x250 = x254
            var x260: Int = 0
            val x256 = x254
            val x257 = x256.toDouble
            val x258 = x257 * 1.2
            val x259 = x258.toInt
            val x261 = x259 >= 255
            val x266 = if (x261) {
              x260 = 255
              ()
            } else {
              x260 = x259
              ()
            }
            val x267 = x260
            val x268 = println("3 chls For loop")
            var x269: Int = 0
            val x270 = println("RGB channels if, else if")
            val x271 = println("green channel")
            val x272 = x249 & 65280
            val x273 = x272 >>> 8
            x269 = x273
            var x279: Int = 0
            val x275 = x273
            val x276 = x275.toDouble
            val x277 = x276 * 1.2
            val x278 = x277.toInt
            val x280 = x278 >= 255
            val x285 = if (x280) {
              x279 = 255
              ()
            } else {
              x279 = x278
              ()
            }
            val x286 = x279
            val x287 = println("3 chls For loop")
            var x288: Int = 0
            val x289 = println("RGB channels if, else if")
            val x290 = println("blue channel")
            val x291 = x249 & 255
            x288 = x291
            var x297: Int = 0
            val x293 = x291
            val x294 = x293.toDouble
            val x295 = x294 * 1.2
            val x296 = x295.toInt
            val x298 = x296 >= 255
            val x303 = if (x298) {
              x297 = 255
              ()
            } else {
              x297 = x296
              ()
            }
            val x304 = x297
            val x305 = println("Out of 3 chls For loop")
            val x306 = println("alpha...")
            val x307 = x267 << 16
            val x308 = -16777216 | x307
            val x309 = x286 << 8
            val x310 = x308 | x309
            val x311 = x310 | x304
            val x312 = x248(x240) = x311
            x170 = x170 + 1
          }
          x169 = x169 + 1
        }
        x168 = x168 + 1
      }
      x167 = x167 + 1
    }
    val x321 = println("Checking the output image array! Saving the picture.")
    true
  }
}
/*****************************************
End of Generated Code
*******************************************/
