
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  //var offsets = List((-1,-1), (0, -1), (1,-1), (-1,0), (1, 0), (-1,1), (0,1), (1,1) )
  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {

//    var allNeighboursRgba = (for (x<- -radius to radius; y<- -radius to radius) yield (x,y)).map(o=>(x+o._1, y +o._2))
//      .filter(pixel=>(pixel._1 >= 0 && pixel._1 < src.width && pixel._2 >= 0 && pixel._2 < src.height))
//      .map(neighbor=>src(neighbor._1, neighbor._2))
//
//    allNeighboursRgba.sum /allNeighboursRgba.length

    val minX = clamp(x - radius, 0, src.width -1)
    val maxX = clamp(x + radius, 0, src.width -1)

    val minY = clamp(y - radius, 0, src.height -1)
    val maxY = clamp(y + radius, 0, src.height -1)

        var counter =0
        var sum =0
    var yy = minY

    while(yy <= maxY){
      var xx = minX
      while(xx <= maxX) {
        sum += src(xx, yy)
        counter += 1
        xx += 1
      }
      yy+=1
    }
//    for(xx<-minX to  maxX; yy<-minY to maxY){
//      sum += src(xx, yy)
//      counter += 1
//    }

    sum/counter
  }

}
