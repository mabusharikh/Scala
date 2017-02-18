import scala.util.{Try, Success, Failure}

trait Coin {}
case class Gold() extends Coin {}
case class Silver() extends Coin {}

val pf: PartialFunction[Coin, String] = {
  case Gold() => "a golden coin"
  // no case for Silver(), because we're only interested in Gold()
}

println(pf.isDefinedAt(Gold()))   // true
println(pf.isDefinedAt(Silver())) // false
println(pf(Gold()))               // a golden coin
//println(pf(Silver()))             // throws a scala.MatchError

trait Generator[+T] { self =>
  def generate: T
  def map[S](f: T => S) : Generator[S] = new Generator[S] {
    def generate = f(self.generate)
  }
  def flatMap[S](f: T => Generator[S]) : Generator[S] = new Generator[S] {
    def generate = f(self.generate).generate
  }
}

val integers = new Generator[Int] {
  val rand = new java.util.Random
  def generate = rand.nextInt()
}


val booleans = for {x <- integers} yield x > 0
val pairs = for {x <- integers; y<- integers} yield (x, y)
def interval(lo: Int, hi: Int) : Generator[Int] = for { x <- integers } yield lo + x % (hi - lo)

for {
  k <- interval(10, 20)
} yield k


//
//def answerToLife(nb: Int) : Try[Int] = {
//  if (nb == 42) Success(nb)
//  else Failure(new Exception("WRONG"))
//}
//
//answerToLife(42) match {
//  case Success(t)           => t        // returns 42
//  case failure @ Failure(e) => failure  // returns Failure(java.Lang.Exception: WRONG)
//}
//
//
