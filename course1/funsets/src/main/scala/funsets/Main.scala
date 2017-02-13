package funsets

object Main extends App {
  import FunSets._
  //println(contains(singletonSet(1), 1))
  private val superset = union(singletonSet(1), singletonSet(2))
  println(superset.toString())

  println(exists(superset, a=>a<1))
}
