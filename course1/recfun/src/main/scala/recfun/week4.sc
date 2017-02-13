trait List[T]{
  val isEmpty: Boolean
  val head:T
  val tail:List[T]
}

class Cons[T](val head:T, val tail:List[T]) extends List[T]{
  override val isEmpty: Boolean = false
}

class Nil[T] extends List[T]{
  override val isEmpty: Boolean = true
  val head:Nothing = throw new Error("Nill.head")
  override val tail: List[T] = throw new Error("Nill.tail")
}

class MyList[T](val head:T, tail:List[T]) extends List[T]{
  def addElement(elem: T) List[T] = {

  }
  def nthElement(n: Int) : T{
  
  }
}