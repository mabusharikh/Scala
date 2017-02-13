import objsets._

abstract class Nat {
  def isZero:Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def +(other:Nat) :Nat
  def -(other:Nat) :Nat
}

object Zero extends Nat{
  override def isZero: Boolean = true

  override def predecessor: Nat = throw new Error("Zero.NoNegative")

  override def +(other: Nat): Nat = other

  override def -(other: Nat): Nat = throw new Error("Zero.CannotSubtract")
}

class Succ(n: Nat) extends Nat{
  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def +(other: Nat): Nat = new Succ(other+n)

  override def -(other: Nat): Nat = new Succ(n-other)
}


object List : {

}