package com.github.riedelcastro.theppl.util

/**
 * A Monad for assembling arguments and creating structures from these arguments.
 */
trait Builder[A <: Product, B] {
  self =>
  
  import Builder._

  def argument: A
  def built: B

  def map[T](f: A => T): Builder[A, T] = {
    new Builder[A, T] {
      def argument = self.argument
      def built = f(argument)
    }
  }

//  def flatMap[A1, B1](f: A => Builder[A1, B1]): Builder[(A, A1), B1] = {
//    new Builder[(A, A1), B1] {
//      def applied = f(self.argument)
//      def argument = (self.argument, applied.argument)
//      def built = applied.built
//    }
//  }
  def flatMap[A1 <: Product, B1, RA <: Product](f: A => Builder[A1, B1])(implicit tb:TupleBuilder[A, A1, RA]): Builder[RA, B1] = {
    new Builder[RA, B1] {
      def applied = f(self.argument)
      def argument = tb.apply(self.argument, applied.argument)
      def built = applied.built
    }
  }


//  def x[A1, B1, R](that: Builder[A1, B1])(implicit tb:TupleBuilder[A,A1,R]): Builder[R, B1] = {
//    new Builder[R, B1] {
//      def argument = tb(self.argument,that.argument)
//      def built = that.built
//    }
//  }

}

object Builder {
  trait TupleBuilder[Left,Right,Result] {
    def apply(left:Left, right:Right):Result
  }
  implicit def tb11[Left,Right] = new TupleBuilder[Tuple1[Left], Tuple1[Right], (Left, Right)] {
    def apply(left: Tuple1[Left], right: Tuple1[Right]) = (left._1,right._1)
  }
  implicit def tb21[L1,L2,Right] = new TupleBuilder[(L1,L2), Tuple1[Right], (L1,L2, Right)] {
    def apply(left: (L1,L2), right: Tuple1[Right]) = (left._1,left._2,right._1)
  }
  implicit def tb12[Left,R1,R2] = new TupleBuilder[Tuple1[Left], (R1, R2), (Left,R1, R2)] {
    def apply(left: Tuple1[Left], right: (R1, R2)) = (left._1,right._1,right._2)
  }



}

trait EmptyBuilt[A <: Product] extends Builder[A, Nothing] {
  def built = sys.error("Can't call built on empty built")
}


object BuilderTest {

  def main(args: Array[String]) {
    case class D[A](inner: A) extends EmptyBuilt[Tuple1[A]] {
      def argument = Tuple1(inner)
    }
//    val test = for (x <- D(1); y <- D(2); z <- D(3); d <- D(4)) yield x -> y -> z
    val test2 = for (x <- D(1); y <- D(2); z <- D(3)) yield x._1
    println(test2.argument._1)
    //    val test = for (x <- D(1, 1)) yield 5 //x -> y
    //    val test2 = D(1,1).flatMap(x => D(2,2))
    //    val test3 = for (x <- D(1,1)) yield 5//x + y
    //    println(test2.map({case (x,y) => x + y}).built)
    //    println(test3.built)

    println(test2.built)
  }
}





