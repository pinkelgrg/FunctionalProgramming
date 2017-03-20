package com.fpinscala.ch3

/**
  * Created by c0dem0nkey on 3/19/17.
  */

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
  case Nil => 0 // The sum of the empty list is 0.
  case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
}

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  /*
    3.2
    Implement the function tail for removing the first element of a List.
    Note that the function takes constant time. What are different choices
    you could make in your implementation if the List is Nil?
    We’ll return to this question in the next chapter.
  */

  def tail[A](l: List[A]): List[A] = l match {

    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons( _, xs ) => xs

  }

  /*
    3.3
    Using the same idea, implement the function setHead for replacing
    the first element of a List with a different value.
  */

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Cons(h, Nil)
    case Cons(_, xs) => Cons(h, xs)

  }
  /*
    Ex: 3.4
    Generalize tail to the function drop, which removes the first n elements from a list.
    Note that this function takes time proportional only to the number of elements being
    dropped—we don’t need to make a copy of the entire List.
  */
  def drop[A](l: List[A], n: Int): List[A] = l match {

    case Nil => l
    case Cons ( _, xs ) =>
      if (n == 0) l
      else
        drop(tail(l), n-1)
  }
  /*
    Ex: 3.5
    Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
  */
  def dropWhile1[A](l: List[A])(f: A => Boolean): List[A] =  l match {

    case Nil => Nil
    case Cons(x, xs) => {
      if (f(x)){
        dropWhile1(xs)(f)
      }else{
        l
      }
    }
  }

  def dropWhile[A](l: List[A],f: A => Boolean): List[A] =  l match {

    case Nil => Nil
    case Cons(x, xs) => {
      if (f(x)){
        dropWhile(xs, f)
      }else{
        l
      }
    }
  }

  /*
    Ex: 3.6
    Implement a function, init, that returns a List consisting of
    all but the last element of a List. So, given List(1,2,3,4),
    init will return List(1,2,3). Why can’t this function be implemented
    in constant time like tail?
  */
  def init[A](l: List[A]): List[A] = l match {

    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))

  }

  /*
    3.9
    Compute the length of a list using foldRight.
  */

  def length[A](l: List[A]): Int = l match {

    case Nil => 0
    case Cons(x, y) =>
      foldRight(l, 0)((x, y) => if (x != Nil) 1 + y
      else
      0
      )
  }
  /*
    3.10
    Our implementation of foldRight is not tail-recursive and will result in a
    StackOverflowError for large lists (we say it’s not stack-safe).
    Convince yourself that this is the case, and then write another general
    list-recursion function, foldLeft, that is tail-recursive, using the
    techniques we discussed in the previous chapter
  */

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /*
    Ex: 3.12
    Write a function that returns the reverse of a list (given List(1,2,3)
    it returns List(3,2,1)). See if you can write it using a fold.
  */
//  def reverse[A](l: List[A]): List[A] = {
//
//
//
//
//  }


//
//  def map[A,B](l: List[A])(f: A => B): List[B] = ???

  def main(args: Array[String]): Unit = {

    println("Ex: 3.2")
    println (tail(List(1, 2, 3)))
    println()

    println("Ex: 3.3")
    println (setHead(List(1,2,3,4), 5))
    println()

    println("Ex: 3.4")
    println (drop(List(1, 2, 3, 4, 5), 2))
    println()

    println("Ex: 3.5")
    println (dropWhile(List(5, 4, 3, 2, 1), (x: Int) => x >= 3))
    println()
    println (dropWhile1(List(5, 4, 3, 2, 1))(x => x > 4))
    println()

    println("Ex: 3.6")
    println (init(List(1, 2, 3, 4, 5)))
    println()

    println("Ex: 3.9")
    println (length(List(1, 2, 3, 4, 5)))
    println()

    println("Ex: 3.11 foldLeft Sum")
    println (foldLeft(List(1,2,3,4,5),0)(_+_))
    println()


    println("Ex: 3.11 foldLeft Product")
    println (foldLeft(List(1,2,3,4,5),1)(_*_))
    println()

    println("Ex: 3.12")
    // println (reverse(List(1,2,3)))
    println()

  }
}