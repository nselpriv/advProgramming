// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition
//handin by Nicolai Seloy (nsel)

package adpro.option

import java.awt.Point
import scala.collection.View.Filter

// Exercise 1

trait OrderedPoint
  extends scala.math.Ordered[java.awt.Point]:

  this: java.awt.Point =>


  override def compare(that: java.awt.Point): Int = (that, this) match
    case (p1: Point, p2: Point) if p1 == p2 => 0
    case (p1: Point, p2: Point) if (p1.x < p2.x || (p1.x == p2.x && p1.y < p2.y)) => 1
    case _ => -1
  

    

// Try the following (and similar) tests in the repl (sbt console):
//
// import adpro.option.*
// val p = new java.awt.Point(0, 1) with OrderedPoint
// val q = new java.awt.Point(0, 2) with OrderedPoint
// assert(p < q)



// Chapter 3 Exercises

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

object Tree:

  // Exercise 2

  def size[A](t: Tree[A]): Int = t match
    case Leaf(value) => 1
    case Branch(left, right) => 1 + size(left) + size(right)

  // Exercise 3

  def maximum(t: Tree[Int]): Int = t match
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  

  // Exercise 4

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))

  // Exercise 5

  def fold[A,B](t: Tree[A])(f: (B, B) => B)(g: A => B): B = t match
    case Leaf(value) => g(value)
    case Branch(left, right) => f(fold(left)(f)(g),fold(right)(f)(g))
  
  //tree type, accum, function

  def size1[A](t: Tree[A]): Int =  
      fold[A, Int](t)((left, right) => left + right + 1)(_ => 1)

  def maximum1(t: Tree[Int]): Int = 
      fold[Int, Int](t)((left,right) => left max right)(identity)

  def map1[A, B](t: Tree[A])(f: A => B): Tree[B] = 
      fold[A, Tree[B]](t)((left,right) => Branch(left,right))(leaf => Leaf(f(leaf)))




enum Option[+A]:
  case Some(get: A)
  case None

  // Exercise 6

  def map[B](f: A => B): Option[B] = this match
    case Some(get) => Some(f(get))
    case None => None
    

  def getOrElse[B >: A] (default: => B): B = this match
    case Some(get) => get
    case None => default

  def flatMap[B](f: A => Option[B]): Option[B] =  this match
    case Some(get) => f(get)
    case None => None 
  

  def filter(p: A => Boolean): Option[A] = this match
    case Some(get) if(p(get)) => Some(get) 
    case Some(get) if!(p(get)) => None
    case None => None
    case _ => ??? //compiler complains.. i dont know how this is reachable unless some(None) is possible.. but that seems weird
  

  // Scroll down for Exercise 7, in the bottom of the file, outside Option

  def forAll(p: A => Boolean): Boolean = this match
    case None => true
    case Some(a) => p(a)

  


object Option:

  // Exercise 9

  def map2[A, B, C](ao: Option[A], bo: Option[B])(f: (A,B) => C): Option[C] =
    for 
      x1 <- ao
      x2 <- bo
    yield f(x1,x2)

  // Exercise 10

  def sequence[A](aos: List[Option[A]]): Option[List[A]] =
      aos.foldRight[Option[List[A]]](Some(List.empty[A]))(map2(_,_)(_::_))
  // Exercise 11

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight[Option[List[B]]](Some(List.empty[B]))((a, acc) => map2(f(a), acc)(_::_))

end Option



// Exercise that are outside the Option companion object

import Option.{Some, None}

def headOption[A](lst: List[A]): Option[A] = lst match
  case Nil => None
  case h:: t => Some(h)

// Exercise 7

def headGrade(lst: List[(String,Int)]): Option[Int] = lst match
  case Nil => None
  case head :: next => Some(head._2)


def headGrade1(lst: List[(String,Int)]): Option[Int] =
  for
    x1 <- headOption(lst)
    x2 <- Some(x1._2)
  yield x2

// Implemented in the text book

def mean(xs: Seq[Double]): Option[Double] =
  if xs.isEmpty then None
  else Some(xs.sum / xs.length)

// Exercise 8

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs).flatMap((m => mean(xs.map(x => math.pow(x -m,2)))))
// Scroll up, to the Option object for Exercise 9

def variance2(xs: Seq[Double]): Option[Double] =
  for
    x1 <- mean(xs)
    x2 <- mean(xs.map(x => math.pow(x -x1,2)))
  yield x2

// i find number two easier to understand.. it feels more like a declarative way to do it instead of everything being oneliners and functions wrapping other functions