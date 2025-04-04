// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.adt

import java.util.NoSuchElementException

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])


object List: 

  def head[A] (l: List[A]): A = l match
    case Nil => throw NoSuchElementException() 
    case Cons(h, _) => h                                                                                                                                                                                                                                       
  
  def apply[A] (as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def append[A] (l1: List[A], l2: List[A]): List[A] =
    l1 match
      case Nil => l2
      case Cons(h, t) => Cons(h, append(t, l2)) 

  def foldRight[A, B] (l: List[A], z: B, f: (A, B) => B): B = l match
    case Nil => z
    case Cons(a, as) => f(a, foldRight(as, z, f))
    
  def map[A, B] (l: List[A], f: A => B): List[B] =
    foldRight[A, List[B]] (l, Nil, (a, z) => Cons(f(a), z))

  // Exercise 1 (is to be solved without programming)

  //3 
  //its calling the List function??  

  // Exercise 2

  def tail[A] (l: List[A]): List[A] = l match
    case Nil => throw NoSuchElementException()
    case Cons(head, tail) => tail
  

  // Exercise 3
  
  def drop[A] (l: List[A], n: Int): List[A] = (n,l) match 
    case (n,l) if n <= 0 => l
    case (_, Nil) => throw NoSuchElementException()
    case (i, Cons(_,tail)) => drop(tail, n-1)

  // Exercise 4

  def dropWhile[A] (l: List[A], p: A => Boolean): List[A] = l match
    case Cons(head, tail) if p(head) => dropWhile(tail, p)
    case _ => l
  

  // Exercise 5
 
  def init[A] (l: List[A]): List[A] = l match
    case Nil => throw NoSuchElementException()
    case Cons(head,Nil) => Nil
    case Cons(head, tail) => Cons(head,init(tail))
  
  //no and no
  

  // Exercise 6

  def length[A] (l: List[A]): Int = 
    foldRight(map(l,a=>1),0,_+_)  

  // Exercise 7
  @annotation.tailrec
  def foldLeft[A, B] (l: List[A], z: B, f: (B, A) => B): B = l match
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z,head),f)
  
  // Exercise 8

  def product (as: List[Int]): Int = 
    foldLeft(as,1,(_*_))

  def length1[A] (as: List[A]): Int = 
    foldLeft(as,0,(b,a) => b+1)

  // Exercise 9

  def reverse[A] (l: List[A]): List[A] = 
  foldLeft(l,Nil,(acc,e) => Cons(e,acc))
  // Exercise 10

  def foldRight1[A, B] (l: List[A], z: B, f: (A, B) => B): B = ???

  // Exercise 11

  def foldLeft1[A, B] (l: List[A], z: B, f: (B, A) => B): B = 
    foldRight[A,B=>B](l,b=>b,((e1,g)=>(e2) => g(f(e2,e1))))(z)
 
  // Exercise 12

  def concat[A] (l: List[List[A]]): List[A] = ???
  
  // Exercise 13

  def filter[A] (l: List[A], p: A => Boolean): List[A] = ???
 
  // Exercise 14

  def flatMap[A,B] (l: List[A], f: A => List[B]): List[B] = ???

  // Exercise 15

  def filter1[A] (l: List[A], p: A => Boolean): List[A] = ???

  // Exercise 16

  def addPairwise (l: List[Int], r: List[Int]): List[Int] = ???

  // Exercise 17

  def zipWith[A, B, C] (l: List[A], r: List[B], f: (A,B) => C): List[C] = ???

  // Exercise 18

  def hasSubsequence[A] (sup: List[A], sub: List[A]): Boolean = ???
