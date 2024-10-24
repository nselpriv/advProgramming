error id: file://<WORKSPACE>/Exercises.scala:[4436..4439) in Input.VirtualFile("file://<WORKSPACE>/Exercises.scala", "// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.state

import adpro.lazyList.LazyList
import adpro.lazyList.LazyList.*


trait RNG:
  /** Generate a random `Int`. We define other functions using `nextInt`. */
  def nextInt: (Int, RNG) 

object RNG:

  case class SimpleRNG(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL 
      // The next state, which is an `RNG` instance created from the new seed. 
      val nextRNG = SimpleRNG(newSeed)
      // `>>>` is right binary shift with zero fill. 
      // The value `n` is our new pseudo-random integer.
      val n = (newSeed >>> 16).toInt 
      // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      (n, nextRNG) 


  // Exercise 1

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (s, nextRNG) = rng.nextInt
    val v = if s < 0 then s * -1 else s
    (v, nextRNG)

  // Exercise 2

  def double(rng: RNG): (Double, RNG) = 
    val (s, nextRNG) = nonNegativeInt(rng)
    (s / (Int.MaxValue.toDouble + 1),nextRNG) 

  // Exercise 3
  
  // The return type is broken and needs to be fixed
  def intDouble(rng: RNG): ((Int, Double), RNG) = 
    val (v1,r1) = nonNegativeInt(rng)
    val (v2,r2) = double(r1)
    ((v1,v2), r2)

  // The return type is broken and needs to be fixed
  def doubleInt(rng: RNG): ((Double, Int), RNG) = 
    val (v1,r1) = nonNegativeInt(rng)
    val (v2,r2) = double(r1)
    ((v2,v1), r2)

  // Exercise 4
  def ints(size: Int)(rng: RNG): (List[Int],RNG) = 
    if size <= 0 
    then (Nil,rng)
    else 
      val(head, rng1) = nonNegativeInt(rng)
      val(tail,rng2) = ints(size-1)(rng1)
      (head :: tail,rng2)

  // The return type is broken and needs to be fixed
  //def intss(size: Int)(rng: RNG): (List[Int],RNG) = 
  //  val step(r: RNG): (RNG,Int) =
  //    r match
  //      case r : RNG => nonNegativeEven(r)
  //  val loop(rrr: RNG) :  =
  //   val (output, state1) = step(state)
      

  type Rand[+A] = RNG => (A, RNG)

  lazy val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt) { i => i - i % 2 }

  // Exercise 5

  //s / (Int.MaxValue.toDouble + 1) 
  //def double(rng: RNG): (Double, RNG) = 
  //  val (s, nextRNG) = nonNegativeEven(rng)
  //  (s / (Int.MaxValue.toDouble + 1),nextRNG) 


  lazy val double2: Rand[Double] = 
    map(nonNegativeInt) (i => i / (Int.MaxValue.toDouble + 1 ))

  // Exercise 6

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    rng => {
      val (a, rng1) = ra (rng)
      val (b, rng2) = rb (rng1)
      (f(a,b),rng2)
    }

  // Exercise 7

  //def sequence[A](ras: List[Rand[A]]): Rand[List[A]] =
  //  rng => {
  //    ras match
  //      case Nil => (Nil, rng)
  //      case head :: next => 
  //        val (h, rng1) = head(rng)
  //        val (t, rng2) = sequence(next)(rng1)
  //        (h :: t, rng2)
  //  }

  //def sequence[A](ras: List[Rand[A]]): Rand[List[A]] = 
  //  rng => {
  //    ras.foldLeft[(List[A], RNG)](Nil, rng)
  //      ((acc, elem) => 
  //        val (a, rng1) = elem (acc._2)
  //        (a::acc._1,rng))
  //  }

  def sequence[A](ras: List[Rand[A]]): Rand[List[A]] =
      ras.foldRight[Rand[List[A]]](unit(Nil))(map2(_,_)(_::_))



  //def sequence[A](aos: List[Option[A]]): Option[List[A]] =
  //    aos.foldRight[Option[List[A]]](Some(List.empty[A]))(map2(_,_)(_::_))




  def ints2(size: Int): Rand[List[Int]] =
    sequence(List.fill(size)(int))

  // Exercise 8

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = initalState => {
    val (value, newState) = f(initalState)
    g(value)(newState)
  }
    

  def nonNegativeLessThan(bound: Int): Rand[Int] =
   flatMap(nonNegativeInt): i => 
    val r = bound
    if i + (r + 1) - i%r >=0 then unit(i % r) else nonNegativeLessThan(r) 
  ///uhhhhhhhhh why is this in the book?
  //could we not just do simple i%r >=0?
end RNG

import State.*

case class State[S, +A](run: S => (A, S)):

  // Exercise 9 (methods in class State)
  // Search for the second part (sequence) below
  def 

  def flatMap[B](f: A => State[S, B]): State[S, B] = 
    State {s => 
        val (a, s1) = run(s)
        f(a).run(s1)
    }

  def map[B](f: A => B): State[S, B] = 
    ???

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = 
    ???


object State:

  def unit[S, A](a: A): State[S, A] =
    State { s => (a, s) }

  def modify[S](f: S => S): State[S, Unit] = for
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  // Now Rand can be redefined like this (we keep it here in the State object,
  // to avoid conflict with the other Rand in RNG).
  type Rand[A] = State[RNG, A]

  // Exercise 9 (sequence, continued)
 
  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] =
    ???

  import adpro.lazyList.LazyList

  // Exercise 10 (stateToLazyList)
  
  def stateToLazyList[S, A](s: State[S,A])(initial: S): LazyList[A] =
    ???

  // Exercise 11 (lazyInts out of stateToLazyList)
  
  def lazyInts(rng: RNG): LazyList[Int] = 
    ???

  lazy val tenStrictInts: List[Int] = 
    ???

end State
")
file://<WORKSPACE>/Exercises.scala
file://<WORKSPACE>/Exercises.scala:166: error: expected identifier; obtained def
  def flatMap[B](f: A => State[S, B]): State[S, B] = 
  ^
#### Short summary: 

expected identifier; obtained def