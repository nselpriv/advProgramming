// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Monads for functional programming by Phil Wadler (Sections 1-2)

// ** INTRODUCTION **
//
// Read the two first sections of "Monads for functional programming" by Phil
// Wadler (linked from the course's website, also found in the repo).
//
// The paper's main point is that monads allow bringing some imperative
// programming style into functional programming, so functional programs are
// easy to modify in situations where imperative programs are easy to modify.
// It emphasizes how different side effects can be implemented in a
// referentially transparent manner, all in similar style. So we also learn how
// different programming language features can be modeled.

package adpro.eval

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.*

// Equivalence is type checked stronger than Equality
// and its definition does not have issues with Matchable in Scala 3
import org.scalactic.TypeCheckedTripleEquals.*
import org.scalactic.Equivalence

// We first include implementation of Functor and Monad type classes for
// convenience (these are taken from the textbook repo, but should  be the same
// or very similar to ours from the monad week)
// You can scroll down until "end Monad"

trait Functor[F[_]]:
  extension [A](fa: F[A])
    def map[B](f: A => B): F[B]

  extension [A, B](fab: F[(A, B)])
    def distribute: (F[A], F[B]) =
      (fab.map(_(0)), fab.map(_(1)))

  extension [A, B](e: Either[F[A], F[B]])
    def codistribute: F[Either[A, B]] = e match
      case Left(fa) => fa.map(Left(_))
      case Right(fb) => fb.map(Right(_))

end Functor


trait Monad[F[_]] extends Functor[F]:
  def unit[A](a: => A): F[A]

  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B] =
      fa.map(f).join

    def map[B](f: A => B): F[B] =
      fa.flatMap(a => unit(f(a)))

    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldRight(unit(List[A]()))((fa, acc) => fa.map2(acc)(_ :: _))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, acc) => f(a).map2(acc)(_ :: _))

  extension [A](ffa: F[F[A]]) def join: F[A] =
    ffa.flatMap(identity)

  object laws:

    /* The triple equality below (===) uses the Equality instance to check
     * equality (by default the same as == but we can override it). */

    def associative[A, B, C](using Arbitrary[F[A]], Arbitrary[A => F[B]],
      Arbitrary[B => F[C]], Equivalence[F[C]]): Prop =
      forAll { (x: F[A], f: A => F[B], g: B => F[C]) =>
        val left = x.flatMap[B](f).flatMap[C](g)
        val right = x.flatMap[C] { a => f(a).flatMap[C](g) }
        (left === right) :| s"left:$left right:$right"
      }

    def identityRight[A](using Arbitrary[F[A]], Arbitrary[A => F[A]], Equivalence[F[A]]) =
      forAll { (x: F[A], f: A => F[A]) =>
        val result = x.flatMap[A](unit[A])
        (result === x) :| s"got:$result expected:$x" }

    def identityLeft[A: Arbitrary](using Arbitrary[A => F[A]], Equivalence[F[A]]) =
      forAll { (y: A, f: A => F[A]) =>
        val left = unit[A](y).flatMap[A](f)
        val right = f(y)
        (left === right) :| s"left:$left right:$right" }

    def identity[A: Arbitrary](using Arbitrary[F[A]], Arbitrary[A => F[A]],
      Equivalence[F[A]]): Prop =
      { "identity left: " |: identityLeft[A]  } &&
      { "identity right:" |: identityRight[A] }

    def monad[A: Arbitrary, B, C] (using Arbitrary[F[A]], Arbitrary[A => F[A]],
      Arbitrary[A => F[B]], Arbitrary[B => F[C]]): Prop =
      { "associative:" |: this.associative[A,B,C] } &&
      { "identity:   " |: this.identity[A] }

  end laws

end Monad

// Section 2.1 [Wadler]
// It may be a good idea to read this code and the paper in parallel.
// Wadler uses a langauge similar to Haskell to implement his evaluator.
// We will use Scala.  This is Wadler's Term language in Scala:

enum Term:
  case Cons(value: Int)
  case Div(left: Term, right: Term)

import Term.*

object BasicEvaluator:
  def eval(term: Term): Int = term match
    case Cons(a) => a
    case Div(t,u) => eval(t) / eval(u)

// Section 2.2: Exceptions

object ExceptionEvaluator:

  // Exercise 1
  //
  // Complete the implementation of eval with exceptions based on the paper.
  // You will need a lot of pattern matching here.  Pattern matching is common
  // in implementation of language processing tools.

  type Exception = String
  enum M[+A]:
    case Raise (e: Exception) extends M[Nothing]
    case Return (a: A)

  import M.*

  def eval(term: Term): M[Int] = term match
    case Cons(value) => Return(value)
    case Div(left, right) => eval(left) match
      case Raise(e) => Raise(e)
      case Return(a1) => eval(right) match
        case Raise(e) => Raise(e)
        case Return(a2) => 
            if a2 == 0 then
              Raise("Divison by zero")
            else 
              Return(a1/a2)
      
    
  

  // Exercise 2
  //
  // Provide evidence that M is a Monad (the implementation of the Monad type
  // class is in Monad). The test suite will automatically check whether it satisfies the
  // monad laws.  If you lack intuition, notice that M is essentially Option or
  // Either, with Return being Some.

  given mIsMonad: Monad[M] = new Monad[M] {

    def unit[A](a: => A): M[A] = M.Return(a)

      extension [A](fa: M[A]) override def flatMap[B](f: A => M[B]): M[B] = fa match
        case Raise(e) => M.Raise(e)
        case Return(a) => f(a)
    
      
  }

  // Exercise 3
  //
  // Reimplement eval using unit and flatMap from the above monad instance
  // This exercise will not work, if the previous is not implemented.
  // You will still need pattern matching here, but on the language constructs only,
  // not on the results.
  //
  // Think: Why is flatMap suddendly available, and it was not in Exercise 1?

  def evalMonad(term: Term): M[Int] = term match
    case Cons(value) => summon[Monad[M]].unit(value)
    case Div(left, right) => 
      evalMonad(left).flatMap{a => 
        evalMonad(right).flatMap{b => 
          if b == 0 then 
            M.Raise("division by zero")
          else
            summon[Monad[M]].unit(a/b)}
      }
  

  // Exercise 4
  //
  // Implement this eval once again but now using for yield. We still use a bit
  // of pattern matchin on the language operators like above.
  //
  // We cannot use the for-if-yield construct, because we did not implement
  // withFilter in M. This is easy to add, you can try, but this is not required material.
  // https://www.scala-lang.org/api/3.x/scala/collection/WithFilter.html#withFilter-fffffb75
  // The exercise can be completed without withFilter, just 1-2 lines longer

  def evalForYield(term: Term)(using m: Monad[M]): M[Int] = term match
    case Cons(value) => m.unit(value)
    case Div(left, right) => 
      for {
        a <- evalForYield(left)
        b <- evalForYield(right)
        result <- if b == 0 then 
            M.Raise("division by zero")
          else m.unit(a/b)

      } yield result
  

end ExceptionEvaluator


// Section 2.3: State

object StateEvaluator:

  type State = Int
  case class M[+A] (step: State => (A,State))

  // This is a week equivalence test that we need because
  // we cannot compare functions in Scala
  given equiv[A]: Equivalence[M[A]] = new:
    def areEquivalent(a1: M[A], a2: M[A]): Boolean =
      (-500 to 500).forall { n => a1.step(n) == a2.step(n) }


  // Exercise 5
  //
  // Complete the implementation of the evaluator as per the spec in the
  // paper.  We do not have a monad instance for M, so we have to use pattern
  // matching on the result.
  //
  // Remember that the State evaluators in the paper do not track divisions by zero
  // (they track something different).

  def eval (term: Term): M[Int] = term match
    case Cons(value) => 
      M(s => (value,s))
    case Div(left, right) => 
      M(s =>  
        val (a, sa) = eval(left).step(s)
        val (b, sb) = eval(right).step(sa)
        if  b == 0 
        then
          throw new RuntimeException("oh no")
        else
          (a / b, sb +1)
      )

  

  // Exercise 6
  //
  // Provide evidence that M is a Monad (the implementation of the Monad type
  // class is in Monad). The test suite will automatically check whether it satisfies the
  // monad laws.  If you lack intuition notice, that M is essentially our adpro.State
  //
  // Note that we could implement the counter incrementation in flatMap, but
  // this then would count not only divisions, so let's not do that.

  given mIsMonad: Monad[M] = new Monad[M] {

    override def unit[A](a: => A): M[A] = M(state => (a, state))

    extension [A](fa: M[A]) override def flatMap[B](f: A => M[B]): M[B] = 
      M (state => 
        val (a, newState) = fa.step(state)
        f(a).step(newState)
      )
  }

  // Exercise 7
  //
  // Reimplement eval using the above instance of Monad (flatMap + unit).

  def tick: M[Unit] = M { (x: State) => ((), x+1) }

  def evalMonad (term: Term): M[Int] = term match
    case Cons(value) =>
      M(state => (value, state))  

    case Div(left, right) =>
      evalMonad(left).flatMap { lValue =>       
        evalMonad(right).flatMap { rValue =>   
          if rValue == 0 then
            M(state => throw new RuntimeException("Division by zero"))  
          else
            tick.flatMap(_ => M(state => (lValue / rValue, state))) 
        }
      }
  

  // Exercise 8
  //
  // Reimplement the above using for-yield

  def evalForYield (term: Term)(using m: Monad[M]): M[Int] = term match
    case Cons(value) => 
      m.unit(value)
    case Div(left, right) => 
      for 
        a <- evalForYield(left)
        b <- evalForYield(right)
        _ <- if b == 0 then
            M(state => throw new RuntimeException("oh no"))
             else 
            tick
      yield a / b

end StateEvaluator

// Section 2.4 [Wadler] Variation three: Output

object OutputEvaluator:

  // We now define the state of an evaluator that outputs the results of
  // reductions
  type Output = String
  case class M[+A] (o: Output, a: A)

  // A helper function from the paper
  def line (a: Term) (v: Int): Output =
    "eval(" + a.toString + ") <= " + v.toString + "\n"

  // Exercise 9
  //
  // Complete the implementation of the evaluator as per the spec in the
  // paper, Section 2.4.  Again don't worry about divisions by zero or
  // counting divisions, we only produce the trace in this exercise.

  def eval (term: Term): M[Int] = term match
    case Cons(value) => M(line(term)(value), value)
    case Div(left, right) =>
      val leftEval = eval(left)
      val rightEval = eval(right)
      val result = leftEval.a / rightEval.a
      val trace = leftEval.o + rightEval.o + line(term)(result)
      M(trace, result)
  

  // Exercise 10
  //
  // Provide evidence that M is a Monad (the implementation of the Monad type
  // class is in Monad). The test suite will automatically check whether it
  // satisfies the monad laws.

  given mIsMonad: Monad[M] = new Monad[M] {

    override def unit[A](a: => A): M[A] = M("", a)

    extension [A](fa: M[A]) override def flatMap[B](f: A => M[B]): M[B] = fa match
      case M(o, a) => 
        val M(o2, b) = f(a)
        M(o + o2,b)
  }

  // Exercise 11
  //
  // Now reimplement eval using unit and flatMap

  def out (o: Output): M[Unit] = M (o, ())

  def evalMonad (term: Term)(using m:Monad[M]): M[Int] = term match
    case Cons(value) => 
        m.unit(value).flatMap { v =>
          out(line(term)(v)).flatMap { _ => m.unit(v) }
        }
    case Div(left, right) => 
      evalMonad(left).flatMap { lValue =>
        evalMonad(right).flatMap { rValue =>
          if rValue == 0 then
            throw new RuntimeException("oh no")
          else
            val result = lValue / rValue
            out(line(term)(result)).flatMap { _ => m.unit(result) }
        }
      }
  

  // Exercise 12
  //
  // Reimplement the above using for-yield

  def evalForYield (term: Term)(using m: Monad[M]): M[Int] = term match 
    case Cons(value) => 
      for {
        v <- m.unit(value)
        _ <- out(line(term)(v))
      } yield v
    case Div(left, right) => 
      for {
        lValue <- evalMonad(left)
        rValue <- evalMonad(right)
        result <- {
          if rValue == 0 then
            throw new RuntimeException("oh no")
          else
            val res = lValue / rValue
            out(line(term)(res)).flatMap { _ => m.unit(res) }
        }
      } yield result

   // Exercise 13
   //
   // Read Sections 2.7-2.9 in the paper and revisit the monadic
   // implementations (evalMonad and evalForYield) to make them in line
   // with the paper.  Can you get to the same point as Wadler,
   // that changing a side effect requires a small change in the interpreter?
   //
   // No new writing in this exercise, just refactoring
   // the 4 earlier exercises.


  

object Main extends App {
  
  // Sample terms for testing
  val const: Term = Cons(42)
  val answer: Term = Div(Cons(6), Cons(2))
  val error: Term = Div(Cons(6), Cons(0))

  // Eval function to print the results
  def printEval(term: Term): Unit = {
    try {
      val result = OutputEvaluator.evalMonad(term)
      println(s"Result: ${result.a}")
      println(s"Trace: ${result.o}")
    } catch {
      case _ => println(s"Exception:")
    }
  }

  // Print eval outputs for different terms
  println("Test 1: Evaluating a constant")
  printEval(const)  // Should print the eval trace for Cons(42)

  println("\nTest 2: Evaluating a division")
  printEval(answer)  // Should print the eval trace for Div(6, 2)

  println("\nTest 3: Division by zero")
  printEval(error)  // Should print exception for division by zero

}
