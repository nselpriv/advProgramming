file://<WORKSPACE>/Exercises.scala
### java.lang.StringIndexOutOfBoundsException: begin 5514, end 5522, length 5519

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 5516
uri: file://<WORKSPACE>/Exercises.scala
text:
```scala
// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.lazyList

import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalacheck.Arbitrary.arbitrary

import lazyList00.* // uncomment to test the book laziness solution implementation
// import lazyList01.* // uncomment to test the broken headOption implementation
// import lazyList02.* // uncomment to test another version

/* Generators and helper functions */

import LazyList.*

/** Convert a strict list to a lazy-list */
def list2lazyList[A](la: List[A]): LazyList[A] = 
  LazyList(la*)

/** Generate finite non-empty lazy lists */
def genNonEmptyLazyList[A](using Arbitrary[A]): Gen[LazyList[A]] =
  for la <- arbitrary[List[A]].suchThat { _.nonEmpty }
  yield list2lazyList(la)
  
/** Generate an infinite lazy list of A values.
  *
  * This lazy list is infinite if the implicit generator for A never fails. The
  * code is ugly-imperative, but it avoids stack overflow (as Gen.flatMap is
  * not tail recursive)
  */
def infiniteLazyList[A: Arbitrary]: Gen[LazyList[A]] =
  def loop: LazyList[A] =
    summon[Arbitrary[A]].arbitrary.sample match
      case Some(a) => cons(a, loop)
      case None => empty
  Gen.const(loop)

/* The test suite */

object LazyListSpec 
  extends org.scalacheck.Properties("testing"):

  // Exercise 1

  property("Ex01.01: headOption returns None on an empty LazyList") = 
    empty.headOption == None

  property("Ex01.02: headOption returns the head of the stream packaged in Some") =

    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (n: Int) => cons(n,empty).headOption == Some(n) } :| "singleton" &&
    forAll { (s: LazyList[Int]) => s.headOption != None }      :| "random" 

  // Exercise 2

  val failList : LazyList[Unit] = cons((), ???)
  property("Ex02: Headoption does not force the tail of a lazy list") = 
    failList.headOption;
    true
  
  // Exercise 3

  def failListN(n:Int) : LazyList[Int] =
    if n <= 0 then Empty
    else Cons(() => ???, () => failListN(n-1))

  property("Ex03: take does not force any heads nor nay tails of the lazy list it manipulates") = 
    forAll(Gen.choose(0,42)) {(length:Int) =>
      forAll(Gen.choose(0,length)) {(n:Int) =>
        failListN(length).take(n);
        true
      }
    } 

  // Exercise 4

  def faillingListTake(n:Int) : LazyList[Int] = n match
    case 0 => Cons(() => ???, () => empty)
    case _ => Cons(() => 0, () => faillingListTake(n-1))


  property("Ex04.01: take(n) does not force n head when not forcing n first") =
    forAll{(n:Int) => faillingListTake(n).take(n); true}  

  property("Ex04.02: take(n) does not force n+1th head when not forcing n first") =
    forAll{(n:Int) => faillingListTake(n).take(n+1); true}  

  property("Ex04.02: take(n) does not force n+1th head when forcing n first") =
    forAll(Gen.choose(0,42)){(n:Int) => faillingListTake(n).take(n).toList; true}  
  
  // Exercise 5
  
  property("Ex05.01: take(n) applied twice is idempotent") =
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    given Arbitrary[Int] = Arbitrary(Gen.choose(0,100))

    forAll{ (s: LazyList[Int]) =>
      forAll{(n:Int) => s.take(n).take(n).toList== s.take(n).toList  
      }
     }
  
  // Exercise 6
  property("Ex06.01: drop(n) applied twice is the same as the sum of dropping") =
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    given Arbitrary[Int] = Arbitrary(Gen.choose(0,100))

    forAll{ (s: LazyList[Int]) =>
        forAll{(n:Int,a:Int) => s.drop(n).drop(a).toList== s.drop(n+a).toList  
      }
    }

  // Exercise 7

  def faillingListTail(n:Int) : LazyList[Int] = n match
    case 0 => Cons(() => 0, () => Empty)
    case _ => Cons(() => ???, () => faillingListTail(n-1))
  
  property("Ex07.01:drop(n) does not force any of first n elements") =
    given Arbitrary[Int] = Arbitrary(Gen.choose(0,100))
    forAll{(n:Int) => faillingListTail(n).drop(n).toList; true}


  // Exercise 8
  property("ex08") = 
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    forAll{(l:LazyList[Int]) => l.map(identity).toList == l.toList}

  // Exercise 9
  property("ex09") = 
    given Arbitrary[Int] = Arbitrary(Gen.choose(0,100))
    given Arbitrary[LazyList[Int]] = Arbitrary(infiniteLazyList[Int])

    forAll{(l: LazyList[Int]) => l.map(identity); true}
 
  // Exercise 10


  property("ex10.01 l.append(m).head = l.head") =
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    forAll {(l:LazyList[Int], m:LazyList[Int]) => l.append(m).headOption == l.headOption}

  property("ex10.02 l.append(Empty) == l") = 
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    forAll {(l:LazyList[Int]) => l.append(Empty).toList == l.toList}

  property("ex10.03 l.append(m).size = l.size+m.size ") =
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    forAll{(l:LazyList[Int], m:LazyList[Int]) => l.append(m).toList.size == l.toList.size + m.toList.size}

  property("ex10.04 l.append(m.reverse) == m.append(l).reverse") = 
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    forAll{(l:LazyList[Int], m:LazyList[Int]) =>
      l.append(list2lazyList(m.toList.reverse)).toList == m.append(list2lazyList(l.toList.reverse)).toList.reverse
    }

  property("ex10.05 (l.append(m)).append(n) == l.append(m.append(n))") =
    given Ar@@
  
```



#### Error stacktrace:

```
java.base/java.lang.String.checkBoundsBeginEnd(String.java:4601)
	java.base/java.lang.String.substring(String.java:2704)
	dotty.tools.pc.PcCollector.isGeneratedGiven(PcCollector.scala:133)
	dotty.tools.pc.PcCollector.soughtSymbols(PcCollector.scala:209)
	dotty.tools.pc.PcCollector.resultWithSought(PcCollector.scala:345)
	dotty.tools.pc.PcCollector.result(PcCollector.scala:335)
	dotty.tools.pc.PcDocumentHighlightProvider.highlights(PcDocumentHighlightProvider.scala:33)
	dotty.tools.pc.ScalaPresentationCompiler.documentHighlight$$anonfun$1(ScalaPresentationCompiler.scala:178)
```
#### Short summary: 

java.lang.StringIndexOutOfBoundsException: begin 5514, end 5522, length 5519