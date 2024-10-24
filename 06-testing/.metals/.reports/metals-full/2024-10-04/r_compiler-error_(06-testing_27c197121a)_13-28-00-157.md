file://<WORKSPACE>/Exercises.scala
### java.lang.AssertionError: assertion failed: MethodType(List(f, p, s1, pp1), List(TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),trait Function1), TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),trait Function1), TypeRef(ThisType(TypeRef(NoPrefix,module class scalacheck)),class Shrink), TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),trait Function1)), TypeRef(ThisType(TypeRef(NoPrefix,module class scalacheck)),class Prop))

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 3568
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
  property("Ex02: name") = 
    failList.headOption;
    true
  
  // Exercise 3

  def failListN(n:Int) : LazyList[Int] =
    if n <= 0 then Empty
    else Cons(() => ???, () => failListN(n-1))

  property("Ex03: something") = 
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
      forAll{(a: Int)} => 
        forAll{(n:Int) => s.drop(n).take(a).toList== s.take(n@@).toList  
        }
      }
    }

  
  // Exercise 7

  // Exercise 8

  // Exercise 9
 
  // Exercise 10


```



#### Error stacktrace:

```
scala.runtime.Scala3RunTime$.assertFailed(Scala3RunTime.scala:8)
	dotty.tools.dotc.core.TypeErasure.dotty$tools$dotc$core$TypeErasure$$sigName(TypeErasure.scala:950)
	dotty.tools.dotc.core.TypeErasure.dotty$tools$dotc$core$TypeErasure$$sigName(TypeErasure.scala:951)
	dotty.tools.dotc.core.TypeErasure$.sigName(TypeErasure.scala:234)
	dotty.tools.dotc.core.Signature.$anonfun$2(Signature.scala:111)
	scala.collection.immutable.List.map(List.scala:246)
	dotty.tools.dotc.core.Signature.prependTermParams(Signature.scala:111)
	dotty.tools.dotc.core.Types$MethodOrPoly.computeSignature$2(Types.scala:3890)
	dotty.tools.dotc.core.Types$MethodOrPoly.signature(Types.scala:3907)
	dotty.tools.dotc.core.Denotations$SingleDenotation.signature(Denotations.scala:623)
	dotty.tools.dotc.core.Denotations$SingleDenotation.signature(Denotations.scala:613)
	dotty.tools.dotc.core.Symbols$Symbol.signature(Symbols.scala:208)
	dotty.tools.pc.SemanticdbSymbols$.addOverloadIdx$1(SemanticdbSymbols.scala:154)
	dotty.tools.pc.SemanticdbSymbols$.addDescriptor$1(SemanticdbSymbols.scala:175)
	dotty.tools.pc.SemanticdbSymbols$.addSymName(SemanticdbSymbols.scala:179)
	dotty.tools.pc.SemanticdbSymbols$.addOwner$1(SemanticdbSymbols.scala:134)
	dotty.tools.pc.SemanticdbSymbols$.addSymName(SemanticdbSymbols.scala:178)
	dotty.tools.pc.SemanticdbSymbols$.addOwner$1(SemanticdbSymbols.scala:134)
	dotty.tools.pc.SemanticdbSymbols$.addSymName(SemanticdbSymbols.scala:178)
	dotty.tools.pc.SemanticdbSymbols$.symbolName(SemanticdbSymbols.scala:117)
	dotty.tools.pc.completions.Completions.visit$3(Completions.scala:588)
	dotty.tools.pc.completions.Completions.filterInteresting$$anonfun$1(Completions.scala:614)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.pc.completions.Completions.filterInteresting(Completions.scala:614)
	dotty.tools.pc.completions.Completions.completions(Completions.scala:133)
	dotty.tools.pc.completions.CompletionProvider.completions(CompletionProvider.scala:90)
	dotty.tools.pc.ScalaPresentationCompiler.complete$$anonfun$1(ScalaPresentationCompiler.scala:146)
```
#### Short summary: 

java.lang.AssertionError: assertion failed: MethodType(List(f, p, s1, pp1), List(TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),trait Function1), TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),trait Function1), TypeRef(ThisType(TypeRef(NoPrefix,module class scalacheck)),class Shrink), TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),trait Function1)), TypeRef(ThisType(TypeRef(NoPrefix,module class scalacheck)),class Prop))