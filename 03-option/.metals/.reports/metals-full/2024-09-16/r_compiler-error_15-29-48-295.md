file://<WORKSPACE>/Variance.java
### java.util.NoSuchElementException: next on empty iterator

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.3.3/scala3-library_3-3.3.3.jar [exists ], <HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.12/scala-library-2.13.12.jar [exists ]
Options:



action parameters:
uri: file://<WORKSPACE>/Variance.java
text:
```scala
package adpro.variance;

class A {};
class B extends A {};
class C extends A {};

class Variance {

  // Java arrays are covariant so if B < A then B[] < A[]
  static void problem () {
 
    // Create an array of B's
    B[] b = { new B() };

    // Create an array of A' and initialize it with b.  This is fine, as every
    // B is also an A (and arrays are covariant in Java.)
    A[] a = b;

    // Assign a[0] to c 
    a[0] = new C();
 
    // This compiles because C's are A's so they can be used in place of A's.
    // But we have a problem, because in reality a is initialized as an array
    // of B's, and anybody who uses the array b (that is aliased) expects B's
    // to be there.
  
    // For instance imagine what would happen if we now call b[0].funB where
    // funB is a method only available in B and not in any of the other
    // classes?

    // Covariants and assignments do not mix well ...
    // Scala arrays are invariant (so the above would not compile)

  }

};

```



#### Error stacktrace:

```
scala.collection.Iterator$$anon$19.next(Iterator.scala:973)
	scala.collection.Iterator$$anon$19.next(Iterator.scala:971)
	scala.collection.mutable.MutationTracker$CheckedIterator.next(MutationTracker.scala:76)
	scala.collection.IterableOps.head(Iterable.scala:222)
	scala.collection.IterableOps.head$(Iterable.scala:222)
	scala.collection.AbstractIterable.head(Iterable.scala:933)
	dotty.tools.dotc.interactive.InteractiveDriver.run(InteractiveDriver.scala:168)
	scala.meta.internal.pc.MetalsDriver.run(MetalsDriver.scala:45)
	scala.meta.internal.pc.WithCompilationUnit.<init>(WithCompilationUnit.scala:28)
	scala.meta.internal.pc.SimpleCollector.<init>(PcCollector.scala:373)
	scala.meta.internal.pc.PcSemanticTokensProvider$Collector$.<init>(PcSemanticTokensProvider.scala:61)
	scala.meta.internal.pc.PcSemanticTokensProvider.Collector$lzyINIT1(PcSemanticTokensProvider.scala:61)
	scala.meta.internal.pc.PcSemanticTokensProvider.Collector(PcSemanticTokensProvider.scala:61)
	scala.meta.internal.pc.PcSemanticTokensProvider.provide(PcSemanticTokensProvider.scala:90)
	scala.meta.internal.pc.ScalaPresentationCompiler.semanticTokens$$anonfun$1(ScalaPresentationCompiler.scala:117)
```
#### Short summary: 

java.util.NoSuchElementException: next on empty iterator