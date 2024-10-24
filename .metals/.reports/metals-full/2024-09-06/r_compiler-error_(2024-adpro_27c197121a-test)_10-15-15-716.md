file://<WORKSPACE>/Main.scala
### dotty.tools.dotc.MissingCoreLibraryException: Could not find package scala from compiler core libraries.
Make sure the compiler core libraries are on the classpath.
   

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 9
uri: file://<WORKSPACE>/Main.scala
text:
```scala
object Ma@@

```



#### Error stacktrace:

```
dotty.tools.dotc.core.Denotations$.select$1(Denotations.scala:1320)
	dotty.tools.dotc.core.Denotations$.recurSimple$1(Denotations.scala:1348)
	dotty.tools.dotc.core.Denotations$.recur$1(Denotations.scala:1350)
	dotty.tools.dotc.core.Denotations$.staticRef(Denotations.scala:1354)
	dotty.tools.dotc.core.Symbols$.requiredPackage(Symbols.scala:939)
	dotty.tools.dotc.core.Definitions.ScalaPackageVal(Definitions.scala:215)
	dotty.tools.dotc.core.Definitions.ScalaPackageClass(Definitions.scala:218)
	dotty.tools.dotc.core.Definitions.AnyClass(Definitions.scala:280)
	dotty.tools.dotc.core.Definitions.syntheticScalaClasses(Definitions.scala:2131)
	dotty.tools.dotc.core.Definitions.syntheticCoreClasses(Definitions.scala:2145)
	dotty.tools.dotc.core.Definitions.init(Definitions.scala:2161)
	dotty.tools.dotc.core.Contexts$ContextBase.initialize(Contexts.scala:899)
	dotty.tools.dotc.core.Contexts$Context.initialize(Contexts.scala:523)
	dotty.tools.dotc.interactive.InteractiveDriver.<init>(InteractiveDriver.scala:41)
	dotty.tools.pc.MetalsDriver.<init>(MetalsDriver.scala:32)
	dotty.tools.pc.ScalaPresentationCompiler.newDriver(ScalaPresentationCompiler.scala:99)
```
#### Short summary: 

dotty.tools.dotc.MissingCoreLibraryException: Could not find package scala from compiler core libraries.
Make sure the compiler core libraries are on the classpath.
   