/* This file is empty on purpose.   It is added, and configured if you
 * wanted to add your own tests during the exam.  It is not graded and
 * should not be submitted.
 */
package adpro

import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, forAllNoShrink}
import adpro.parsing.Sliceable.run
import pigaro.Pigaro


object ExamSpec
  extends org.scalacheck.Properties("exam-2023-autumn"):

  property("A test that always passes (a sanity check)") = 
    forAll { (n: Int) => n == n }

  //property("test fold") = 
    //Streaming.fViaFold(LazyList(1,2,3,4,5) == 2)

  //property("infinie fold") = 
    //Streaming.fViaFold(LazyList(0,1)  == 1 && ) ¨


  property("Question 2") = 
    val parser = adpro.Parsing.longestLine
    parser.run("1,2,3,4,5\n1,2,3") match
      case Right(result) => result == 5
      case Left(_) => false

  property("Question 3 - allLinesTheSame") = 
    val parser = adpro.Parsing.allLinesTheSame
    val testCases = List(
      ("1,2,3\n4,5,6", true),
      ("1,2,3\n4,5", false),
      ("1,2\n3,4\n5,6", true),
      ("1\n2,3\n4,5,6", false)
    )
    testCases.forall { case (input, expected) =>
      parser.run(input) match
        case Right(result) => result == expected
        case Left(_) => false
    }
    
  println(s"Alice's winning probability: ${adpro.Game.aliceFraction}")

  property("Question 6 - Alice winning probability") = 
    val aliceWinProbability = adpro.Game.aliceFraction
    // Since Alice has a uniform strategy and Bob never picks Scissors,
    // Alice should win approximately 1/3 of the time.
    aliceWinProbability > 0.3 && aliceWinProbability < 0.4

end ExamSpec

object NullUpdatesSpecObj
  extends RL.NullUpdatesSpec(update = RL.update, "studentrl") {}
