package reductions

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LineOfSightSuite extends FunSuite {

  import LineOfSight._

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = parLineOfSight(input, output, 1)

  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }


  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }


  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

//  test("tree with single leaf") {
//    val input = Array[Float](0f, 7.0f, 7.0f, 33f, 48f)
//    val output = new Array[Float](5)
//    val tree = upsweep(input, 0, input.length, 8)
//    downsweep(input, output, 0, tree)
//    println(output)
//  }
}

