package reductions

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LineOfSightSuite extends FunSuite {
  import LineOfSight._
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

  test("downsweep should correctly handle a 4 element array when the starting angle is zero") {
    val input = Array[Float](0f, 1f, 8f, 9f)
    val output = new Array[Float](4)
    val tree = upsweep(input, 1, input.length, 1)
    downsweep(input, output, 0f, tree)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("downsweepSequential should correctly handle a 5 element array when the starting angle is zero") {
    val input = Array[Float](0f, 7f, 6f, 33f, 48f)
    val output = new Array[Float](5)
    downsweepSequential(input, output, 0f, 1, 5)
    assert(output.toList == List(0f, 7f, 7f, 11f, 12f))
  }

  test("downsweep should correctly handle a 5 element array when the starting angle is zero") {
    val input = Array[Float](0f, 7f, 6f, 33f, 48f)
    val output = new Array[Float](5)
    val tree = upsweep(input, 1, input.length, 1)
    downsweep(input, output, 0f, tree)
    assert(output.toList == List(0f, 7f, 7f, 11f, 12f))
  }

  test("downsweepSequential should correctly handle another 5 element array when the starting angle is zero") {
    val input = Array[Float](0f, 8f, 8f, 33f, 48f)
    val output = new Array[Float](5)
    downsweepSequential(input, output, 0f, 1, 5)
    assert(output.toList == List(0f, 8f, 8f, 11f, 12f))
  }

  test("downsweep should correctly handle another 5 element array when the starting angle is zero") {
    val input = Array[Float](0f, 8f, 8f, 33f, 48f)
    val output = new Array[Float](5)
    val tree = upsweep(input, 1, input.length, 1)
    downsweep(input, output, 0f, tree)
    assert(output.toList == List(0f, 8f, 8f, 11f, 12f))
  }

  test("parLineOfSight should correctly handle another 5 element array when the starting angle is zero and threshold is two") {
    val input = Array[Float](0f, 8f, 8f, 33f, 48f)
    val output = new Array[Float](5)
    parLineOfSight(input, output, 2)
    assert(output.toList == List(0f, 8f, 8f, 11f, 12f))
  }
}
