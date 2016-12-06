package reductions

import common._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var count = 0
    for (char: Char <- chars) {
      if (char.equals('(')) count += 1
      else if (char.equals(')')) count -= 1
      if (count < 0) return false
    }
    if (count != 0) false
    else true
    // TODO GC detected.
    // def helper(chars: Array[Char], count: Int): Boolean = {
    //   if (chars.isEmpty) {
    //     if (count != 0) false
    //     else true
    //   } else {
    //     if (count < 0) false
    //     else {
    //       if (chars.head.equals('(')) helper(chars.tail, count + 1)
    //       else if (chars.head.equals(')')) helper(chars.tail, count - 1)
    //       else helper(chars.tail, count)
    //     }
    //   }
    // }
    // helper(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if (idx >= until) {
        (arg1, arg2)
      } else {
        if (chars(idx).equals('(')) traverse(idx + 1, until, arg1 + 1, arg2)
        else if (chars(idx).equals(')')) {
          if (arg1 > 0) traverse(idx + 1, until, arg1 - 1, arg2)
          else traverse(idx + 1, until, arg1, arg2 + 1)
        }
        else traverse(idx + 1, until, arg1, arg2)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val middle = (from + until) / 2
        val (t1, t2) = parallel(reduce(from, middle), reduce(middle, until))
        if (t2._2 >= t1._1) (t2._1, t2._2 - t1._1 + t1._2)
        else (t1._1 - t2._2 + t2._1, t1._2)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!
}
