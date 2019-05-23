package reductions

import common.parallel
import org.scalameter._

import scala.annotation.{switch, tailrec}

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

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
    0 == chars.foldLeft(0) {
      case (count, _) if count < 0 => count
      case (count, '(') => count + 1
      case (count, ')') => count - 1
      case (count, _) => count
    }
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    @tailrec def traverse(idx: Int, until: Int, open: Int, close: Int): (Int, Int) = {
      if (idx == until)
        (open, close)
      else (chars(idx): @switch) match {
        case '(' => traverse(idx + 1, until, open + 1, close)
        case ')' if open > 0 => traverse(idx + 1, until, open - 1, close) // balance out an existing '('
        case ')' => traverse(idx + 1, until, open, close + 1)
        case _ => traverse(idx + 1, until, open, close)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold)
        traverse(from, until, 0, 0)
      else {
        val mid = from + (until - from) / 2
        val ((open1, close1), (open2, close2)) = parallel(reduce(from, mid), reduce(mid, until))
        if (open1 >= close2) {
          (open1 - close2 + open2, close1)
        } else {
          (open2, close1 + close2 - open1)
        }
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
