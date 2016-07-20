package reductions

import scala.annotation._
import org.scalameter._
import common._

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
    @tailrec
    def loop(idx: Int, depth: Int): Boolean =
      if (idx >= chars.length) depth == 0
      else if (chars(idx) == '(') loop(idx + 1, depth + 1)
      else if (chars(idx) == ')') if (depth > 0) loop(idx + 1, depth - 1) else false
      else loop(idx + 1, depth)
    loop(0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    @tailrec
    def traverse(idx: Int, until: Int, mn: Int, cur: Int): (Int, Int) = {
      if (idx >= until) (mn, cur)
      else if (chars(idx) == '(') traverse(idx + 1, until, mn, cur + 1)
      else if (chars(idx) == ')') traverse(idx + 1, until, math.min(mn, cur - 1), cur - 1)
      else traverse(idx + 1, until, mn, cur)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      val mid = (until + from) / 2

      if (until - from <= threshold || mid <= from) {
        traverse(from, until, 0, 0)
      }
      else {
        val ((lmin, llast), (rmin, rlast)) = parallel(reduce(from, mid), reduce(mid, until))
        (math.min(lmin, llast + rmin), llast + rlast)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!
}
