package com.github.mbuzdalov.patchga.distribution
import java.util.Random

class BinomialDistribution private (n: Int, p: Double) extends IntegerDistribution:
  private val log1p = math.log1p(-p)
  private def next(from: Long, rng: Random): Long = (from + math.log(rng.nextDouble()) / log1p).toLong
  override def min: Int = 0
  override def max: Int = n
  override def sample(rng: Random): Int =
    var result = 0
    var ptr = next(0, rng)
    while ptr < n do
      result += 1
      ptr = next(ptr + 1, rng)
    result  

object BinomialDistribution:
  def apply(n: Int, p: Double): IntegerDistribution =
    if p < 0 || p > 1 then
      throw new IllegalArgumentException(s"p is out of bounds: $p is not in [0;1]")
    else if n < 0 then
      throw new IllegalArgumentException(s"n is negative: $n")
    else if p == 0 || n == 0 then 
      ConstantDistribution.zero
    else if p == 1 then
      ConstantDistribution(n)
    else
      new BinomialDistribution(n, p)
      