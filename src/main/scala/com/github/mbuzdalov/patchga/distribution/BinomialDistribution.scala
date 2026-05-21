package com.github.mbuzdalov.patchga.distribution

import com.github.mbuzdalov.patchga.util.Loops

import java.util.random.RandomGenerator

object BinomialDistribution:
  def countCoinFlips(n: Int, rng: RandomGenerator): Int =
    if n == 0 then 0 else
      var nn = n
      var result = 0
      while
        nn -= 64
        nn > 0
      do result += java.lang.Long.bitCount(rng.nextLong())
      result + java.lang.Long.bitCount(rng.nextLong() >>> -nn)
  
  private class LogBasedBinomialDistribution(n: Int, p: Double) extends IntegerDistribution:
    private val log1p = math.log1p(-p)
    private def next(from: Long, rng: RandomGenerator): Long = (from + math.log(rng.nextDouble()) / log1p).toLong
    override def min: Int = 0
    override def max: Int = n
    override def sample(rng: RandomGenerator): Int =
      var result = 0
      var ptr = next(0, rng)
      while ptr < n do
        result += 1
        ptr = next(ptr + 1, rng)
      result

  private class NaiveBinomialDistribution(n: Int, p: Double) extends IntegerDistribution:
    override def min: Int = 0
    override def max: Int = n
    override def sample(rng: RandomGenerator): Int = Loops.count(0, n)(_ => rng.nextDouble() < p)
    
  private class OneHalfBinomialDistribution(n: Int) extends IntegerDistribution:
    override def min: Int = 0
    override def max: Int = n
    override def sample(rng: RandomGenerator): Int = countCoinFlips(n, rng)
    
  def apply(n: Int, p: Double): IntegerDistribution =
    if p < 0 || p > 1 then throw IllegalArgumentException(s"p is out of bounds: $p is not in [0;1]")
    else if n < 0 then throw IllegalArgumentException(s"n is negative: $n")
    else if p == 0 || n == 0 then ConstantDistribution.zero
    else if p == 1 then ConstantDistribution(n)
    else if p < 0.05 then LogBasedBinomialDistribution(n, p)
    else if p == 0.5 then OneHalfBinomialDistribution(n)
    else NaiveBinomialDistribution(n, p)
      