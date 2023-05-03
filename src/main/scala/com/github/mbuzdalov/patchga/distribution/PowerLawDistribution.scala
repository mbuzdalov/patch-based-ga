package com.github.mbuzdalov.patchga.distribution

import java.util.Random
import java.util.concurrent.ConcurrentHashMap

import com.github.mbuzdalov.patchga.util.GrowableDoubleArray

class PowerLawDistribution private(n: Int, val beta: Double, prefixSums: GrowableDoubleArray) extends IntegerDistribution:
  override def min: Int = 1
  override def max: Int = n
  override def sample(rng: Random): Int = PowerLawDistribution.sample(n, beta, rng, prefixSums)

object PowerLawDistribution:
  private val distributionInstances = new ConcurrentHashMap[Double, GrowableDoubleArray]()

  private def sample(n: Int, beta: Double, rng: Random, array: GrowableDoubleArray): Int = if n == 1 then 1 else
    // array(x) contains the prefix sum for (1, 2, ..., x + 2)^{-beta}
    val maxIdx = n - 2
    if array.length <= maxIdx then
      array.synchronized {
        if array.length == 0 then
          array.add(1 + math.pow(2, -beta))
        while array.length <= maxIdx do
          array.add(array.last + math.pow(array.length + 2, -beta))
      }
    val query = rng.nextDouble() * array(maxIdx)
    if query < 1 then 1 else
      var index = 0
      while query >= array(index) do index += 1
      index + 2

  private def getArrayForBeta(beta: Double) = distributionInstances.computeIfAbsent(beta, _ => new GrowableDoubleArray)

  def sample(n: Int, beta: Double, rng: Random): Int = sample(n, beta, rng, getArrayForBeta(beta))
  def apply(n: Int, beta: Double): IntegerDistribution = new PowerLawDistribution(n, beta, getArrayForBeta(beta))
