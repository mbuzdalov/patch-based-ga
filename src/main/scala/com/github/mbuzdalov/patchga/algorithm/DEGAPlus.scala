package com.github.mbuzdalov.patchga.algorithm

import com.github.mbuzdalov.patchga.distribution.{BinomialDistribution, IntegerDistribution}
import com.github.mbuzdalov.patchga.util.Loops

import java.util.random.RandomGenerator
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class DEGAPlus(mutationDistributionSource: Int => IntegerDistribution) extends Optimizer:
  override def optimize(config: Optimizer.Config): Nothing =
    import config.*
    
    val n = maximumPatchSize
    val mutationDistribution = mutationDistributionSource(n)
    
    val population = ArrayBuffer[IndividualHandle]()
    population.addOne(newRandomIndividualH())
    population.addOne(mutateH(population(0), n))
    
    def distanceGreater(base: IndividualHandle, h1: IndividualHandle, h2: IndividualHandle): Boolean =
      var d1, d2 = -1
      collectDistanceToHandles(base): (h, i) =>
        if h == h1 then d1 = i /* no 'else' as sometimes h1 == h2 */
        if h == h2 then d2 = i
      assert(d1 >= 0)
      assert(d2 >= 0)
      d1 > d2

    @tailrec
    def subsample(budget: Int, y: IndividualHandle, rate: IntegerDistribution): Unit =
      if budget > 0 then
        val x = population(0)
        val z = crossoverH(x, y, d => rate.sample(random), _ => 0)
        if compare(fitnessH(z), fitnessH(x)) > 0
        then population(0) = z
        else subsample(budget - 1, y, rate)
    
    val coinFlipper = DEGAPlus.CoinFlipInterceptor(random)
    
    Loops.forever:
      if random.nextBoolean() then
        // mutation
        val idx = random.nextInt(2)
        val parent = population(idx)
        val other = population(1 - idx)
        val offspring = mutateH(parent, mutationDistribution.sample(random))
        val cmp = compare(fitnessH(offspring), fitnessH(parent))
        if cmp > 0 || cmp == 0 && distanceGreater(other, offspring, parent) then
          population(idx) = offspring
      else
        // crossover
        if compare(fitnessH(population(0)), fitnessH(population(1))) > 0 then
          val tmp = population(0)
          population(0) = population(1)
          population(1) = tmp
        val y = crossoverH(population(0), population(1), coinFlipper, _ => 0)
        val xDistance = coinFlipper.lastResult
        assert(coinFlipper.lastResult >= 0)
        if compare(fitnessH(y), fitnessH(population(0))) > 0 then
          val dist = BinomialDistribution(xDistance, 1.0 / xDistance)
          subsample((xDistance * math.log(n) + 0.5).toInt, y, dist)
      end if

object DEGAPlus:
  val withStandardBitMutation: DEGAPlus = DEGAPlus(n => BinomialDistribution(n, 1.0 / n))

  private class CoinFlipInterceptor(rng: RandomGenerator) extends (Int => Int):
    private var _lastResult: Int = 0
    def lastResult: Int = _lastResult
    override def apply(v1: Int): Int =
      _lastResult = BinomialDistribution.countCoinFlips(v1, rng)
      _lastResult
