package com.github.mbuzdalov.patchga.algorithm

import com.github.mbuzdalov.patchga.config.*
import com.github.mbuzdalov.patchga.distribution.{BinomialDistribution, IntegerDistribution}
import com.github.mbuzdalov.patchga.util.Loops

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object DEGAPlus extends Optimizer:
  type RequiredConfig = FitnessType & Population & MaximumPatchSize & FitnessComparator & RandomProvider
  
  override def optimize(config: FitnessType & Population & MaximumPatchSize & FitnessComparator & RandomProvider): Nothing =
    import config.*
    
    val n = maximumPatchSize
    val mutationDistribution = BinomialDistribution(n, 1.0 / n)
    
    val population = ArrayBuffer[IndividualHandle]()
    population.addOne(newRandomIndividualH())
    population.addOne(mutateH(population(0), n))
    
    def distanceGreater(base: IndividualHandle, h1: IndividualHandle, h2: IndividualHandle): Boolean =
      var d1, d2 = -1
      collectDistanceToHandles(base, (h, i) =>
        if h == h1 then d1 = i /* no 'else' as sometimes h1 == h2 */
        if h == h2 then d2 = i)
      assert(d1 >= 0)
      assert(d2 >= 0)
      d1 > d2

    @tailrec
    def subsample(budget: Int, y: IndividualHandle, rate: IntegerDistribution, replacementIdx: Int): Unit =
      if budget > 0 then
        val x = population(replacementIdx)
        val z = crossoverH(x, y, d => rate.sample(random), _ => 0)
        if compare(fitnessH(z), fitnessH(x)) > 0 then
          population(replacementIdx) = z
        else
          subsample(budget - 1, y, rate, replacementIdx)
    
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
        val smaller = if compare(fitnessH(population(0)), fitnessH(population(1))) < 0 then 0 else 1
        var xDistance = -1
        var pDistance = -1
        val y = crossoverH(population(smaller), population(1 - smaller), d =>
          pDistance = d
          xDistance = BinomialDistribution(d, 0.5).sample(random)
          xDistance
        , _ => 0)
        assert(xDistance >= 0)
        if compare(fitnessH(y), fitnessH(population(smaller))) > 0 then
          val dist = BinomialDistribution(xDistance, 1.0 / xDistance)
          subsample((xDistance * math.log(n) + 0.5).toInt, y, dist, smaller)
      end if
