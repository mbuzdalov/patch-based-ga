package com.github.mbuzdalov.patchga.algorithm

import scala.annotation.tailrec
import com.github.mbuzdalov.patchga.config.*
import com.github.mbuzdalov.patchga.distribution.{BinomialDistribution, ConstantDistribution, IntegerDistribution}

class OnePlusOneEA(distributionSource: Int => IntegerDistribution) extends Optimizer:
  type RequiredConfig = FitnessType & Population & MaximumPatchSize & FitnessComparator & RandomProvider

  def optimize(config: RequiredConfig): Nothing =
    import config.*
    val distribution = distributionSource(maximumPatchSize)

    @tailrec
    def go(curr: IndividualHandle): Nothing =
      val next = mutateH(curr, distribution.sample(random))
      if compare(fitnessH(curr), fitnessH(next)) <= 0 then
        discardH(curr)
        go(next)
      else
        discardH(next)
        go(curr)

    go(newRandomIndividualH())

object OnePlusOneEA:
  val randomizedLocalSearch: OnePlusOneEA = OnePlusOneEA(n => ConstantDistribution(1))
  val withStandardBitMutation: OnePlusOneEA = OnePlusOneEA(n => BinomialDistribution(n, 1.0 / n))
