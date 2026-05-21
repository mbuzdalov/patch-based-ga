package com.github.mbuzdalov.patchga.distribution

import java.util.random.RandomGenerator

class ConstantDistribution(value: Int) extends IntegerDistribution:
  override def min: Int = value
  override def max: Int = value
  override def sample(rng: RandomGenerator): Int = value

object ConstantDistribution:
  val zero: IntegerDistribution = ConstantDistribution(0)
  val one: IntegerDistribution = ConstantDistribution(1)
