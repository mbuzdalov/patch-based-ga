package com.github.mbuzdalov.patchga.distribution

import java.util.random.RandomGenerator

class UniformDistribution(val min: Int, val max: Int) extends IntegerDistribution:
  assert(min <= max)
  override def sample(rng: RandomGenerator): Int = rng.nextInt(min, max + 1)
