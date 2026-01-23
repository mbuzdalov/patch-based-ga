package com.github.mbuzdalov.patchga.distribution

import java.util.Random

class UniformDistribution(val min: Int, val max: Int) extends IntegerDistribution:
  assert(min <= max)
  override def sample(rng: Random): Int = rng.nextInt(min, max + 1)
