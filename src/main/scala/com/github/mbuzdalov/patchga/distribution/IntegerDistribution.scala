package com.github.mbuzdalov.patchga.distribution

import java.util.Random

trait IntegerDistribution:
  def min: Int
  def max: Int
  def sample(rng: Random): Int
  