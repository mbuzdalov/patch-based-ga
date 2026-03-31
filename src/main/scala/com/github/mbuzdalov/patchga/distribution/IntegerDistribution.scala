package com.github.mbuzdalov.patchga.distribution

import java.util.Random

trait IntegerDistribution:
  def min: Int
  def max: Int
  def sample(rng: Random): Int
  
  def + (that: Int): IntegerDistribution = IntegerDistribution.addConstant(this, +that)
  def - (that: Int): IntegerDistribution = IntegerDistribution.addConstant(this, -that)
  def symmetric(reflection: Int): IntegerDistribution = IntegerDistribution.symmetric(this, reflection)

object IntegerDistribution:
  private def addConstant(source: IntegerDistribution, constant: Int): IntegerDistribution =
    val newMin = source.min + constant
    val newMax = source.max + constant
    assert(newMin <= newMax, "Overflow when adding a constant to a distribution")
    new IntegerDistribution:
      override def min: Int = newMin
      override def max: Int = newMax
      override def sample(rng: Random): Int = source.sample(rng) + constant

  private def symmetric(source: IntegerDistribution, reflection: Int): IntegerDistribution =
    val newMin = math.min(source.min, reflection - source.max)
    val newMax = math.max(source.max, reflection - source.min)
    new IntegerDistribution:
      override def min: Int = newMin
      override def max: Int = newMax
      override def sample(rng: Random): Int =
        val base = source.sample(rng)
        if rng.nextBoolean() then base else reflection - base
