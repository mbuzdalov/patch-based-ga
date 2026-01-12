package com.github.mbuzdalov.patchga.distribution

import java.util.Random

trait IntegerDistribution:
  def min: Int
  def max: Int
  def sample(rng: Random): Int
  
  def + (that: Int): IntegerDistribution = IntegerDistribution.addConstant(this, +that)
  def - (that: Int): IntegerDistribution = IntegerDistribution.addConstant(this, -that)
      
object IntegerDistribution:
  private def addConstant(source: IntegerDistribution, constant: Int): IntegerDistribution =
    val newMin = source.min + constant
    val newMax = source.max + constant
    assert(newMin <= newMax, "Overflow when adding a constant to a distribution")
    new IntegerDistribution:
      override def min: Int = newMin
      override def max: Int = newMax
      override def sample(rng: Random): Int = source.sample(rng) + constant
