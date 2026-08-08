package com.github.mbuzdalov.patchga.distribution

import java.util.random.RandomGenerator
import scala.annotation.targetName

trait IntegerDistribution:
  def min: Int
  def max: Int
  def sample(rng: RandomGenerator): Int

  @targetName("add")
  infix def + (that: Int): IntegerDistribution = IntegerDistribution.addConstant(this, +that)
  
  @targetName("subtract")
  infix def - (that: Int): IntegerDistribution = IntegerDistribution.addConstant(this, -that)

  @targetName("multiply")
  infix def * (that: Int): IntegerDistribution = IntegerDistribution.multiplyByConstant(this, that)

  @targetName("negate")
  def unary_- : IntegerDistribution = IntegerDistribution.multiplyByConstant(this, -1)
  
  def symmetric: IntegerDistribution = IntegerDistribution.symmetric(this)
end IntegerDistribution

object IntegerDistribution:
  extension (constant: Int)
    def + (that: IntegerDistribution): IntegerDistribution = addConstant(that, constant)
    def - (that: IntegerDistribution): IntegerDistribution = addConstant(-that, constant)
    def * (that: IntegerDistribution): IntegerDistribution = multiplyByConstant(that, constant)
  
  private def multiplyByConstant(source: IntegerDistribution, constant: Int): IntegerDistribution =
    val newMin = math.min(source.min * constant, source.max * constant)
    val newMax = math.max(source.min * constant, source.max * constant)
    assert(newMin <= newMax, "Overflow when multiplying a distribution by a constant")
    new IntegerDistribution:
      override def min: Int = newMin
      override def max: Int = newMax
      override def sample(rng: RandomGenerator): Int = source.sample(rng) * constant
  
  private def addConstant(source: IntegerDistribution, constant: Int): IntegerDistribution =
    val newMin = source.min + constant
    val newMax = source.max + constant
    assert(newMin <= newMax, "Overflow when adding a constant to a distribution")
    new IntegerDistribution:
      override def min: Int = newMin
      override def max: Int = newMax
      override def sample(rng: RandomGenerator): Int = source.sample(rng) + constant

  private def symmetric(source: IntegerDistribution): IntegerDistribution =
    new IntegerDistribution:
      override def min: Int = source.min
      override def max: Int = source.max
      override def sample(rng: RandomGenerator): Int =
        val base = source.sample(rng)
        if rng.nextBoolean() then base else source.min + source.max - base
