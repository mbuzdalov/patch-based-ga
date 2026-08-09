package com.github.mbuzdalov.patchga.distribution

import java.util.random.RandomGenerator
import scala.annotation.targetName

trait IntegerDistribution:
  /**
   * Returns the minimum value ever returned from `sample`.
   *
   * This value should be computed in such a way that this value is returned with probability > 0 in the ideal world.
   * For example, `BinomialDistribution(100, 0.999999)` will return 0, even though the probability of this to happen
   * is 10^{-600} and in double-precision this number is zero. However, `BinomialDistribution(100, 1)` will return
   * 100, because returning anything other than 100 happens with probability 0.
   *
   * @return the minimum value sampled.
   */
  def min: Int

  /**
   * Returns the maximum value ever returned from `sample`.
   *
   * This value should be computed in such a way that this value is returned with probability > 0 in the ideal world.
   * For example, `BinomialDistribution(100, 0.000001)` will return 100, even though the probability of this to happen
   * is 10^{-600} and in double-precision this number is zero. However, `BinomialDistribution(100, 0)` will return
   * 0, because returning anything other than 0 happens with probability 0.
   *
   * @return the maximum value sampled.
   */
  def max: Int
  
  /**
   * Samples an integer using the provided random number generator and returns it.
   * This value will always be at least `min` and at most `max`.
   *
   * @param rng the random number generator to use.
   * @return the sampled number.
   */
  def sample(rng: RandomGenerator): Int

  @targetName("add")
  infix def + (that: Int): IntegerDistribution = IntegerDistribution.addConstant(this, +that)
  
  @targetName("subtract")
  infix def - (that: Int): IntegerDistribution = IntegerDistribution.addConstant(this, -that)

  @targetName("multiply")
  infix def * (that: Int): IntegerDistribution = IntegerDistribution.multiplyByConstant(this, that)

  @targetName("negate")
  def unary_- : IntegerDistribution = IntegerDistribution.multiplyByConstant(this, -1)
  
  @targetName("add")
  infix def + (that: IntegerDistribution): IntegerDistribution = IntegerDistribution.addDistributions(this, that)
  
  @targetName("subtract")
  infix def - (that: IntegerDistribution): IntegerDistribution = IntegerDistribution.subtractDistributions(this, that)
  
  @targetName("multiply")
  infix def * (that: IntegerDistribution): IntegerDistribution = IntegerDistribution.multiplyDistributions(this, that)
  
  /**
   * Returns a distribution which is a symmetric version of this distribution,
   * centered around the [`min`;`max`] range.
   *
   * This means that, in the returned distribution, the probability of sampling a number `X`` is the same as
   * the probability of sampling a number `max`+`min`-`X`. This probability is an average of the probabilities
   * to sample these two numbers in the original distribution.
   *
   * Note that the distributions generally bear no identification for how they were created.
   * For instance, `BinomialDistribution(n, p).symmetric` does not automatically have `min = 0` and `max = n`,
   * because if `p = 0` or `p = 1`, the range will collapse,
   * and this method will no longer produce what one may imply.
   *
   * @return the symmetric version of this distribution.
   */
  def symmetric: IntegerDistribution = IntegerDistribution.symmetric(this)
end IntegerDistribution

object IntegerDistribution:
  extension (constant: Int)
    def + (that: IntegerDistribution): IntegerDistribution = that + constant
    def - (that: IntegerDistribution): IntegerDistribution = -that + constant
    def * (that: IntegerDistribution): IntegerDistribution = that * constant
  
  private def multiplyByConstant(source: IntegerDistribution, constant: Int): IntegerDistribution =
    if constant == 0 then ConstantDistribution.zero else
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

  private def addDistributions(left: IntegerDistribution, right: IntegerDistribution): IntegerDistribution =
    if right.min == right.max then left + right.min
    else if left.min == left.max then left.min + right
    else
      val newMin = left.min + right.min
      val newMax = left.max + right.max
      require(newMin <= newMax, "Overflow when adding two distributions")
      new IntegerDistribution:
        override def min: Int = newMin
        override def max: Int = newMax
        override def sample(rng: RandomGenerator): Int = left.sample(rng) + right.sample(rng)
        
  private def subtractDistributions(left: IntegerDistribution, right: IntegerDistribution): IntegerDistribution =
    if right.min == right.max then left - right.min
    else if left.min == left.max then left.min - right
    else
      val newMin = left.min - right.max
      val newMax = left.max - right.min
      require(newMin <= newMax, "Overflow when subtracting two distributions")
      new IntegerDistribution:
        override def min: Int = newMin
        override def max: Int = newMax
        override def sample(rng: RandomGenerator): Int = left.sample(rng) - right.sample(rng)
  
  private def multiplyDistributions(left: IntegerDistribution, right: IntegerDistribution): IntegerDistribution =
    if right.min == right.max then left * right.min
    else if left.min == left.max then left.min * right
    else
      val minMin = left.min * right.min
      val minMax = left.min * right.max
      val maxMin = left.max * right.min
      val maxMax = left.max * right.max
      val newMin = math.min(math.min(minMin, minMax), math.min(maxMin, maxMax))
      val newMax = math.max(math.max(minMin, minMax), math.max(maxMin, maxMax))
      // There should be much more checks with these...
      require(newMin <= newMax, "Overflow when multiplying two distributions")
      new IntegerDistribution:
        override def min: Int = newMin
        override def max: Int = newMax
        override def sample(rng: RandomGenerator): Int = left.sample(rng) * right.sample(rng)
