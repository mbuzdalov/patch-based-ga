package com.github.mbuzdalov.patchga.distribution

import java.util.random.RandomGenerator
import scala.annotation.targetName

case class UniformDistribution(min: Int, max: Int) extends IntegerDistribution:
  require(min <= max, s"Uniform distribution cannot be created when min <= max, but $min > $max")
  override def sample(rng: RandomGenerator): Int = rng.nextInt(min, max + 1)
  
  @targetName("add")      override infix def +(that: Int): IntegerDistribution = UniformDistribution(min + that, max + that)
  @targetName("subtract") override infix def -(that: Int): IntegerDistribution = UniformDistribution(min - that, max - that)
  @targetName("negate")   override def unary_- : IntegerDistribution = UniformDistribution(-max, -min)
  override def symmetric: IntegerDistribution = this
