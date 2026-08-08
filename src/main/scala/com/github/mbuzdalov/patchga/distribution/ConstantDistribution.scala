package com.github.mbuzdalov.patchga.distribution

import java.util.random.RandomGenerator
import scala.annotation.targetName

case class ConstantDistribution(value: Int) extends IntegerDistribution:
  override def min: Int = value
  override def max: Int = value
  override def sample(rng: RandomGenerator): Int = value
  
  @targetName("add")      override infix def +(that: Int): IntegerDistribution = ConstantDistribution(value + that)
  @targetName("subtract") override infix def -(that: Int): IntegerDistribution = ConstantDistribution(value - that)
  @targetName("multiply") override infix def *(that: Int): IntegerDistribution = ConstantDistribution(value * that)
  @targetName("negate")   override def unary_- : IntegerDistribution = ConstantDistribution(-value)
  override def symmetric: IntegerDistribution = this

object ConstantDistribution:
  val zero: IntegerDistribution = ConstantDistribution(0)
  val one: IntegerDistribution = ConstantDistribution(1)
