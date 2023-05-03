package com.github.mbuzdalov.patchga

import java.util.Random

import com.github.mbuzdalov.patchga.distribution.{BinomialDistribution, ConstantDistribution, IntegerDistribution, PowerLawDistribution}
import com.github.mbuzdalov.patchga.util.Loops
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DistributionTests extends AnyFlatSpec with Matchers:
  private def testConstant(distribution: IntegerDistribution, expectedValue: Int): Unit =
    distribution.min shouldBe expectedValue
    distribution.max shouldBe expectedValue
    val rng = new Random(32423532)
    for _ <- 0 until 10 do distribution.sample(rng) shouldBe expectedValue

  private def testOneOverN(distribution: IntegerDistribution): Unit =
    distribution.min shouldBe 0
    val n = distribution.max
    val rng = new Random(2354643643L)
    val counts = new Array[Int](2)
    val runs = 100000
    for _ <- 0 until runs do
      val v = distribution.sample(rng)
      if v < 2 then counts(v) += 1
    val prob0 = math.exp(n * math.log1p(-1.0 / n))
    val prob1 = math.exp((n - 1) * math.log1p(-1.0 / n))
    counts(0).toDouble / runs shouldBe prob0 +- 0.2 * prob0
    counts(1).toDouble / runs shouldBe prob1 +- 0.2 * prob1

  private def testPowerLaw(n: Int, beta: Double): Unit =
    val probabilities = Array.tabulate(n)(i => math.pow(i + 1, -beta))
    val sum = probabilities.sum
    Loops.loop(0, n)(i => probabilities(i) /= sum)
    val counts = new Array[Int](n)
    val distribution = PowerLawDistribution(n, beta)
    distribution.min shouldBe 1
    distribution.max shouldBe n
    val rng = new Random(33453236432L)
    val size = 10000000
    Loops.loop(0, size)(_ => counts(distribution.sample(rng) - 1) += 1)
    Loops.loop(0, n)(i => counts(i).toDouble / size shouldBe probabilities(i) +- math.max(0.1 * probabilities(i), 5e-6))

  "ConstantDistribution.zero" should "produce zeros" in testConstant(ConstantDistribution.zero, 0)
  "ConstantDistribution.one" should "produce ones" in testConstant(ConstantDistribution.one, 1)
  "ConstantDistribution(5)" should "produce fives" in testConstant(ConstantDistribution(5), 5)

  "BinomialDistribution(10, 0)" should "be constant 0" in testConstant(BinomialDistribution(10, 0), 0)
  "BinomialDistribution(10, 1)" should "be constant 10" in testConstant(BinomialDistribution(10, 1), 10)
  "BinomialDistribution(0, 0.4)" should "be constant 0" in testConstant(BinomialDistribution(0, 0.4), 0)

  "BinomialDistribution(1000, 0.001)" should "behave as expected" in testOneOverN(BinomialDistribution(1000, 0.001))

  "PowerLawDistribution(100, 1.5)" should "behave as expected" in testPowerLaw(100, 1.5)
  "PowerLawDistribution(100, 2.0)" should "behave as expected" in testPowerLaw(100, 2.0)
  "PowerLawDistribution(100, 2.5)" should "behave as expected" in testPowerLaw(100, 2.5)
