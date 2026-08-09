package com.github.mbuzdalov.patchga

import java.util.Random
import com.github.mbuzdalov.patchga.distribution.{BinomialDistribution, ConstantDistribution, IntegerDistribution, PowerLawDistribution, UniformDistribution}
import com.github.mbuzdalov.patchga.util.Loops
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DistributionTests extends AnyFlatSpec with Matchers:
  private def testConstant(distribution: IntegerDistribution, expectedValue: Int): Unit =
    distribution.min shouldBe expectedValue
    distribution.max shouldBe expectedValue
    val rng = Random(32423532)
    Loops.repeat(10):
      distribution.sample(rng) shouldBe expectedValue

  private def testOneOverN(distribution: IntegerDistribution): Unit =
    distribution.min shouldBe 0
    val n = distribution.max
    val rng = Random(2354643643L)
    val counts = new Array[Int](2)
    val runs = 100000
    Loops.repeat(runs):
      val v = distribution.sample(rng)
      if v < 2 then counts(v) += 1
    val prob0 = math.exp(n * math.log1p(-1.0 / n))
    val prob1 = math.exp((n - 1) * math.log1p(-1.0 / n))
    counts(0).toDouble / runs shouldBe prob0 +- 0.2 * prob0
    counts(1).toDouble / runs shouldBe prob1 +- 0.2 * prob1

  private def testPowerLaw(n: Int, beta: Double): Unit =
    val probabilities = Array.tabulate(n)(i => math.pow(i + 1, -beta))
    val sum = probabilities.sum
    Loops.foreach(0, n)(i => probabilities(i) /= sum)
    val counts = new Array[Int](n)
    val distribution = PowerLawDistribution(n, beta)
    distribution.min shouldBe 1
    distribution.max shouldBe n
    val rng = Random(33453236432L)
    val size = 10000000
    Loops.repeat(size):
      counts(distribution.sample(rng) - 1) += 1
    Loops.foreach(0, n): i => 
      counts(i).toDouble / size shouldBe probabilities(i) +- math.max(0.1 * probabilities(i), 5e-6)

  private def testSymmetricPowerLaw(n: Int, beta: Double): Unit =
    val probabilities = Array.tabulate(n)(i => math.pow(i + 1, -beta) + math.pow(n - i, -beta))
    val sum = probabilities.sum
    Loops.foreach(0, n)(i => probabilities(i) /= sum)
    val counts = new Array[Int](n)
    val distribution = PowerLawDistribution(n, beta).symmetric
    distribution.min shouldBe 1
    distribution.max shouldBe n
    val rng = Random(33453236432L)
    val size = 10000000
    Loops.repeat(size):
      counts(distribution.sample(rng) - 1) += 1
    Loops.foreach(0, n): i =>
      counts(i).toDouble / size shouldBe probabilities(i) +- math.max(0.15 * probabilities(i), 5e-6)
  
  private def testHalfBinomial(distribution: IntegerDistribution): Unit =
    val n = distribution.max
    val counts = new Array[Int](n + 1)
    val size = 10000000
    val rng = Random(72353444623426L)
    Loops.repeat(size):
      counts(distribution.sample(rng)) += 1
    var choose = 1L
    Loops.foreach(0, n + 1): i =>
      val expected = choose.toDouble / (1L << n)
      val found = counts(i).toDouble / size
      found shouldBe expected +- math.max(0.1 * expected, 5e-6)
      choose *= n - i
      choose /= i + 1
  
  "ConstantDistribution.zero" should "produce zeros" in testConstant(ConstantDistribution.zero, 0)
  "ConstantDistribution.one" should "produce ones" in testConstant(ConstantDistribution.one, 1)
  "ConstantDistribution(5)" should "produce fives" in testConstant(ConstantDistribution(5), 5)

  "BinomialDistribution(10, 0)" should "be constant 0" in testConstant(BinomialDistribution(10, 0), 0)
  "BinomialDistribution(10, 1)" should "be constant 10" in testConstant(BinomialDistribution(10, 1), 10)
  "BinomialDistribution(0, 0.4)" should "be constant 0" in testConstant(BinomialDistribution(0, 0.4), 0)

  "BinomialDistribution(1000, 0.001)" should "behave as expected" in testOneOverN(BinomialDistribution(1000, 0.001))
  "BinomialDistribution(30, 0.5)" should "behave as expected" in testHalfBinomial(BinomialDistribution(30, 0.5))
  "BinomialDistribution(60, 0.5)" should "behave as expected" in testHalfBinomial(BinomialDistribution(60, 0.5))

  "PowerLawDistribution(100, 1.5)" should "behave as expected" in testPowerLaw(100, 1.5)
  "PowerLawDistribution(100, 2.0)" should "behave as expected" in testPowerLaw(100, 2.0)
  "PowerLawDistribution(100, 2.5)" should "behave as expected" in testPowerLaw(100, 2.5)

  "Symmetric PowerLawDistribution(100, 1.5)" should "behave as expected" in testSymmetricPowerLaw(100, 1.5)
  "Symmetric PowerLawDistribution(100, 2.0)" should "behave as expected" in testSymmetricPowerLaw(100, 2.0)
  "Symmetric PowerLawDistribution(100, 2.5)" should "behave as expected" in testSymmetricPowerLaw(100, 2.5)

  "ConstantDistribution" should "have correct arithmetic overrides" in:
    ConstantDistribution(0) + 5 shouldEqual ConstantDistribution(5)
    ConstantDistribution(5) - 6 shouldEqual ConstantDistribution(-1)
    5 - ConstantDistribution(6) shouldEqual ConstantDistribution(-1)
    ConstantDistribution(4) * 7 shouldEqual ConstantDistribution(28)
    4 * ConstantDistribution(7) shouldEqual ConstantDistribution(28)
    -ConstantDistribution(7) shouldEqual ConstantDistribution(-7)
    ConstantDistribution(8).symmetric shouldEqual ConstantDistribution(8)
  
  "UniformDistribution" should "have correct arithmetic overrides" in:
    UniformDistribution(2, 9) - 3 shouldEqual UniformDistribution(-1, 6)
    UniformDistribution(0, 33) + 9 shouldEqual UniformDistribution(9, 42)
    33 + UniformDistribution(0, 9) shouldEqual UniformDistribution(33, 42)
    -UniformDistribution(4, 9) shouldEqual UniformDistribution(-9, -4)
    UniformDistribution(3, 6).symmetric shouldEqual UniformDistribution(3, 6)
