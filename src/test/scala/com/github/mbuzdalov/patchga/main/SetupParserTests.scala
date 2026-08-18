package com.github.mbuzdalov.patchga.main

import com.github.mbuzdalov.patchga.algorithm.{DEGAPlus, MuPlusOneGA, NeverForgettingGA, OnePlusLLGA, OnePlusOneEA, Optimizer}
import com.github.mbuzdalov.patchga.distribution.*
import com.github.mbuzdalov.patchga.infra.FixedTargetTerminator
import com.github.mbuzdalov.patchga.problem.Problems
import com.github.mbuzdalov.patchga.util.Loops
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class SetupParserTests extends AnyFlatSpec with should.Matchers:
  "Parser for integers" should "accept small integers" in:
    SetupParser.evaluateAsInt("0") shouldEqual Right(0)
    SetupParser.evaluateAsInt("1234") shouldEqual Right(1234)
    SetupParser.evaluateAsInt("-4") shouldEqual Right(-4)
    SetupParser.evaluateAsInt("+4") shouldEqual Right(4)
  
  it should "fail on something other than integers" in:
    for q <- Seq("", "000", "AA", "X0", "0.0", "1111111111111111") do
      SetupParser.evaluateAsInt(q) match
        case Left(err) =>
        case Right(value) => fail(s"Expected parsing failure for '$q' but was parsed as $value")
  
  it should "accept correct integer expressions" in:
    for ((str, result) <- Seq(
      "59" -> 59,
      "(1+1)" -> 2,
      "-min(5,9)" -> -5,
      "min(1,2,3,4,5)" -> 1,
      "max(5, -min(-6, 7))" -> 6,
      "4 * (3-2)" -> 4,
      "-8 * (-7)" -> 56,
      "4 div 2" -> 2,
      "5 div 2" -> 2,
      "ceil(5.5)" -> 6,
      "floor(5.9)" -> 5,
      "round(6.45)" -> 6,
      "round(6.55)" -> 7,
    )) do
      SetupParser.evaluateAsInt(str) match
        case Left(err) => fail(s"Expression '$str' should succeed but failed with errors:\n$err")
        case Right(value) => value shouldBe result
  
  it should "fail on incorrect integer expressions" in :
    for (str <- Seq("x", ")", "+", "(2+3(5))", "--5", "(-)8", "4.26", "-8 * -7", "min()", "max()", "2/1")) do
      SetupParser.evaluateAsInt(str) match
        case Right(value) => fail(s"Expected parsing failure for '$str' but was computed as $value")
        case Left(err) =>
  
  "Parser for floats" should "accept small floats" in:
    SetupParser.evaluateAsDouble("0") shouldEqual Right(0.0)
    SetupParser.evaluateAsDouble("0.0") shouldEqual Right(0.0)
    SetupParser.evaluateAsDouble("1.0") shouldEqual Right(1.0)
    SetupParser.evaluateAsDouble("+71.45") shouldEqual Right(71.45)
    SetupParser.evaluateAsDouble("-4.55") shouldEqual Right(-4.55)
    SetupParser.evaluateAsDouble("1e6") shouldEqual Right(1e6)
    SetupParser.evaluateAsDouble("1e-6") shouldEqual Right(1e-6)
    SetupParser.evaluateAsDouble("-44.1211E-02") shouldEqual Right(-0.441211)
    SetupParser.evaluateAsDouble("0.1") shouldEqual Right(0.1)
    SetupParser.evaluateAsDouble("1.01") shouldEqual Right(1.01)
  
  it should "fail on something other than floats" in :
    for q <- Seq("", "AA", "X0", "01.0", "0.0.0", "000") do
      SetupParser.evaluateAsDouble(q) match
        case Left(err) =>
        case Right(value) => fail(s"Expected parsing failure for '$q' but was parsed as $value")
  
  it should "accept correct floating-point expressions" in:
    for ((str, result) <- Seq(
      "59" -> 59,
      "(1+1.4)" -> 2.4,
      "-min(5.4,9)" -> -5.4,
      "max(5, -min(-6.9, 7))" -> 6.9,
      "max(3.0, 3.1, 3.2, 3.3)" -> 3.3,
      "4 * (3-2-0.5)" -> 2.0,
      "-8 * (-7)" -> 56.0,
      "8.0 / 5.4" -> 8.0 / 5.4,
      "log(42)" -> math.log(42),
      "ceil(5.5)" -> 6.0,
      "floor(5.9)" -> 5.0,
      "round(6.45)" -> 6.0,
      "round(6.55)" -> 7.0,
    )) do
      SetupParser.evaluateAsDouble(str) match
        case Left(err) => fail(s"Expression '$str' should succeed but failed with errors:\n$err")
        case Right(value) => value shouldBe result
  
  it should "fail on incorrect floating-point expressions" in :
    for (str <- Seq("x", ")", "+", "(2+3(5))", "--5", "(-)8", "4,26", "4.5.6", "log(2, 3)", "min", "4.0 div 7.1")) do
      SetupParser.evaluateAsDouble(str) match
        case Left(err) =>
        case Right(value) => fail(s"Expected parsing failure for '$str' but was evaluated as $value")
  
  "Parser for int=>int functions" should "work on examples" in:
    for ((str, fn) <- Seq(
      "x => x" -> ((x: Int) => x),
      "x => 59" -> ((x: Int) => 59),
      "x => x + 42" -> ((x: Int) => x + 42),
      "x => x * (x-4)" -> ((x: Int) => x * (x - 4)),
      "x => -x" -> ((x: Int) => -x),
      "x => -(4 + x)" -> ((x: Int) => -(4 + x)),
      "x => min(x, -x)" -> ((x: Int) => math.min(x, -x)),
    )) do
      SetupParser.evaluateAsIntIntFunction(str) match
        case Left(err) => fail(s"Function '$str' should succeed but failed with errors:\n$err")
        case Right(fun) => Loops.foreach(-239, 239): i =>
          val expected = fn(i)
          val found = fun(i)
          if expected != found then fail(s"Function '$str': on x = $i, expected $expected found $found")
  
  "Parser for int=>double functions" should "work on examples" in :
    for ((str, fn) <- Seq(
      "x => x" -> ((x: Int) => x.toDouble),
      "x => 59.7" -> ((x: Int) => 59.7),
      "x => x + 3.14147" -> ((x: Int) => x + 3.14147),
      "x => x * (x-4.3)" -> ((x: Int) => x * (x - 4.3)),
      "x => -x" -> ((x: Int) => -x.toDouble),
      "x => -(3.5 + x)" -> ((x: Int) => -(3.5 + x)),
      "x => min(x, -x)" -> ((x: Int) => math.min(x, -x).toDouble),
      "x => log(5.6 * x)" -> ((x: Int) => math.log(5.6 * x)),
    )) do
      SetupParser.evaluateAsIntDoubleFunction(str) match
        case Left(err) => fail(s"Function '$str' should succeed but failed with errors:\n$err")
        case Right(fun) => Loops.foreach(-239, 239): i =>
          val expected = fn(i)
          val found = fun(i)
          if expected != found && !(expected.isNaN && found.isNaN) then fail(s"Function '$str': on x = $i, expected $expected found $found")

  private def validateDistributions(expected: IntegerDistribution, found: IntegerDistribution, name: String, n: Int): Unit =
    if found.min != expected.min then fail(s"Distribution $name, size $n: minimum expected ${expected.min} found ${found.min}")
    if found.max != expected.max then fail(s"Distribution $name, size $n: maximum expected ${expected.max} found ${found.max}")
  
  "Parser for integer distributions" should "parse plain literals well" in:
    for ((str, dist) <- Seq(
      "x => 42" -> ((x: Int) => ConstantDistribution(42)),
      "x => 4 * x div 3" -> ((x: Int) => ConstantDistribution(4 * x / 3)),
      "x => 4 * (x div 3)" -> ((x: Int) => ConstantDistribution(4 * (x / 3))),
      "n => uniform(0, 5)" -> ((x: Int) => UniformDistribution(0, 5)),
      "d => uniform(d div 2, d)" -> ((x: Int) => UniformDistribution(x / 2, x)),
      "qq => powerLaw(qq, 1.5)" -> ((x: Int) => PowerLawDistribution(x, 1.5)),
      "x => powerLaw(4 * x, 2.5)" -> ((x: Int) => PowerLawDistribution(4 * x, 2.5)),
      "x => powerLaw(round(3 + log(x + 1)), 2)" -> ((x: Int) => PowerLawDistribution(math.round(3 + math.log(x + 1)).toInt, 2)),
      "x => binomial(x, 0.5)" -> ((x: Int) => BinomialDistribution(x, 0.5)),
    )) do SetupParser.evaluateAsIntDistributionFunction(str) match
      case Left(err) => fail(s"Distribution '$str' should succeed but failed with errors:\n$err")
      case Right(fun) => Loops.foreach(1, 239): i =>
        val expected = dist(i)
        val found = fun(i)
        validateDistributions(expected, found, str, i)

  it should "parse more complicated expressions" in:
    for ((str, dist) <- Seq(
      "p => 1 + binomial(max(0, p - 2), 0.5)" -> ((x: Int) => 1 + BinomialDistribution(math.max(0, x - 2), 0.5)),
      "dist => 1 + powerLaw(max(1, dist - 2), 1.5)" -> ((x: Int) => 1 + PowerLawDistribution(math.max(1, x - 2), 1.5)),
      "x => 2 * binomial(x div 2, 1 / x) - uniform(x div 2, x)" -> ((x: Int) => 2 * BinomialDistribution(x / 2, 1.0 / x) - UniformDistribution(x / 2, x)),
      "x => symmetric(4 * uniform(4 * x, 5 * x))" -> ((x: Int) => (4 * UniformDistribution(4 * x, 5 * x)).symmetric),
      "1 + (variable => 4 * variable)" -> ((x: Int) => ConstantDistribution(1 + 4 * x)),
      "(a => uniform(4, a + 4)) + (b => uniform(6, 6 + 3 * b))" -> ((x: Int) => UniformDistribution(4, x + 4) + UniformDistribution(6, 6 + 3 * x)),
    )) do SetupParser.evaluateAsIntDistributionFunction(str) match
      case Left(err) => fail(s"Distribution '$str' should succeed but failed with errors:\n$err")
      case Right(fun) => Loops.foreach(1, 239): i =>
        val expected = dist(i)
        val found = fun(i)
        validateDistributions(expected, found, str, i)

  private def algoSanityCheck(algo: Optimizer): Unit =
    FixedTargetTerminator.runUntilTargetReached(algo, Problems.incrementalOneMaxFT(10, false, false), 10).nEvaluations should be <= 1000L
  
  "Parser for algorithms" should "parse DEGA" in:
    val config =
      """DEGA:
        |  - mutationDistribution: n => powerLaw(n, 1.5)
        |""".stripMargin
    SetupParser.evaluateAsOptimizer(config) match
      case Left(err) => fail(s"DEGA's config should succeed but failed with errors:\n$err")
      case Right(algo) =>
        algo.isInstanceOf[DEGAPlus] shouldBe true
        algoSanityCheck(algo)
  
  it should "produce error for DEGA when the parameter name is mistyped" in:
    val config =
      """DEGA:
        |  - mutationDistributon: n => powerLaw(n, 1.5)
        |""".stripMargin
    SetupParser.evaluateAsOptimizer(config) match
      case Left(err) =>
      case Right(algo) => fail(s"This DEGA's config should fail because of the typo")
  
  it should "produce error for DEGA when the parameter type is incorrect" in:
    val config =
      """DEGA:
        |  - mutationDistribution: 1.5
        |""".stripMargin
    SetupParser.evaluateAsOptimizer(config) match
      case Left(err) =>
      case Right(algo) => fail(s"This DEGA's config should fail because of the wrong parameter type")
  
  it should "produce error for DEGA when there is an extra parameter" in:
    val config =
      """DEGA:
        |  - mutationDistribution: n => 1
        |  - unnecessaryParameter: 42
        |""".stripMargin
    SetupParser.evaluateAsOptimizer(config) match
      case Left(err) =>
      case Right(algo) => fail(s"This DEGA's config should fail because of the extra parameter")
  
  it should "parse MuPlusOneGA" in:
    val config =
      """MuPlusOneGA:
        |  - populationSize: 10
        |  - crossoverProbability: 0.456
        |  - mutationOnlyDistribution: n => powerLaw(n, 1.5)
        |  - mutationAfterCrossoverDistribution: n => powerLaw(n + 1, 1.5) - 1
        |""".stripMargin
    SetupParser.evaluateAsOptimizer(config) match
      case Left(err) => fail(s"MuPlusOneGA's config should succeed but failed with errors:\n$err")
      case Right(algo) =>
        algo.isInstanceOf[MuPlusOneGA] shouldBe true
        algoSanityCheck(algo)
  
  it should "parse MuPlusOneGA in a different order" in:
    val config =
      """MuPlusOneGA:
        |  - mutationOnlyDistribution: n => powerLaw(n, 1.5)
        |  - crossoverProbability: 0.456
        |  - mutationAfterCrossoverDistribution: n => powerLaw(n + 1, 1.5) - 1
        |  - populationSize: 10
        |""".stripMargin
    SetupParser.evaluateAsOptimizer(config) match
      case Left(err) => fail(s"MuPlusOneGA's config should succeed but failed with errors:\n$err")
      case Right(algo) =>
        algo.isInstanceOf[MuPlusOneGA] shouldBe true
        algoSanityCheck(algo)
  
  it should "parse NFGA with all distributions set to beta=1.5" in:
    val config =
      """NFGA:
        |  - mutationParentSelectionDistribution: q => powerLaw(q, 1.5)
        |  - firstParentSelectionDistribution:    q => powerLaw(q, 1.5)
        |  - secondParentSelectionDistribution:   q => powerLaw(q, 1.5)
        |  - mutationDistanceDistribution:        n => powerLaw(n, 1.5)
        |  - crossoverParentMinimumDistance:      n => powerLaw(n, 1.5)
        |  - crossoverDistanceDistribution:       d => powerLaw(d - 1, 1.5)
        |  - crossoverParentMaximumDistance:      n => n
        |  - crossoverProbability:                0.5
        |""".stripMargin
    SetupParser.evaluateAsOptimizer(config) match
      case Left(err) => fail(s"NFGA's config should succeed but failed with errors:\n$err")
      case Right(algo) =>
        algo.isInstanceOf[NeverForgettingGA] shouldBe true
        algoSanityCheck(algo)
  
  it should "parse NFGA with incorrect parameters, which then fails" in :
    val config =
      """NFGA:
        |  - mutationParentSelectionDistribution: q => powerLaw(q, 1.5)
        |  - firstParentSelectionDistribution:    q => powerLaw(q, 1.5)
        |  - secondParentSelectionDistribution:   q => powerLaw(q, 1.5)
        |  - mutationDistanceDistribution:        n => powerLaw(n, 1.5)
        |  - crossoverParentMinimumDistance:      n => powerLaw(n, 1.5)
        |  - crossoverDistanceDistribution:       d => powerLaw(d, 1.5)
        |  - crossoverParentMaximumDistance:      n => n
        |  - crossoverProbability:                0.5
        |""".stripMargin
    SetupParser.evaluateAsOptimizer(config) match
      case Left(err) => fail(s"NFGA's config should succeed but failed with errors:\n$err")
      case Right(algo) =>
        algo.isInstanceOf[NeverForgettingGA] shouldBe true
        an[AssertionError] shouldBe thrownBy:
          algoSanityCheck(algo)
  
  it should "parse OnePlusLLGA" in :
    val config =
      """OnePlusLLGA:
        |  - mutationDistribution: n => powerLaw(n, 2.5)
        |  - crossoverDistribution: n => powerLaw(n, 2.5)
        |""".stripMargin
    SetupParser.evaluateAsOptimizer(config) match
      case Left(err) => fail(s"OnePlusLLGA's config should succeed but failed with errors:\n$err")
      case Right(algo) =>
        algo.isInstanceOf[OnePlusLLGA] shouldBe true
        algoSanityCheck(algo)

  // todo: support 'n => max(1, binomial(n, 1/n))', the classical shift distribution
  it should "parse OnePlusOneEA" in :
    val config =
      """OnePlusOneEA:
        |  - mutationDistribution: n => binomial(n - 1, 1 / n) + 1
        |""".stripMargin
    SetupParser.evaluateAsOptimizer(config) match
      case Left(err) => fail(s"OnePlusOneEA's config should succeed but failed with errors:\n$err")
      case Right(algo) =>
        algo.isInstanceOf[OnePlusOneEA] shouldBe true
        algoSanityCheck(algo)
  
