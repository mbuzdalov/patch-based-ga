package com.github.mbuzdalov.patchga.main

import com.github.mbuzdalov.patchga.distribution.IntegerDistribution
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
      "x" -> ((x: Int) => x),
      "59" -> ((x: Int) => 59),
      "x + 42" -> ((x: Int) => x + 42),
      "x * (x-4)" -> ((x: Int) => x * (x - 4)),
      "-x" -> ((x: Int) => -x),
      "-(4 + x)" -> ((x: Int) => -(4 + x)),
      "min(x, -x)" -> ((x: Int) => math.min(x, -x)),
      //"binLog(5 * x)" -> ((x: Int) => math.ceil(math.log(5 * x) / math.log(2)).toInt),
    )) do
      SetupParser.evaluateAsIntIntFunction(str, "x") match
        case Left(err) => fail(s"Function '$str' should succeed but failed with errors:\n$err")
        case Right(fun) => Loops.foreach(-239, 239): i =>
          val expected = fn(i)
          val found = fun(i)
          if expected != found then fail(s"Function '$str': on x = $i, expected $expected found $found")
  
  "Parser for int=>double functions" should "work on examples" in :
    for ((str, fn) <- Seq(
      "x" -> ((x: Int) => x.toDouble),
      "59.7" -> ((x: Int) => 59.7),
      "x + 3.14147" -> ((x: Int) => x + 3.14147),
      "x * (x-4.3)" -> ((x: Int) => x * (x - 4.3)),
      "-x" -> ((x: Int) => -x.toDouble),
      "-(3.5 + x)" -> ((x: Int) => -(3.5 + x)),
      "min(x, -x)" -> ((x: Int) => math.min(x, -x).toDouble),
      //"binLog(5 * x)" -> ((x: Int) => math.log(5 * x) / math.log(2)),
      "log(5.6 * x)" -> ((x: Int) => math.log(5.6 * x)),
    )) do
      SetupParser.evaluateAsIntDoubleFunction(str, "x") match
        case Left(err) => fail(s"Function '$str' should succeed but failed with errors:\n$err")
        case Right(fun) => Loops.foreach(-239, 239): i =>
          val expected = fn(i)
          val found = fun(i)
          if expected != found && !(expected.isNaN && found.isNaN) then fail(s"Function '$str': on x = $i, expected $expected found $found")

end SetupParserTests

/*

  "Evaluator of integer expressions" should "work on examples" in:
  
  

  "Parser for integer distributions" should "succeed on examples" in:
    for (str <- Seq(
      "n=>[1..n]",
      "n => 1",
      "d => [1..d-2] + 1",
      "n => uniform(0, n / 2)",
      "d => binomial(d, 1 / d)",
      "n => powerLaw(n, 1.5)",
      "n => powerLaw(n, 1.5) + binomial(n, 2 / n) * 2",
      "d => 1 + [1..d-2]",
      "d => 1 + [1..d-4/2]",
      "d => d/2 + [1..d/2]",
    )) do
      parse(str, distribution(using _)) match
        case f: Failure => fail(s"Distribution '$str' should parse, but the following happened:\n${f.trace().longAggregateMsg}")
        case s: Success[Int => IntegerDistribution] =>
 */