package com.github.mbuzdalov.patchga

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.github.mbuzdalov.patchga.algorithm.{MuPlusOneGA, OnePlusOneEA, Optimizer}
import com.github.mbuzdalov.patchga.distribution.BinomialDistribution
import com.github.mbuzdalov.patchga.infra.FixedTargetTerminator
import com.github.mbuzdalov.patchga.problem.Problems
import com.github.mbuzdalov.patchga.util.Loops

class IncrementalOneMaxTests extends AnyFlatSpec with Matchers:
  private case class RunResults(avgEvaluations: Double, avgTime: Double)

  private def run(optimizer: Optimizer)
                 (problem: => optimizer.RequiredConfig & FixedTargetTerminator): RunResults =
    val nRuns = 10
    var sumEvaluations = 0.0
    val tBegin = System.nanoTime()
    Loops.repeat(nRuns):
      val instance = problem
      sumEvaluations += FixedTargetTerminator.runUntilTargetReached(optimizer)(instance).nEvaluations
    RunResults(sumEvaluations / nRuns, (System.nanoTime() - tBegin) * 1e-9 / nRuns)

  private def simpleTest(expected: Int => Double)
                        (optimizer: Optimizer)
                        (problem: Int => optimizer.RequiredConfig & FixedTargetTerminator): Unit =
    val n = 1024
    val expectedEvs = expected(n)
    val RunResults(evs, _) = run(optimizer)(problem(n))
    evs shouldBe expectedEvs +- (0.22 * expectedEvs)

  "RLS on OneMax" should "work well with single-slot MST-based population" in
    simpleTest(n => n * math.log(n))
              (OnePlusOneEA.randomizedLocalSearch)
              (n => Problems.incrementalOneMaxFT(n, allowDuplicates = true))

  "(1+1) EA on OneMax" should "work well with single-slot MST-based population" in
    simpleTest(n => math.E * n * math.log(n))
              (OnePlusOneEA.withStandardBitMutation)
              (n => Problems.incrementalOneMaxFT(n, allowDuplicates = true))

  // Constants for (2+1) GA are taken from https://link.springer.com/article/10.1007/s00453-021-00893-w.

  "(2+1) GA on OneMax" should "work well with single-slot MST-based population using c=1" in
    simpleTest(n => 2.224 * n * math.log(n))
              (new MuPlusOneGA(2, 1.0, n => BinomialDistribution(n, 1.0 / n)))
              (n => Problems.incrementalOneMaxFT(n, allowDuplicates = true))

  it should "work well with single-slot MST-based population using c=1.2122" in
    simpleTest(n => 2.18417 * n * math.log(n))
              (new MuPlusOneGA(2, 1.0, n => BinomialDistribution(n, 1.2122 / n)))
              (n => Problems.incrementalOneMaxFT(n, allowDuplicates = true))

  // Constants for (10+1) GA are taken from https://link.springer.com/article/10.1007/s00453-020-00743-1,
  // but they underestimate the runtime for the used problem sizes.

  "(10+1) GA on OneMax" should "work well with single-slot MST-based population" in
    simpleTest(n => 1.75 * n * math.log(n))
              (new MuPlusOneGA(10, 1.0, n => BinomialDistribution(n, 1.43 / n)))
              (n => Problems.incrementalOneMaxFT(n, allowDuplicates = true))
