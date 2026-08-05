package com.github.mbuzdalov.patchga

import com.github.mbuzdalov.patchga.algorithm.Optimizer
import com.github.mbuzdalov.patchga.infra.FixedTargetTerminator
import com.github.mbuzdalov.patchga.problem.Problems
import com.github.mbuzdalov.patchga.util.Loops
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

trait IntProblemTestsBase extends AnyFlatSpec with should.Matchers:
  case class RunResults(avgEvaluations: Double, avgTime: Double)
  
  def run(optimizer: Optimizer, target: Int)
                 (problem: => Problems.IntProblem): RunResults =
    val nRuns = 10
    var sumEvaluations = 0.0
    val tBegin = System.nanoTime()
    Loops.repeat(nRuns):
      val instance = problem
      sumEvaluations += FixedTargetTerminator.runUntilTargetReached(optimizer, instance, target).nEvaluations
    RunResults(sumEvaluations / nRuns, (System.nanoTime() - tBegin) * 1e-9 / nRuns)
  
  def simpleTest(ns: Int*)
                (expected: Int => Double)
                (optimizer: Optimizer)
                (problem: Int => Problems.IntProblem): Unit =
    for n <- ns do
      val expectedEvs = expected(n)
      val RunResults(evs, _) = run(optimizer, n)(problem(n))
      evs shouldBe expectedEvs +- (0.3 * expectedEvs)
