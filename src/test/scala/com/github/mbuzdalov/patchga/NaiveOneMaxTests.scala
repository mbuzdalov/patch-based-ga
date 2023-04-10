package com.github.mbuzdalov.patchga

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.github.mbuzdalov.patchga.algorithm.{OnePlusOneEA, Optimizer, RandomizedLocalSearch}
import com.github.mbuzdalov.patchga.config.{FitnessComparator, FitnessType, IndividualType, SimpleFitnessFunction}
import com.github.mbuzdalov.patchga.infra.{FixedTargetTerminator, ThreadLocalRandomProvider}
import com.github.mbuzdalov.patchga.population.NaiveScratchPopulation
import com.github.mbuzdalov.patchga.representation.UnconstrainedBitString
import com.github.mbuzdalov.patchga.problem.OneMax

class NaiveOneMaxTests extends AnyFlatSpec with Matchers:
  private class NaiveOneMax(size: Int) extends UnconstrainedBitString(size), OneMax, NaiveScratchPopulation, ThreadLocalRandomProvider, FixedTargetTerminator:
    override def targetFitness: Fitness = size

  private case class RunResults(avgEvaluations: Double, avgTime: Double)

  private def run(optimizer: Optimizer)(problem: => optimizer.RequiredConfig & FixedTargetTerminator): RunResults =
    val nRuns = 10
    var sumEvaluations = 0.0
    val tBegin = System.nanoTime()
    var t = 0
    while t < nRuns do
      val instance = problem
      try
        optimizer.optimize(instance)
      catch
        case e: instance.TargetReached => sumEvaluations += e.nEvaluations
      t += 1
    RunResults(sumEvaluations / nRuns, (System.nanoTime() - tBegin) * 1e-9 / nRuns)

  private def simpleTest(expected: Int => Double)(optimizer: Optimizer)(problem: Int => optimizer.RequiredConfig & FixedTargetTerminator): Unit =
    val n = 512
    val expectedEvs = expected(n)
    val RunResults(evs, _) = run(optimizer)(problem(n))
    evs shouldBe expectedEvs +- (0.3 * expectedEvs)

  "RLS on OneMax" should "work well with naive population" in
    simpleTest(n => n * math.log(n))(RandomizedLocalSearch)(n => new NaiveOneMax(n))

  "(1+1) EA on OneMax" should "work well with naive population" in
    simpleTest(n => math.E * n * math.log(n))(OnePlusOneEA.withStandardBitMutation)(n => new NaiveOneMax(n))
