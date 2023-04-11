package com.github.mbuzdalov.patchga

import com.github.mbuzdalov.patchga.algorithm.{MuPlusOneGA, OnePlusOneEA, Optimizer, RandomizedLocalSearch}
import com.github.mbuzdalov.patchga.config.{FitnessComparator, FitnessType, IndividualType, SimpleFitnessFunction}
import com.github.mbuzdalov.patchga.distribution.BinomialDistribution
import com.github.mbuzdalov.patchga.infra.{FixedTargetTerminator, ThreadLocalRandomProvider}
import com.github.mbuzdalov.patchga.population.{NaiveScratchPopulation, SingleSlotMSTPopulation}
import com.github.mbuzdalov.patchga.problem.OneMax
import com.github.mbuzdalov.patchga.representation.UnconstrainedBitString
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IncrementalOneMaxTests extends AnyFlatSpec with Matchers:
  private class IncrementalOneMax(size: Int)
    extends UnconstrainedBitString(size), OneMax, OneMax.Incremental, SingleSlotMSTPopulation,
      ThreadLocalRandomProvider, FixedTargetTerminator.Incremental:
    override def targetFitness: Fitness = size

  private case class RunResults(avgEvaluations: Double, avgTime: Double)

  private def run(optimizer: Optimizer)
                 (problem: => optimizer.RequiredConfig & FixedTargetTerminator): RunResults =
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

  private def simpleTest(expected: Int => Double)
                        (optimizer: Optimizer)
                        (problem: Int => optimizer.RequiredConfig & FixedTargetTerminator): Unit =
    val n = 1024
    val expectedEvs = expected(n)
    val RunResults(evs, _) = run(optimizer)(problem(n))
    evs shouldBe expectedEvs +- (0.2 * expectedEvs)

  "RLS on OneMax" should "work well with single-slot MST-based population" in
    simpleTest(n => n * math.log(n))
              (RandomizedLocalSearch)
              (n => new IncrementalOneMax(n))

  "(1+1) EA on OneMax" should "work well with single-slot MST-based population" in
    simpleTest(n => math.E * n * math.log(n))
              (OnePlusOneEA.withStandardBitMutation)
              (n => new IncrementalOneMax(n))

  // constants for (2+1) GA are taken from https://link.springer.com/article/10.1007/s00453-021-00893-w

  "(2+1) GA on OneMax" should "work well with single-slot MST-based population using c=1" in
    simpleTest(n => 2.224 * n * math.log(n))
              (new MuPlusOneGA(2, 1.0, n => BinomialDistribution(n, 1.0 / n)))
              (n => new IncrementalOneMax(n))

  it should "work well with single-slot MST-based population using c=1.2122" in
    simpleTest(n => 2.18417 * n * math.log(n))
              (new MuPlusOneGA(2, 1.0, n => BinomialDistribution(n, 1.2122 / n)))
              (n => new IncrementalOneMax(n))

  // constants for (10+1) GA are taken from https://link.springer.com/article/10.1007/s00453-020-00743-1

  "(10+1) GA on OneMax" should "work well with single-slot MST-based population" in
    simpleTest(n => 1.66 * n * math.log(n))
              (new MuPlusOneGA(10, 1.0, n => BinomialDistribution(n, 1.43 / n)))
              (n => new IncrementalOneMax(n))
