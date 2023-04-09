package com.github.mbuzdalov.patchga

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.github.mbuzdalov.patchga.algorithm.RandomizedLocalSearch
import com.github.mbuzdalov.patchga.config.{FitnessComparator, FitnessType, IndividualType, SimpleFitnessFunction}
import com.github.mbuzdalov.patchga.infra.{FixedTargetTerminator, ThreadLocalRandomProvider}
import com.github.mbuzdalov.patchga.population.NaiveScratchPopulation
import com.github.mbuzdalov.patchga.representation.UnconstrainedBitString
import com.github.mbuzdalov.patchga.problem.OneMax

class RLSonOneMax extends AnyFlatSpec with Matchers:
  private class NaiveOneMax(size: Int) extends UnconstrainedBitString(size), OneMax, NaiveScratchPopulation, ThreadLocalRandomProvider, FixedTargetTerminator:
    override def targetFitness: Fitness = size

  private case class RunResults(avgEvaluations: Double, avgTime: Double)

  private def runRLS(problem: => RandomizedLocalSearch.RequiredConfig & FixedTargetTerminator): RunResults =
    val nRuns = 10
    var sumEvaluations = 0.0
    val tBegin = System.nanoTime()
    var t = 0
    while t < nRuns do
      val instance = problem
      try
        RandomizedLocalSearch.optimize(instance)
      catch
        case e: instance.TargetReached => sumEvaluations += e.nEvaluations
      t += 1
    RunResults(sumEvaluations / nRuns, (System.nanoTime() - tBegin) * 1e-9 / nRuns)

  "RLS on OneMax" should "work well with naive population" in {
    val n = 512
    val expectedEvs = n * math.log(n)
    val RunResults(evs, _) = runRLS(new NaiveOneMax(n))
    evs shouldBe expectedEvs +- (0.3 * expectedEvs)
  }
