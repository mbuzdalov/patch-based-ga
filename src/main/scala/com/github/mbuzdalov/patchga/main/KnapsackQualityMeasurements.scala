package com.github.mbuzdalov.patchga.main

import java.util.Random
import java.util.concurrent.ScheduledThreadPoolExecutor

import com.github.mbuzdalov.patchga.algorithm.*
import com.github.mbuzdalov.patchga.distribution.{BinomialDistribution, PowerLawDistribution}
import com.github.mbuzdalov.patchga.infra.FixedBudgetTerminator
import com.github.mbuzdalov.patchga.population.SingleSlotMSTPopulation
import com.github.mbuzdalov.patchga.problem.Problems
import com.github.mbuzdalov.patchga.util.MeanAndStandardDeviation

object KnapsackQualityMeasurements:
  private type OptimizerType = Optimizer {
    type RequiredConfig >: Problems.KnapsackFB & SingleSlotMSTPopulation
  }

  private def algorithmList(name: String): Seq[(String, OptimizerType)] =
    name match
      case "default" => Seq(
        "RLS" -> OnePlusOneEA.randomizedLocalSearch,
        "(1+1) EA" -> OnePlusOneEA.withStandardBitMutation,
        "(1+1) hEA" -> new OnePlusOneEA(n => PowerLawDistribution(n, 1.5)),
        "(2+1) GA" -> new MuPlusOneGA(2, 1, n => BinomialDistribution(n, math.min(1, 4.0 / n))),
        "(2+1) hGA" -> new MuPlusOneGA(2, 1, n => PowerLawDistribution(n, 1.5)),
        "(10+1) GA" -> new MuPlusOneGA(10, 1, n => BinomialDistribution(n, math.min(1, 4.0 / n))),
        "(10+1) hGA" -> new MuPlusOneGA(10, 1, n => PowerLawDistribution(n, 1.5)),
        "NFGA" -> new NeverForgettingGA(2.5, 1.5, 0.5, 1.5, 2.5, 2.5),
      )
      case "(2+1)" => for cc <- 8 to 15; c = (1 << cc) * 0.001 yield
        s"(2+1) EA [$c]" -> new MuPlusOneGA(2, 1, n => BinomialDistribution(n, math.min(1, c / n)))
      case "(10+1)" => for cc <- 8 to 15; c = (1 << cc) * 0.001 yield
        s"(10+1) EA [$c]" -> new MuPlusOneGA(10, 1, n => BinomialDistribution(n, math.min(1, c / n)))

  def main(args: Array[String]): Unit =
    val n = args(0).toInt
    val budget = args(1).toInt
    val nRuns = args(2).toInt
    val nProcessors = args(3).toInt
    val algorithms: Seq[(String, OptimizerType)] = algorithmList(args.lift.apply(4).getOrElse("default"))

    val pool = new ScheduledThreadPoolExecutor(if nProcessors <= 0 then Runtime.getRuntime.availableProcessors() else nProcessors)

    for (algoName, algo) <- algorithms do
      val tasks = IndexedSeq.tabulate(nRuns): i => 
        pool.submit: () =>
          val rng = new Random(134235253 * (i + 132))
          val weights, values = IArray.fill(n)(10000 + rng.nextInt(10000))
          val capacity = weights.sum / 2
          val problem = Problems.incrementalKnapsackFB(weights, values, capacity, budget, allowDuplicates = true, disableDiscard = false)
          val rawFitness = FixedBudgetTerminator.runUntilBudgetReached(algo)(problem).fitness
          if rawFitness.isValid then rawFitness.sumValues else 0
      val results = tasks.map(_.get()).sorted
      val evaluationStats = new MeanAndStandardDeviation(nRuns)
      results.foreach(v => evaluationStats.record(v.toDouble))
      val median = results(nRuns / 2)
      println(s"$n, $algoName:")
      println(s"  median $median, mean ${evaluationStats.mean}, stddev ${evaluationStats.stdDev}")
      println(results.mkString("  runs: ", ",", ""))

    pool.shutdown()
