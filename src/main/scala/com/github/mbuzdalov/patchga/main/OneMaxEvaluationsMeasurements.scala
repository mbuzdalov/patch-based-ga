package com.github.mbuzdalov.patchga.main

import java.util.concurrent.ScheduledThreadPoolExecutor

import com.github.mbuzdalov.patchga.algorithm.*
import com.github.mbuzdalov.patchga.distribution.BinomialDistribution
import com.github.mbuzdalov.patchga.infra.FixedTargetTerminator
import com.github.mbuzdalov.patchga.population.SingleSlotMSTPopulation
import com.github.mbuzdalov.patchga.problem.Problems
import com.github.mbuzdalov.patchga.util.MeanAndStandardDeviation

object OneMaxEvaluationsMeasurements:
  private type OptimizerType = Optimizer {
    type RequiredConfig >: Problems.OneMaxFT & SingleSlotMSTPopulation
  }

  private def algorithmList(name: String): Seq[(String, OptimizerType)] =
    name match
      case "default" => Seq(
        "RLS" -> OnePlusOneEA.randomizedLocalSearch,
        "(1+1) EA" -> OnePlusOneEA.withStandardBitMutation,
        "(2+1) GA" -> new MuPlusOneGA(2, 1, n => BinomialDistribution(n, math.min(1, 0.002 / n))),
        "(10+1) GA" -> new MuPlusOneGA(10, 1, n => BinomialDistribution(n, math.min(1, 0.004 / n))),
        "NFGA" -> new NeverForgettingGA(2.5, 1.5, 0.5, 1.5, 2.5, 2.5),
      )
      case "(2+1)" => for cc <- 0 to 10; c = (1 << cc) * 0.001 yield
        s"(2+1) EA [$c]" -> new MuPlusOneGA(2, 1, n => BinomialDistribution(n, math.min(1, c / n)))
      case "(10+1)" => for cc <- 0 to 10; c = (1 << cc) * 0.001 yield
        s"(10+1) EA [$c]" -> new MuPlusOneGA(10, 1, n => BinomialDistribution(n, math.min(1, c / n)))

  def main(args: Array[String]): Unit =
    val minLogN = args(0).toInt
    val maxLogN = args(1).toInt
    val nRuns = args(2).toInt
    val nProcessors = args(3).toInt
    val algorithms: Seq[(String, OptimizerType)] = algorithmList(args.lift.apply(4).getOrElse("default"))

    val pool = new ScheduledThreadPoolExecutor(if nProcessors <= 0 then Runtime.getRuntime.availableProcessors() else nProcessors)

    for
      nLog <- minLogN to maxLogN
      n = 1 << nLog
      (algoName, algo) <- algorithms
    do
      val tasks = IndexedSeq.fill(nRuns):
        pool.submit: () => 
          val om = Problems.incrementalOneMaxFT(n, allowDuplicates = false)
          FixedTargetTerminator.runUntilTargetReached(algo)(om).nEvaluations
      val results = tasks.map(_.get()).sorted
      val evaluationStats = new MeanAndStandardDeviation(nRuns)
      results.foreach(v => evaluationStats.record(v.toDouble))
      val median = results(nRuns / 2)
      println(s"$n, $algoName:")
      println(s"  median $median, mean ${evaluationStats.mean}, stddev ${evaluationStats.stdDev}")
      println(results.mkString("  runs: ", ",", ""))

    pool.shutdown()
