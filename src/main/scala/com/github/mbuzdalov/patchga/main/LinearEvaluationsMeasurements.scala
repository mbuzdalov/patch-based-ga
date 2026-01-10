package com.github.mbuzdalov.patchga.main

import com.github.mbuzdalov.patchga.algorithm.*
import com.github.mbuzdalov.patchga.distribution.BinomialDistribution
import com.github.mbuzdalov.patchga.infra.FixedTargetTerminator
import com.github.mbuzdalov.patchga.problem.Problems
import com.github.mbuzdalov.patchga.util.MeanAndStandardDeviation

import java.util.concurrent.ScheduledThreadPoolExecutor

object LinearEvaluationsMeasurements:
  private def algorithmList(name: String): Seq[(String, Optimizer.Any)] =
    name match
      case "default" => Seq(
        "RLS" -> OnePlusOneEA.randomizedLocalSearch,
        "(1+1) EA" -> OnePlusOneEA.withStandardBitMutation,
        "(2+1) GA" -> new MuPlusOneGA(2, 1, n => BinomialDistribution(n, math.min(1, 1.2 / n))),
        "(10+1) GA" -> new MuPlusOneGA(10, 1, n => BinomialDistribution(n, math.min(1, 1.4 / n))),
        "NFGA [dist 1.5 other 2.5]" -> new NeverForgettingGA(2.5, 1.5, 0.5, 1.5, 2.5, 2.5),
        "NFGA [all 1.5]" -> new NeverForgettingGA(1.5, 1.5, 0.5, 1.5, 1.5, 1.5),
      )
      case "(2+1)" => for cc <- 0 to 10; c = (1 << cc) * 0.001 yield
        s"(2+1) EA [$c]" -> new MuPlusOneGA(2, 1, n => BinomialDistribution(n, math.min(1, c / n)))
      case "(10+1)" => for cc <- 0 to 10; c = (1 << cc) * 0.001 yield
        s"(10+1) EA [$c]" -> new MuPlusOneGA(10, 1, n => BinomialDistribution(n, math.min(1, c / n)))

  def main(args: Array[String]): Unit =
    val weightRates = args(0).split(',').map(_.toDouble)
    assert(math.abs(weightRates.sum - 1) < 1e-9, "Weight rates should sum up to 1")
    val minLogN = args(1).toInt
    val maxLogN = args(2).toInt
    val nRuns = args(3).toInt
    val nProcessors = args(4).toInt
    val algorithms = algorithmList(args.lift.apply(5).getOrElse("default"))

    val pool = new ScheduledThreadPoolExecutor(if nProcessors <= 0 then Runtime.getRuntime.availableProcessors() else nProcessors)

    for
      nLog <- minLogN to maxLogN
      n = 1 << nLog
      (algoName, algo) <- algorithms
    do
      // generate weights
      val weights0 = weightRates.map(v => (v * n).toInt)
      weights0(weights0.length - 1) += n - weights0.sum
      val weights = IArray.unsafeFromArray(weights0)
      // generate and execute tasks
      val tasks = IndexedSeq.tabulate(nRuns): i =>
        pool.submit: () => 
          val lin = Problems.incrementalLinearFT(weights, 31 * i + 3635263, allowDuplicates = false, disableDiscard = true)
          FixedTargetTerminator.runUntilTargetReached(algo)(lin).nEvaluations
      // compute and print the statistics
      val results = tasks.map(_.get()).sorted
      val evaluationStats = new MeanAndStandardDeviation(nRuns)
      results.foreach(v => evaluationStats.record(v.toDouble))
      val median = results(nRuns / 2)
      println(s"$n = ${weights.mkString("[", ", ", "]")}, $algoName:")
      println(s"  median $median, mean ${evaluationStats.mean}, stddev ${evaluationStats.stdDev}")
      println(results.mkString("  runs: ", ",", ""))

    pool.shutdown()
