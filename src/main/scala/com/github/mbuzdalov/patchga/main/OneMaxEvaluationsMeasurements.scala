package com.github.mbuzdalov.patchga.main

import java.util.concurrent.ScheduledThreadPoolExecutor

import com.github.mbuzdalov.patchga.algorithm.*
import com.github.mbuzdalov.patchga.distribution.{BinomialDistribution, ConstantDistribution}
import com.github.mbuzdalov.patchga.infra.FixedTargetTerminator
import com.github.mbuzdalov.patchga.population.SingleSlotMSTPopulation
import com.github.mbuzdalov.patchga.problem.Problems
import com.github.mbuzdalov.patchga.util.{Loops, MeanAndStandardDeviation}

object OneMaxEvaluationsMeasurements:
  private type OptimizerType = Optimizer {
    type RequiredConfig >: Problems.OneMaxFT & SingleSlotMSTPopulation
  }

  private def algorithmList(name: String): Seq[(String, OptimizerType)] =
    name match
      case "default" => Seq(
        "RLS" -> RandomizedLocalSearch,
        "(1+1) EA" -> OnePlusOneEA.withStandardBitMutation,
        "(2+1) GA" -> new MuPlusOneGA(2, 1, n => BinomialDistribution(n, math.min(1, 0.05 / n))),
        "(10+1) GA" -> new MuPlusOneGA(10, 1, n => BinomialDistribution(n, math.min(1, 0.09 / n))),
        "NFGA" -> new NeverForgettingGA(2.5, 1.5, 0.5, 1.5, 2.5, 1.5),
      )
      case "(2+1)" => for cc <- 1 to 10; c = (1 << cc) * 0.001 yield
        s"(2+1) EA [$c]" -> new MuPlusOneGA(2, 1, n => BinomialDistribution(n, math.min(1, c / n)))
      case "(10+1)" => for cc <- 1 to 10; c = (1 << cc) * 0.001 yield
        s"(10+1) EA [$c]" -> new MuPlusOneGA(10, 1, n => BinomialDistribution(n, math.min(1, c / n)))

  def main(args: Array[String]): Unit =
    val minLogN = args(0).toInt
    val maxLogN = args(1).toInt
    val nRuns = args(2).toInt
    val algorithms: Seq[(String, OptimizerType)] = algorithmList(args.lift.apply(3).getOrElse("default"))

    val pool = new ScheduledThreadPoolExecutor(Runtime.getRuntime.availableProcessors())

    for
      nLog <- minLogN to maxLogN
      n = 1 << nLog
      (algoName, algo) <- algorithms
    do
      val tasks = IndexedSeq.fill(nRuns)(pool.submit(() => FixedTargetTerminator.runUntilTargetReached(algo)(Problems.incrementalOneMaxFT(n, allowDuplicates = false)).nEvaluations))
      val results = tasks.map(_.get()).sorted
      val evaluationStats = new MeanAndStandardDeviation(nRuns)
      results.foreach(v => evaluationStats.record(v.toDouble))
      val median = results(nRuns / 2)
      println(s"$n, $algoName:")
      println(s"  median $median, mean ${evaluationStats.mean}, stddev ${evaluationStats.stdDev}")
      println(results.mkString("  runs: ", ",", ""))

    pool.shutdown()
