package com.github.mbuzdalov.patchga.main

import com.github.mbuzdalov.patchga.algorithm.*
import com.github.mbuzdalov.patchga.distribution.{BinomialDistribution, PowerLawDistribution}
import com.github.mbuzdalov.patchga.infra.FixedTargetTerminator
import com.github.mbuzdalov.patchga.problem.Problems
import com.github.mbuzdalov.patchga.util.MeanAndStandardDeviation

import java.util.concurrent.ScheduledThreadPoolExecutor

object CliffEvaluationsMeasurements:
  private val algorithmList: Seq[(String, Optimizer.Any)] = Seq(
    "(1+1) EA" -> OnePlusOneEA.withStandardBitMutation,
    "(1+1) hEA" -> new OnePlusOneEA(n => PowerLawDistribution(n, 1.5)),
    "(2+1) GA" -> new MuPlusOneGA(2, 1, n => BinomialDistribution(n, math.min(1, 1.2 / n))),
    "(10+1) GA" -> new MuPlusOneGA(10, 1, n => BinomialDistribution(n, math.min(1, 1.4 / n))),
    "NFGA [dist 1.5 other 2.5]" -> new NeverForgettingGA(2.5, 1.5, 0.5, 1.5, 2.5, 2.5),
    "NFGA [all 1.5]" -> new NeverForgettingGA(1.5, 1.5, 0.5, 1.5, 1.5, 1.5),
  )

  def main(args: Array[String]): Unit =
    val cliffConst = args(0).toInt
    val minLogN = args(1).toInt
    val maxLogN = args(2).toInt
    val nRuns = args(3).toInt
    val nProcessors = args(4).toInt

    val pool = new ScheduledThreadPoolExecutor(if nProcessors <= 0 then Runtime.getRuntime.availableProcessors() else nProcessors)

    for
      nLog <- minLogN to maxLogN
      n = 1 << nLog
      (algoName, algo) <- algorithmList
    do
      val t0 = System.currentTimeMillis()
      val tasks = IndexedSeq.fill(nRuns):
        pool.submit: () => 
          val prob = Problems.incrementalCliffFT(n, cliffConst, allowDuplicates = false, disableDiscard = true)
          FixedTargetTerminator.runUntilTargetReached(algo)(prob).nEvaluations
      val results = tasks.map(_.get()).sorted
      val time = System.currentTimeMillis() - t0
      val median = results(nRuns / 2)
      println(s"$n, $algoName: took $time ms")
      val evaluationStats = new MeanAndStandardDeviation(nRuns)
      results.foreach(v => evaluationStats.record(v.toDouble))
      println(s"  median $median, mean ${evaluationStats.mean}, stddev ${evaluationStats.stdDev}")
      println(results.mkString("  runs: ", ",", ""))

    pool.shutdown()
