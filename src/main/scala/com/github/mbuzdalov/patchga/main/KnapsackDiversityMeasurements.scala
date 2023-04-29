package com.github.mbuzdalov.patchga.main

import java.util.Random

import com.github.mbuzdalov.patchga.algorithm.*
import com.github.mbuzdalov.patchga.distribution.BinomialDistribution
import com.github.mbuzdalov.patchga.infra.FixedBudgetTerminator
import com.github.mbuzdalov.patchga.problem.Problems
import com.github.mbuzdalov.patchga.util.{Loops, MeanAndStandardDeviation}

object KnapsackDiversityMeasurements:
  def main(args: Array[String]): Unit =
    val n = args(0).toInt
    val budget = args(1).toInt

    val rng = new Random(n * 7632453253523432L)
    def randomArray() = IArray.fill(n)(10000 + rng.nextInt(10000))
    val weights, values = randomArray()
    val capacity = weights.sum / 2

    val optimizer = new MuPlusOneGA(10, 0.9, n => BinomialDistribution(n, 1.4 / n))
    def newKnapsack() = Problems.incrementalKnapsackFB(weights, values, capacity, budget, allowDuplicates = true)

    val patchSize, operationTime = new MeanAndStandardDeviation()

    Loops.loop(0, 110) { t =>
      var nRuns = 0L
      val tBegin = System.nanoTime()
      while System.nanoTime() - tBegin < 1e9 do
        val instance = newKnapsack()
        FixedBudgetTerminator.runUntilBudgetReached(optimizer)(instance)
        nRuns += 1
        if t >= 10 then
          patchSize.record(instance.totalSizeOfPatches.toDouble)

      if t >= 10 then
        val avgOperationTime = (System.nanoTime() - tBegin) * 1e-9 / nRuns / budget
        operationTime.record(avgOperationTime)
        println(s"Cross time: ${operationTime.mean} +- ${operationTime.stdDev}")
        println(s"Cross size: ${patchSize.mean} +- ${patchSize.stdDev}")
    }
