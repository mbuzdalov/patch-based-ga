package com.github.mbuzdalov.patchga.main

import java.util.Random

import com.github.mbuzdalov.patchga.algorithm.*
import com.github.mbuzdalov.patchga.config.FitnessType
import com.github.mbuzdalov.patchga.distribution.BinomialDistribution
import com.github.mbuzdalov.patchga.infra.{FixedBudgetTerminator, ThreadLocalRandomProvider}
import com.github.mbuzdalov.patchga.population.SingleSlotMSTPopulation
import com.github.mbuzdalov.patchga.problem.Knapsack
import com.github.mbuzdalov.patchga.representation.UnconstrainedBitString
import com.github.mbuzdalov.patchga.util.Loops

object KnapsackDiversityMeasurements:
  private class IncrementalKnapsack(weights: IArray[Int], values: IArray[Int], capacity: Int, budget: Int)
    extends UnconstrainedBitString(weights.length), Knapsack(weights, values, capacity), Knapsack.Incremental,
      SingleSlotMSTPopulation, ThreadLocalRandomProvider, FixedBudgetTerminator(budget), FixedBudgetTerminator.Incremental


  def main(args: Array[String]): Unit =
    val n = args(0).toInt
    val budget = args(1).toInt

    val rng = new Random(n * 7632453253523432L)
    def randomArray() = IArray.fill(n)(10000 + rng.nextInt(10000))
    val weights, values = randomArray()
    val capacity = weights.sum / 2

    val optimizer = new MuPlusOneGA(10, 0.9, n => BinomialDistribution(n, 1.4 / n))
    def newKnapsack() = new IncrementalKnapsack(weights, values, capacity, budget)

    var crossAveragePatchSum = 0.0
    var crossAveragePatchSumSq = 0.0
    var crossAverageCnt = 0

    var crossAverageTimeSum = 0.0
    var crossAverageTimeSumSq = 0.0
    while System.in.available() == 0 do
      var sumPatchSizes: Double = 0.0
      var nRuns = 0L
      val tBegin = System.nanoTime()
      while System.nanoTime() - tBegin < 1e9 do
        val instance = newKnapsack()
        try
          optimizer.optimize(instance)
        catch
          case e: instance.BudgetReached =>
            nRuns += 1
            if e.fitness.isValid then sumPatchSizes += instance.totalSizeOfPatches

      val avgOperationTime = (System.nanoTime() - tBegin) * 1e-9 / nRuns / budget
      val avgPatchSize = sumPatchSizes / nRuns
      crossAverageCnt += 1
      crossAveragePatchSum += avgPatchSize
      crossAveragePatchSumSq += avgPatchSize * avgPatchSize
      crossAverageTimeSum += avgOperationTime
      crossAverageTimeSumSq += avgOperationTime * avgOperationTime
      val crossAveragePatch = crossAveragePatchSum / crossAverageCnt
      val crossAverageTime = crossAverageTimeSum / crossAverageCnt
      println(s"averageOperation: $avgOperationTime, averagePatchSize = $avgPatchSize")
      println(s"  Cross time: $crossAverageTime +- ${math.sqrt(crossAverageCnt / (crossAverageCnt - 1.0) * (crossAverageTimeSumSq / crossAverageCnt - crossAverageTime * crossAverageTime))}")
      println(s"  Cross size: $crossAveragePatch +- ${math.sqrt(crossAverageCnt / (crossAverageCnt - 1.0) * (crossAveragePatchSumSq / crossAverageCnt - crossAveragePatch * crossAveragePatch))}")

