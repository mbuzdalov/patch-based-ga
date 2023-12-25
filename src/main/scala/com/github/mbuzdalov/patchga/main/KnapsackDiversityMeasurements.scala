package com.github.mbuzdalov.patchga.main

import java.io.PrintWriter
import java.util.Random

import scala.util.Using

import com.github.mbuzdalov.patchga.algorithm.*
import com.github.mbuzdalov.patchga.distribution.BinomialDistribution
import com.github.mbuzdalov.patchga.infra.FixedBudgetTerminator
import com.github.mbuzdalov.patchga.problem.Problems
import com.github.mbuzdalov.patchga.util.Loops

object KnapsackDiversityMeasurements:
  def main(args: Array[String]): Unit =
    val n = args(0).toInt
    val budget = args(1).toInt

    val rng = new Random(n * 7632453253523432L)
    def randomArray() = IArray.fill(n)(10000 + rng.nextInt(10000))
    val weights, values = randomArray()
    val capacity = weights.sum / 2

    val optimizer = new MuPlusOneGA(10, 0.9, n => BinomialDistribution(n, math.min(1, 1.4 / n)))
    def newKnapsack() = Problems.incrementalKnapsackFB(weights, values, capacity, budget, allowDuplicates = true)

    Using.resource(new PrintWriter("diversity-correlations.csv")): out =>
      out.println("evaluations,avg-time,avg-patch-size")
      Loops.loop(0, 110): t =>
        val instance = newKnapsack()
        FixedBudgetTerminator.runUntilBudgetReached(optimizer)(instance)
        if t >= 10 then
          for result <- instance.timePatchBudgetCorrelations do
            out.println(s"${result.totalEvaluations},${result.averageOperationTime},${result.averagePatchSize}")
