package com.github.mbuzdalov.patchga.main

import java.util.Random

import com.github.mbuzdalov.patchga.algorithm.*
import com.github.mbuzdalov.patchga.config.FitnessType
import com.github.mbuzdalov.patchga.distribution.BinomialDistribution
import com.github.mbuzdalov.patchga.infra.FixedBudgetTerminator
import com.github.mbuzdalov.patchga.problem.{Knapsack, Problems}
import com.github.mbuzdalov.patchga.util.{Loops, MeanAndStandardDeviation}

object KnapsackTimeMeasurements:
  private case class RunResults(avgTime: Double, avgFitness: Double)

  private def run(optimizer: Optimizer)
                 (problem: => optimizer.RequiredConfig & FixedBudgetTerminator & FitnessType { type Fitness = Knapsack.FitnessObject }): RunResults =
    var sumFitnessValues: Double = 0.0
    var nRuns = 0L
    val tBegin = System.nanoTime()
    while System.nanoTime() - tBegin < 1e9 do
      val instance = problem
      val result = FixedBudgetTerminator.runUntilBudgetReached(optimizer)(instance)
      nRuns += 1
      if result.fitness.isValid then sumFitnessValues += result.fitness.sumValues
    RunResults((System.nanoTime() - tBegin) * 1e-9 / nRuns, sumFitnessValues / nRuns)

  def main(args: Array[String]): Unit =
    val algo = args(0)
    val flavour = args(1)
    val n = args(2).toInt
    val budget = args(3).toInt

    val twoPlusOneGA = new MuPlusOneGA(2, 0.9, n => BinomialDistribution(n, math.min(1, 1.2 / n)))
    val tenPlusOneGA = new MuPlusOneGA(10, 0.9, n => BinomialDistribution(n, math.min(1, 1.4 / n)))
    val fiftyPlusOneGA = new MuPlusOneGA(50, 0.9, n => BinomialDistribution(n, math.min(1, 1.4 / n)))

    val evaluations = new MeanAndStandardDeviation(window = 10)

    println(s"$algo, $flavour, $n:")

    val rng = new Random(n * 76324532535L)
    def randomArray() = IArray.fill(n)(10000 + rng.nextInt(10000))

    def naive() =
      val weights, values = randomArray()
      Problems.naiveKnapsackFB(weights, values, weights.sum / 2, budget)

    def incremental() =
      val weights, values = randomArray()
      Problems.incrementalKnapsackFB(weights, values, weights.sum / 2, budget, allowDuplicates = true)

    def newProblem() = flavour match
      case "naive" => naive()
      case "incre" => incremental()

    while System.in.available() == 0 do
      val result = algo match
        case "RLS" => run(RandomizedLocalSearch)(newProblem())
        case "(1+1)" => run(OnePlusOneEA.withStandardBitMutation)(newProblem())
        case "(2+1)" => run(twoPlusOneGA)(newProblem())
        case "(10+1)" => run(tenPlusOneGA)(newProblem())
        case "(50+1)" => run(fiftyPlusOneGA)(newProblem())
      val curr = result.avgTime / budget
      evaluations.record(curr)
      val cnt = evaluations.count
      if cnt == 10 then
        println(s"$curr. Over last $cnt: ($n,${evaluations.mean})+-(0,${evaluations.stdDev})")
      else
        println(curr)
