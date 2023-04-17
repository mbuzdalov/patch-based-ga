package com.github.mbuzdalov.patchga.main

import java.util.Random

import com.github.mbuzdalov.patchga.algorithm.*
import com.github.mbuzdalov.patchga.config.FitnessType
import com.github.mbuzdalov.patchga.distribution.BinomialDistribution
import com.github.mbuzdalov.patchga.infra.FixedBudgetTerminator
import com.github.mbuzdalov.patchga.problem.{Knapsack, Problems}
import com.github.mbuzdalov.patchga.util.Loops

object KnapsackTimeMeasurements:
  private case class RunResults(avgTime: Double, avgFitness: Double):
    def toString(budget: Int): String = s"${avgTime / budget} (average fitness $avgFitness)"

  private def run(optimizer: Optimizer)
                 (problem: => optimizer.RequiredConfig & FixedBudgetTerminator & FitnessType { type Fitness = Knapsack.FitnessObject }): RunResults =

    var sumFitnessValues: Double = 0.0
    var nRuns = 0L
    val tBegin = System.nanoTime()
    while System.nanoTime() - tBegin < 1e9 do
      val instance = problem
      try
        optimizer.optimize(instance)
      catch
        case e: instance.BudgetReached =>
          nRuns += 1
          if e.fitness.isValid then sumFitnessValues += e.fitness.sumValues
    RunResults((System.nanoTime() - tBegin) * 1e-9 / nRuns, sumFitnessValues / nRuns)

  def main(args: Array[String]): Unit =
    val algo = args(0)
    val flavour = args(1)
    val n = args(2).toInt
    val budget = args(3).toInt

    val twoPlusOneGA = new MuPlusOneGA(2, 0.9, n => BinomialDistribution(n, 1.2 / n))
    val tenPlusOneGA = new MuPlusOneGA(10, 0.9, n => BinomialDistribution(n, 1.4 / n))
    val fiftyPlusOneGA = new MuPlusOneGA(50, 0.9, n => BinomialDistribution(n, 1.4 / n))

    var sumAvgEvals, sumSqAvgEvals = 0.0
    val sequence = new Array[Double](10)
    var index = 0

    println(s"$algo, $flavour, $n:")

    val rng = new Random(n * 76324532535L)
    def randomArray() = IArray.fill(n)(10000 + rng.nextInt(10000))

    def naive() =
      val weights, values = randomArray()
      Problems.naiveKnapsackFB(weights, values, weights.sum / 2, budget)

    def incremental() =
      val weights, values = randomArray()
      Problems.incrementalKnapsackFB(weights, values, weights.sum / 2, budget)

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
      val m = sequence(index % 10)
      sumAvgEvals -= m
      sumSqAvgEvals -= m * m
      val curr = result.avgTime / budget
      sequence(index % 10) = curr
      index += 1
      sumAvgEvals += curr
      sumSqAvgEvals += curr * curr
      if index > 10 then
        val l = sequence.length
        println(s"$curr. Over last $l: ($n,${sumAvgEvals / l})+-(0,${math.sqrt(l / (l - 1.0) * (sumSqAvgEvals / l - (sumAvgEvals / l) * (sumAvgEvals / l)))})")
      else
        println(curr)
