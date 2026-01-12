package com.github.mbuzdalov.patchga.main

import com.github.mbuzdalov.patchga.algorithm.*
import com.github.mbuzdalov.patchga.config.FitnessType
import com.github.mbuzdalov.patchga.infra.FixedBudgetTerminator
import com.github.mbuzdalov.patchga.problem.{Knapsack, Problems}
import com.github.mbuzdalov.patchga.util.{Loops, MeanAndStandardDeviation}

import java.util.Random

object KnapsackWallClockTimeMeasurements:
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
    val compactOutput = args.length > 4 && args(4) == "compact"

    val twoPlusOneGA = MuPlusOneGA.withStandardBitMutation(2, 0.9, 1.2)
    val tenPlusOneGA = MuPlusOneGA.withStandardBitMutation(10, 0.9, 1.4)
    val fiftyPlusOneGA = MuPlusOneGA.withStandardBitMutation(50, 0.9, 1.4)

    val evaluations = new MeanAndStandardDeviation(window = 10)

    if !compactOutput then println(s"$algo, $flavour, $n:")

    val rng = new Random(n * 76324532535L)
    def randomArray() = IArray.fill(n)(10000 + rng.nextInt(10000))

    def naive() =
      val weights, values = randomArray()
      Problems.naiveKnapsackFB(weights, values, weights.sum / 2, budget, allowDuplicates = true, disableDiscard = false)

    def incremental() =
      val weights, values = randomArray()
      Problems.incrementalKnapsackFB(weights, values, weights.sum / 2, budget, allowDuplicates = true, disableDiscard = false)

    def newProblem() = flavour match
      case "naive" => naive()
      case "incre" => incremental()

    Loops.repeat(20):
      val result = algo match
        case "RLS" => run(OnePlusOneEA.randomizedLocalSearch)(newProblem())
        case "(1+1)" => run(OnePlusOneEA.withStandardBitMutation)(newProblem())
        case "(2+1)" => run(twoPlusOneGA)(newProblem())
        case "(10+1)" => run(tenPlusOneGA)(newProblem())
        case "(50+1)" => run(fiftyPlusOneGA)(newProblem())
      val curr = result.avgTime / budget
      evaluations.record(curr)
      if !compactOutput then
        val cnt = evaluations.count
        if cnt == 10 then
          println(s"$curr. Over last $cnt: ($n,${evaluations.mean})+-(0,${evaluations.stdDev})")
        else
          println(curr)

    if compactOutput then print(s"($n,${evaluations.mean})+-(0,${evaluations.stdDev})")
