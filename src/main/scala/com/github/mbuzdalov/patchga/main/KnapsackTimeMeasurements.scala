package com.github.mbuzdalov.patchga.main

import java.util.Random

import com.github.mbuzdalov.patchga.algorithm.*
import com.github.mbuzdalov.patchga.distribution.BinomialDistribution
import com.github.mbuzdalov.patchga.infra.{FixedBudgetTerminator, ThreadLocalRandomProvider}
import com.github.mbuzdalov.patchga.population.{NaiveScratchPopulation, SingleSlotMSTPopulation}
import com.github.mbuzdalov.patchga.problem.Knapsack
import com.github.mbuzdalov.patchga.representation.UnconstrainedBitString
import com.github.mbuzdalov.patchga.util.Loops

object KnapsackTimeMeasurements:
  private class NaiveKnapsack(weights: IArray[Int], values: IArray[Int], capacity: Int, budget: Int)
    extends UnconstrainedBitString(weights.length), Knapsack(weights, values, capacity), NaiveScratchPopulation, 
      ThreadLocalRandomProvider, FixedBudgetTerminator(budget)

  private class IncrementalKnapsack(weights: IArray[Int], values: IArray[Int], capacity: Int, budget: Int)
    extends UnconstrainedBitString(weights.length), Knapsack(weights, values, capacity), Knapsack.Incremental,
      SingleSlotMSTPopulation, ThreadLocalRandomProvider, FixedBudgetTerminator(budget), FixedBudgetTerminator.Incremental

  private case class RunResults(avgTime: Double)

  private def run(nRuns: Int)
                 (optimizer: Optimizer)
                 (problem: => optimizer.RequiredConfig & FixedBudgetTerminator): RunResults =
    val tBegin = System.nanoTime()
    Loops.loop(0, nRuns) { _ =>
      val instance = problem
      try
        optimizer.optimize(instance)
      catch
        case e: instance.BudgetReached =>
    }
    val avgTime = (System.nanoTime() - tBegin) * 1e-9 / nRuns
    RunResults(avgTime)

  def main(args: Array[String]): Unit =
    for w <- 0 to 2 do
      println("******************************************")
      println(s"************ Warm-up round $w ************")
      println("******************************************")
      for n <- Seq(32, 64, 128, 256, 512, 1024, 2048, 4096) do
        val rng = new Random(n * 76324532535L)
        val weights, values = IArray.fill(n)(10000 + rng.nextInt(10000))
        val capacity = weights.sum / 2
        val budget = 25000

        def naive() = new NaiveKnapsack(weights, values, capacity, budget)
        def incremental() = new IncrementalKnapsack(weights, values, capacity, budget)
        
        val runMany = run(40960 / n)
        val twoPlusOneGA = new MuPlusOneGA(2, 0.5, n => BinomialDistribution(n, 1.0 / n))
        println(n)
        println("RLS:")
        println(s"  naive: ${runMany(RandomizedLocalSearch)(naive()).avgTime / budget}")
        println(s"  incre: ${runMany(RandomizedLocalSearch)(incremental()).avgTime / budget}")
        println("(1+1) EA:")
        println(s"  naive: ${runMany(OnePlusOneEA.withStandardBitMutation)(naive()).avgTime / budget}")
        println(s"  incre: ${runMany(OnePlusOneEA.withStandardBitMutation)(incremental()).avgTime / budget}")
        println("(2+1) GA:")
        println(s"  naive: ${runMany(twoPlusOneGA)(naive()).avgTime / budget}")
        println(s"  incre: ${runMany(twoPlusOneGA)(incremental()).avgTime / budget}")