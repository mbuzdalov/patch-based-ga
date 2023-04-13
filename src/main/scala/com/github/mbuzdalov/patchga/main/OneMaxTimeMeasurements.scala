package com.github.mbuzdalov.patchga.main

import com.github.mbuzdalov.patchga.algorithm.*
import com.github.mbuzdalov.patchga.config.*
import com.github.mbuzdalov.patchga.distribution.BinomialDistribution
import com.github.mbuzdalov.patchga.infra.*
import com.github.mbuzdalov.patchga.population.*
import com.github.mbuzdalov.patchga.problem.OneMax
import com.github.mbuzdalov.patchga.representation.UnconstrainedBitString
import com.github.mbuzdalov.patchga.util.Loops

object OneMaxTimeMeasurements:
  private class NaiveOneMax(size: Int)
    extends UnconstrainedBitString(size), OneMax, NaiveScratchPopulation,
      ThreadLocalRandomProvider, FixedTargetTerminator:
    override def targetFitness: Fitness = size

  private class IncrementalOneMax(size: Int)
    extends UnconstrainedBitString(size), OneMax, OneMax.Incremental, SingleSlotMSTPopulation,
      ThreadLocalRandomProvider, FixedTargetTerminator.Incremental:
    override def targetFitness: Fitness = size

  private case class RunResults(avgEvaluations: Double, avgTime: Double):
    def timeOfEval: Double = avgTime / avgEvaluations

  private def run(nRuns: Int)
                 (optimizer: Optimizer)
                 (problem: => optimizer.RequiredConfig & FixedTargetTerminator): RunResults =
    var sumEvaluations = 0.0
    val tBegin = System.nanoTime()
    Loops.loop(0, nRuns) { _ =>
      val instance = problem
      try
        optimizer.optimize(instance)
      catch
        case e: instance.TargetReached => sumEvaluations += e.nEvaluations
    }
    RunResults(sumEvaluations / nRuns, (System.nanoTime() - tBegin) * 1e-9 / nRuns)

  def main(args: Array[String]): Unit =
    for w <- 0 to 2 do
      println("******************************************")
      println(s"************ Warm-up round $w ************")
      println("******************************************")
      for n <- Seq(32, 64, 128, 256, 512, 1024, 2048, 4096) do
        val runMany = run(40960 / n)
        val twoPlusOneGA = new MuPlusOneGA(2, 0.9, n => BinomialDistribution(n, 1.2 / n))
        val tenPlusOneGA = new MuPlusOneGA(2, 0.9, n => BinomialDistribution(n, 1.4 / n))
        println(n)
        println("RLS:")
        println(s"  naive: ${runMany(RandomizedLocalSearch)(new NaiveOneMax(n)).timeOfEval}")
        println(s"  incre: ${runMany(RandomizedLocalSearch)(new IncrementalOneMax(n)).timeOfEval}")
        println("(1+1) EA:")
        println(s"  naive: ${runMany(OnePlusOneEA.withStandardBitMutation)(new NaiveOneMax(n)).timeOfEval}")
        println(s"  incre: ${runMany(OnePlusOneEA.withStandardBitMutation)(new IncrementalOneMax(n)).timeOfEval}")
        println("(2+1) GA:")
        println(s"  naive: ${runMany(twoPlusOneGA)(new NaiveOneMax(n)).timeOfEval}")
        println(s"  incre: ${runMany(twoPlusOneGA)(new IncrementalOneMax(n)).timeOfEval}")
        println("(10+1) GA:")
        println(s"  naive: ${runMany(tenPlusOneGA)(new NaiveOneMax(n)).timeOfEval}")
        println(s"  incre: ${runMany(tenPlusOneGA)(new IncrementalOneMax(n)).timeOfEval}")
