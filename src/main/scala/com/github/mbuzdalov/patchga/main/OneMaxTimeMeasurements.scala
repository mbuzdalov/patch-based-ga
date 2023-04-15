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
    def avgTimePerEval: Double = avgTime / avgEvaluations

  private def run(optimizer: Optimizer)
                 (problem: => optimizer.RequiredConfig & FixedTargetTerminator): RunResults =
    var sumEvaluations = 0.0
    var nRuns = 0L
    val tBegin = System.nanoTime()
    while System.nanoTime() - tBegin < 1e9 do
      val instance = problem
      try
        optimizer.optimize(instance)
      catch
        case e: instance.TargetReached =>
          nRuns += 1
          sumEvaluations += e.nEvaluations
    RunResults(sumEvaluations / nRuns, (System.nanoTime() - tBegin) * 1e-9 / nRuns)

  def main(args: Array[String]): Unit =
    val algo = args(0)
    val flavour = args(1)
    val n = args(2).toInt

    val twoPlusOneGA = new MuPlusOneGA(2, 0.9, n => BinomialDistribution(n, 1.2 / n))
    val tenPlusOneGA = new MuPlusOneGA(10, 0.9, n => BinomialDistribution(n, 1.4 / n))
    val fiftyPlusOneGA = new MuPlusOneGA(50, 0.9, n => BinomialDistribution(n, 1.4 / n))

    var sumAvgEvals, sumSqAvgEvals = 0.0
    val sequence = new Array[Double](10)
    var index = 0

    println(s"$algo, $flavour, $n:")

    def newProblem() = flavour match
      case "naive" => new NaiveOneMax(n)
      case "incre" => new IncrementalOneMax(n)

    while true do
      val result = algo match
        case "RLS" => run(RandomizedLocalSearch)(newProblem())
        case "(1+1)" => run(OnePlusOneEA.withStandardBitMutation)(newProblem())
        case "(2+1)" => run(twoPlusOneGA)(newProblem())
        case "(10+1)" => run(tenPlusOneGA)(newProblem())
        case "(50+1)" => run(fiftyPlusOneGA)(newProblem())
      val m = sequence(index % 10)
      sumAvgEvals -= m
      sumSqAvgEvals -= m * m
      val curr = result.avgTimePerEval
      sequence(index % 10) = curr
      index += 1
      sumAvgEvals += curr
      sumSqAvgEvals += curr * curr
      if index > 10 then
        val n = sequence.length
        println(s"$curr. Over last $n: avg = ${sumAvgEvals / n}, stddev = ${math.sqrt(n / (n - 1) * (sumSqAvgEvals / n - (sumAvgEvals / n) * (sumAvgEvals / n)))}")
      else
        println(curr)
