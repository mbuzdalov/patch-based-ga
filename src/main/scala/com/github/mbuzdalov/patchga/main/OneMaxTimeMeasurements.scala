package com.github.mbuzdalov.patchga.main

import com.github.mbuzdalov.patchga.algorithm.*
import com.github.mbuzdalov.patchga.distribution.BinomialDistribution
import com.github.mbuzdalov.patchga.infra.FixedTargetTerminator
import com.github.mbuzdalov.patchga.problem.Problems
import com.github.mbuzdalov.patchga.util.{Loops, MeanAndStandardDeviation}

object OneMaxTimeMeasurements:
  private case class RunResults(avgEvaluations: Double, avgTime: Double):
    def avgTimePerEval: Double = avgTime / avgEvaluations

  private def run(optimizer: Optimizer)
                 (problem: => optimizer.RequiredConfig & FixedTargetTerminator): RunResults =
    var sumEvaluations = 0.0
    var nRuns = 0L
    val tBegin = System.nanoTime()
    while System.nanoTime() - tBegin < 1e9 do
      val instance = problem
      val result = FixedTargetTerminator.runUntilTargetReached(optimizer)(instance)
      nRuns += 1
      sumEvaluations += result.nEvaluations
    RunResults(sumEvaluations / nRuns, (System.nanoTime() - tBegin) * 1e-9 / nRuns)

  def main(args: Array[String]): Unit =
    val algo = args(0)
    val flavour = args(1)
    val n = args(2).toInt

    val twoPlusOneGA = new MuPlusOneGA(2, 0.9, n => BinomialDistribution(n, 1.2 / n))
    val tenPlusOneGA = new MuPlusOneGA(10, 0.9, n => BinomialDistribution(n, 1.4 / n))
    val fiftyPlusOneGA = new MuPlusOneGA(50, 0.9, n => BinomialDistribution(n, 1.4 / n))

    val evaluations = new MeanAndStandardDeviation(window = 10)

    println(s"$algo, $flavour, $n:")

    def newProblem() = flavour match
      case "naive" => Problems.naiveOneMaxFT(n)
      case "incre" => Problems.incrementalOneMaxFT(n)

    while System.in.available() == 0 do
      val result = algo match
        case "RLS" => run(RandomizedLocalSearch)(newProblem())
        case "(1+1)" => run(OnePlusOneEA.withStandardBitMutation)(newProblem())
        case "(2+1)" => run(twoPlusOneGA)(newProblem())
        case "(10+1)" => run(tenPlusOneGA)(newProblem())
        case "(50+1)" => run(fiftyPlusOneGA)(newProblem())
      val curr = result.avgTimePerEval
      evaluations.record(curr)
      val cnt = evaluations.count
      if cnt == 10 then
        println(s"$curr. Over last $cnt: ($n,${evaluations.mean})+-(0,${evaluations.stdDev})")
      else
        println(curr)
