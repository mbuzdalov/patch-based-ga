package com.github.mbuzdalov.patchga.main

import com.github.mbuzdalov.patchga.algorithm.*
import com.github.mbuzdalov.patchga.infra.FixedTargetTerminator
import com.github.mbuzdalov.patchga.problem.Problems
import com.github.mbuzdalov.patchga.util.{Loops, MeanAndStandardDeviation}

object OneMaxWallClockTimeMeasurements:
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
    val compactOutput = args.length > 3 && args(3) == "compact" 

    val twoPlusOneGA = MuPlusOneGA.withStandardBitMutation(2, 0.9, 1.2)
    val tenPlusOneGA = MuPlusOneGA.withStandardBitMutation(10, 0.9, 1.4)
    val fiftyPlusOneGA = MuPlusOneGA.withStandardBitMutation(50, 0.9, 1.4)

    val evaluations, evaluationTimes = new MeanAndStandardDeviation(window = 10)

    if !compactOutput then println(s"$algo, $flavour, $n:")

    def newProblem() = flavour match
      case "naive" => Problems.naiveOneMaxFT(n, allowDuplicates = true, disableDiscard = false)
      case "incre" => Problems.incrementalOneMaxFT(n, allowDuplicates = false, disableDiscard = false)

    Loops.repeat(20):
      val result = algo match
        case "RLS" => run(OnePlusOneEA.randomizedLocalSearch)(newProblem())
        case "(1+1)" => run(OnePlusOneEA.withStandardBitMutation)(newProblem())
        case "(2+1)" => run(twoPlusOneGA)(newProblem())
        case "(10+1)" => run(tenPlusOneGA)(newProblem())
        case "(50+1)" => run(fiftyPlusOneGA)(newProblem())
      val currTime = result.avgTimePerEval
      val currEval = result.avgEvaluations
      evaluationTimes.record(currTime)
      evaluations.record(currEval)
      if !compactOutput then
        val cnt = evaluationTimes.count
        if cnt == 10 then
          println(s"$currTime. Over last $cnt: ($n,${evaluationTimes.mean})+-(0,${evaluationTimes.stdDev})")
          println(s"  $currEval. Over last $cnt: ($n,${evaluations.mean})+-(0,${evaluations.stdDev})")
        else
          println(currTime)
          println(s"  $currEval")

    if compactOutput then print(s"($n,${evaluationTimes.mean})+-(0,${evaluationTimes.stdDev})")
    