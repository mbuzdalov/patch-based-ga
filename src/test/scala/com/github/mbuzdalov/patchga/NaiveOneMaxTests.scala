package com.github.mbuzdalov.patchga

import com.github.mbuzdalov.patchga.algorithm.{DEGAPlus, MuPlusOneGA, OnePlusOneEA, Optimizer}
import com.github.mbuzdalov.patchga.infra.FixedTargetTerminator
import com.github.mbuzdalov.patchga.problem.Problems
import com.github.mbuzdalov.patchga.util.Loops
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NaiveOneMaxTests extends AnyFlatSpec with Matchers:
  private case class RunResults(avgEvaluations: Double, avgTime: Double)

  private def run(optimizer: Optimizer, target: Int)
                 (problem: => optimizer.RequiredConfig & Problems.IntProblem): RunResults =
    val nRuns = 10
    var sumEvaluations = 0.0
    val tBegin = System.nanoTime()
    Loops.repeat(nRuns):
      val instance = problem
      sumEvaluations += FixedTargetTerminator.runUntilTargetReached(optimizer, instance, target).nEvaluations
    RunResults(sumEvaluations / nRuns, (System.nanoTime() - tBegin) * 1e-9 / nRuns)

  private def simpleTest(ns: Int*)
                        (expected: Int => Double)
                        (optimizer: Optimizer)
                        (problem: Int => optimizer.RequiredConfig & Problems.IntProblem): Unit =
    for n <- ns do
      val expectedEvs = expected(n)
      val RunResults(evs, _) = run(optimizer, n)(problem(n))
      evs shouldBe expectedEvs +- (0.3 * expectedEvs)

  "RLS on OneMax" should "work well with naive population w/o genealogy" in
    simpleTest(256, 512, 1024)
              (n => n * math.log(n))
              (OnePlusOneEA.randomizedLocalSearch)
              (n => Problems.naiveOneMaxFT(n, allowDuplicates = true, disableDiscard = false, supportGenealogy = false))
  
  it should "work well with naive population with genealogy" in
    simpleTest(256, 512, 1024)
              (n => n * math.log(n))
              (OnePlusOneEA.randomizedLocalSearch)
              (n => Problems.naiveOneMaxFT(n, allowDuplicates = true, disableDiscard = false, supportGenealogy = true))
  
  "(1+1) EA on OneMax" should "work well with naive population w/o genealogy" in
    simpleTest(256, 512, 1024)
              (n => math.E * n * math.log(n))
              (OnePlusOneEA.withStandardBitMutation)
              (n => Problems.naiveOneMaxFT(n, allowDuplicates = true, disableDiscard = false, supportGenealogy = false))
  
  it should "work well with naive population with genealogy" in
    simpleTest(256, 512, 1024)
              (n => math.E * n * math.log(n))
              (OnePlusOneEA.withStandardBitMutation)
              (n => Problems.naiveOneMaxFT(n, allowDuplicates = true, disableDiscard = false, supportGenealogy = true))
  
  // Constants for (2+1) GA are taken from https://link.springer.com/article/10.1007/s00453-021-00893-w.
  
  "(2+1) GA on OneMax" should "work well with naive population using c=1 w/o genealogy" in
    simpleTest(256, 512, 1024)
              (n => 2.224 * n * math.log(n))
              (MuPlusOneGA.withStandardBitMutation(2, 1.0, 1))
              (n => Problems.naiveOneMaxFT(n, allowDuplicates = true, disableDiscard = false, supportGenealogy = false))
  
  it should "work well with naive population using c=1 with genealogy" in
    simpleTest(256, 512, 1024)
              (n => 2.224 * n * math.log(n))
              (MuPlusOneGA.withStandardBitMutation(2, 1.0, 1))
              (n => Problems.naiveOneMaxFT(n, allowDuplicates = true, disableDiscard = false, supportGenealogy = true))
  
  it should "work well with naive population using c=1.2122 w/o genealogy" in
    simpleTest(256, 512, 1024)
              (n => 2.18417 * n * math.log(n))
              (MuPlusOneGA.withStandardBitMutation(2, 1.0, 1.2122))
              (n => Problems.naiveOneMaxFT(n, allowDuplicates = true, disableDiscard = false, supportGenealogy = false))
  
  it should "work well with naive population using c=1.2122 with genealogy" in
    simpleTest(256, 512, 1024)
              (n => 2.18417 * n * math.log(n))
              (MuPlusOneGA.withStandardBitMutation(2, 1.0, 1.2122))
              (n => Problems.naiveOneMaxFT(n, allowDuplicates = true, disableDiscard = false, supportGenealogy = true))
  
  // Constants for (10+1) GA are taken from https://link.springer.com/article/10.1007/s00453-020-00743-1,
  // but they underestimate the runtime for the used problem sizes.
  
  "(10+1) GA on OneMax" should "work well with naive population w/o genealogy" in
    simpleTest(256, 512, 1024)
              (n => 1.75 * n * math.log(n))
              (MuPlusOneGA.withStandardBitMutation(10, 1.0, 1.43))
              (n => Problems.naiveOneMaxFT(n, allowDuplicates = true, disableDiscard = false, supportGenealogy = false))
  
  it should "work well with naive population with genealogy" in
    simpleTest(256, 512, 1024)
              (n => 1.75 * n * math.log(n))
              (MuPlusOneGA.withStandardBitMutation(10, 1.0, 1.43))
              (n => Problems.naiveOneMaxFT(n, allowDuplicates = true, disableDiscard = false, supportGenealogy = true))


  "DEGA+ on OneMax" should "work well with naive population w/o genealogy" in
    simpleTest(64, 96, 128, 150)
              (n => 2.1 * n * math.log(n))
              (DEGAPlus.withStandardBitMutation)
              (n => Problems.naiveOneMaxFT(n, allowDuplicates = true, disableDiscard = false, supportGenealogy = false))
  
  it should "work well with naive population with genealogy" in
    simpleTest(64, 96, 128, 150)
              (n => 2.1 * n * math.log(n))
              (DEGAPlus.withStandardBitMutation)
              (n => Problems.naiveOneMaxFT(n, allowDuplicates = true, disableDiscard = false, supportGenealogy = true))
