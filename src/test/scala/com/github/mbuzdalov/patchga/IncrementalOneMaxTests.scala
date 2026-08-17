package com.github.mbuzdalov.patchga

import com.github.mbuzdalov.patchga.algorithm.*
import com.github.mbuzdalov.patchga.distribution.{BinomialDistribution, PowerLawDistribution}
import com.github.mbuzdalov.patchga.problem.Problems
import org.scalatest.flatspec.AnyFlatSpec

class IncrementalOneMaxTests extends IntProblemTestsBase:
  "RLS on OneMax" should "work well with single-slot MST-based population" in
    simpleTest(256, 512, 1024)
              (n => n * math.log(n))
              (OnePlusOneEA.randomizedLocalSearch)
              (n => Problems.incrementalOneMaxFT(n, allowDuplicates = true, disableDiscard = false))

  "(1+1) EA on OneMax" should "work well with single-slot MST-based population" in
    simpleTest(256, 512, 1024)
              (n => math.E * n * math.log(n))
              (OnePlusOneEA.withStandardBitMutation)
              (n => Problems.incrementalOneMaxFT(n, allowDuplicates = true, disableDiscard = false))

  // Constants for (2+1) GA are taken from https://link.springer.com/article/10.1007/s00453-021-00893-w.

  "(2+1) GA on OneMax" should "work well with single-slot MST-based population using c=1" in
    simpleTest(256, 512, 1024)
              (n => 2.224 * n * math.log(n))
              (MuPlusOneGA.withStandardBitMutation(2, 1.0, 1))
              (n => Problems.incrementalOneMaxFT(n, allowDuplicates = true, disableDiscard = false))

  it should "work well with single-slot MST-based population using c=1.2122" in
    simpleTest(256, 512, 1024)
              (n => 2.18417 * n * math.log(n))
              (MuPlusOneGA.withStandardBitMutation(2, 1.0, 1.2122))
              (n => Problems.incrementalOneMaxFT(n, allowDuplicates = true, disableDiscard = false))

  // Constants for (10+1) GA are taken from https://link.springer.com/article/10.1007/s00453-020-00743-1,
  // but they underestimate the runtime for the used problem sizes.

  "(10+1) GA on OneMax" should "work well with single-slot MST-based population" in
    simpleTest(256, 512, 1024)
              (n => 1.75 * n * math.log(n))
              (MuPlusOneGA.withStandardBitMutation(10, 1.0, 1.43))
              (n => Problems.incrementalOneMaxFT(n, allowDuplicates = true, disableDiscard = false))
  
  // (2+1) DEGA+, runtimes empirically calibrated
  
  "DEGA+ on OneMax" should "work well with single-slot MST-based population" in
    simpleTest(64, 96, 128, 150)
              (n => 2.15 * n * math.log(n))
              (DEGAPlus.withStandardBitMutation)
              (n => Problems.incrementalOneMaxFT(n, allowDuplicates = true, disableDiscard = false))

  // (1+(lambda,lambda)) GA, heavy-tailed version
  
  "(1+(L,L)) GA on OneMax" should "work well with single-slot MST-based population" in
    simpleTest(128, 256, 512)
              (n => 4 * n * math.log(math.log(n)))
              (OnePlusLLGA(PowerLawDistribution(_, 2.5), PowerLawDistribution(_, 2.5)))
              (n => Problems.incrementalOneMaxFT(n, allowDuplicates = true, disableDiscard = false))

  // NFGA

  "NFGA with UX on OneMax" should "work well with single-slot MST-based population" in
    simpleTest(64, 128, 256)
      (n => 4 * n * math.log(math.log(n)))
      (NeverForgettingGA(2.5, 1.5, 2.5, 0.5, 1.5, None, 2.5, n => BinomialDistribution(n - 2, 0.5) + 1))
      (n => Problems.incrementalOneMaxFT(n, allowDuplicates = true, disableDiscard = false))
