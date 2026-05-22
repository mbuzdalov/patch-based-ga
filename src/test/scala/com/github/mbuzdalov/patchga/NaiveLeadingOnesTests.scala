package com.github.mbuzdalov.patchga

import com.github.mbuzdalov.patchga.algorithm.*
import com.github.mbuzdalov.patchga.problem.Problems
import org.scalatest.flatspec.AnyFlatSpec

class NaiveLeadingOnesTests extends IntProblemTestsBase:
  "RLS on LeadingOnes" should "work well with naive population" in
    simpleTest(64, 96, 128)
              (n => 0.5 * n * (n + 1)) // classic bound
              (OnePlusOneEA.randomizedLocalSearch)
              (n => Problems.naiveLeadingOnesFT(n, allowDuplicates = true, disableDiscard = false))

  "(1+1) EA on OneMax" should "work well with naive population" in
    simpleTest(64, 96, 128)
              (n => 0.86 * n * (n + 1)) // Bottcher, Doerr, Neumann
              (OnePlusOneEA.withStandardBitMutation)
              (n => Problems.naiveLeadingOnesFT(n, allowDuplicates = true, disableDiscard = false))
  
  "DEGA+ on LeadingOnes" should "work well with naive population" in
    simpleTest(64, 96, 128)
              (n => 2.5 * math.pow(n, 1.75)) // rough fits to make tests pass
              (DEGAPlus.withStandardBitMutation)
              (n => Problems.naiveLeadingOnesFT(n, allowDuplicates = true, disableDiscard = false))
