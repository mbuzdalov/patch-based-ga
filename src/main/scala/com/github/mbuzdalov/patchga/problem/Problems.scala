package com.github.mbuzdalov.patchga.problem

import com.github.mbuzdalov.patchga.config.*
import com.github.mbuzdalov.patchga.infra.*
import com.github.mbuzdalov.patchga.population.*
import com.github.mbuzdalov.patchga.representation.UnconstrainedBitString

object Problems:
  type OneMaxFT = UnconstrainedBitString & OneMax & Population & ThreadLocalRandomProvider & FixedTargetTerminator

  def naiveOneMaxFT(size: Int): OneMaxFT = new UnconstrainedBitString(size) with OneMax with NaiveScratchPopulation
    with ThreadLocalRandomProvider with FixedTargetTerminator:
    override def targetFitness: Fitness = size

  def incrementalOneMaxFT(size: Int): OneMaxFT = new UnconstrainedBitString(size) with OneMax with OneMax.Incremental
    with SingleSlotMSTPopulation with ThreadLocalRandomProvider with FixedTargetTerminator.Incremental:
    override def targetFitness: Fitness = size

  type KnapsackFB = UnconstrainedBitString & Knapsack & Population & ThreadLocalRandomProvider & FixedBudgetTerminator

  def naiveKnapsackFB(weights: IArray[Int], values: IArray[Int], capacity: Int, budget: Int): KnapsackFB =
    new UnconstrainedBitString(weights.length) with Knapsack(weights, values, capacity) with NaiveScratchPopulation
      with ThreadLocalRandomProvider with FixedBudgetTerminator(budget)

  def incrementalKnapsackFB(weights: IArray[Int], values: IArray[Int], capacity: Int, budget: Int): KnapsackFB & SingleSlotMSTPopulation =
    new UnconstrainedBitString(weights.length) with Knapsack(weights, values, capacity) with Knapsack.Incremental
      with SingleSlotMSTPopulation with ThreadLocalRandomProvider
      with FixedBudgetTerminator(budget) with FixedBudgetTerminator.Incremental
