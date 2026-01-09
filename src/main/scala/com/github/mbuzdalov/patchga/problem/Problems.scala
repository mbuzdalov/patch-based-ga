package com.github.mbuzdalov.patchga.problem

import com.github.mbuzdalov.patchga.config.*
import com.github.mbuzdalov.patchga.infra.*
import com.github.mbuzdalov.patchga.population.*
import com.github.mbuzdalov.patchga.problem
import com.github.mbuzdalov.patchga.representation.{CompressedBitString, UnconstrainedBitString}

object Problems:
  type OneMaxFT = UnconstrainedBitString & OneMax.BasicArray & Population & ThreadLocalRandomProvider & FixedTargetTerminator
  type CompOneMaxFT = CompressedBitString & OneMax.Compressed & Population & ThreadLocalRandomProvider & FixedTargetTerminator

  def naiveOneMaxFT(size: Int, allowDuplicates: Boolean, disableDiscard: Boolean): OneMaxFT =
    new UnconstrainedBitString(size)
      with OneMax.BasicArray
      with NaiveScratchPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider with FixedTargetTerminator:
      override def targetFitness: Fitness = size

  def incrementalOneMaxFT(size: Int, allowDuplicates: Boolean, disableDiscard: Boolean): OneMaxFT =
    new UnconstrainedBitString(size)
      with OneMax.BasicArray with OneMax.BasicArrayIncremental
      with SingleSlotMSTPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider with FixedTargetTerminator.Incremental:
      override def targetFitness: Fitness = size

  def compressedOneMaxFT(size: Int, allowDuplicates: Boolean, disableDiscard: Boolean): CompOneMaxFT =
    new CompressedBitString(size)
      with OneMax.Compressed
      with NaiveScratchPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider with FixedTargetTerminator:
      override def targetFitness: Fitness = size
  
  type LinearFT = UnconstrainedBitString & LinearIntegerWeights & Population & ThreadLocalRandomProvider & FixedTargetTerminator

  def naiveLinearFT(size: Int, weightCounts: IArray[Int], weightSeed: Long, allowDuplicates: Boolean, disableDiscard: Boolean): LinearFT =
    new UnconstrainedBitString(size)
      with LinearIntegerWeights(weightCounts, weightSeed)
      with NaiveScratchPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider with FixedTargetTerminator:
      override def targetFitness: Fitness = sumWeights

  def incrementalLinearFT(size: Int, weightCounts: IArray[Int], weightSeed: Long, allowDuplicates: Boolean, disableDiscard: Boolean): LinearFT =
    new UnconstrainedBitString(size)
      with LinearIntegerWeights(weightCounts, weightSeed) with LinearIntegerWeights.Incremental
      with SingleSlotMSTPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider with FixedTargetTerminator.Incremental:
      override def targetFitness: Fitness = sumWeights

  type LeadingOnesFT = UnconstrainedBitString & LeadingOnes & Population & ThreadLocalRandomProvider & FixedTargetTerminator

  def naiveLeadingOnesFT(size: Int, allowDuplicates: Boolean, disableDiscard: Boolean): LeadingOnesFT =
    new UnconstrainedBitString(size)
      with LeadingOnes
      with NaiveScratchPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider with FixedTargetTerminator:
      override def targetFitness: Fitness = size

  def incrementalLeadingOnesFT(size: Int, allowDuplicates: Boolean, disableDiscard: Boolean): LeadingOnesFT =
    new UnconstrainedBitString(size)
      with LeadingOnes with LeadingOnes.Incremental
      with SingleSlotMSTPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider with FixedTargetTerminator.Incremental:
      override def targetFitness: Fitness = size

  type KnapsackFB = UnconstrainedBitString & Knapsack & Population & ThreadLocalRandomProvider & FixedBudgetTerminator

  def naiveKnapsackFB(weights: IArray[Int], values: IArray[Int],
                      capacity: Int, budget: Int, allowDuplicates: Boolean, disableDiscard: Boolean): KnapsackFB =
    new UnconstrainedBitString(weights.length)
      with Knapsack(weights, values, capacity)
      with NaiveScratchPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider with FixedBudgetTerminator(budget)

  def incrementalKnapsackFB(weights: IArray[Int], values: IArray[Int], 
                            capacity: Int, budget: Int, allowDuplicates: Boolean, disableDiscard: Boolean): KnapsackFB & TimePatchBudgetCorrelation =
    new UnconstrainedBitString(weights.length)
      with Knapsack(weights, values, capacity) with Knapsack.Incremental
      with SingleSlotMSTPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider with FixedBudgetTerminator(budget) with FixedBudgetTerminator.Incremental
      with TimePatchBudgetCorrelation(10)
