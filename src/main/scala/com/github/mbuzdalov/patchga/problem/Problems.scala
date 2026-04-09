package com.github.mbuzdalov.patchga.problem

import com.github.mbuzdalov.patchga.config.*
import com.github.mbuzdalov.patchga.infra.*
import com.github.mbuzdalov.patchga.population.*
import com.github.mbuzdalov.patchga.problem
import com.github.mbuzdalov.patchga.representation.{CompressedBitString, UnconstrainedBitString}

object Problems:
  type MinimalRequirements = IndividualType & FitnessType & Population & MaximumPatchSize & FitnessComparator & RandomProvider
  type IntProblem = MinimalRequirements & FitnessType:
    type Fitness = Int
  type LongProblem = MinimalRequirements & FitnessType:
    type Fitness = Long
  type KnapsackProblem = MinimalRequirements & FitnessType:
    type Fitness = Knapsack.FitnessObject

  def naiveOneMaxFT(size: Int, allowDuplicates: Boolean, disableDiscard: Boolean, supportGenealogy: Boolean): IntProblem =
    new UnconstrainedBitString(size)
      with OneMax.BasicArray
      with NaiveScratchPopulation(allowDuplicates, disableDiscard, supportGenealogy)
      with ThreadLocalRandomProvider

  def incrementalOneMaxFT(size: Int, allowDuplicates: Boolean, disableDiscard: Boolean): IntProblem =
    new UnconstrainedBitString(size)
      with OneMax.BasicArray with OneMax.BasicArrayIncremental
      with SingleSlotMSTPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider

  def compressedOneMaxFT(size: Int, allowDuplicates: Boolean, disableDiscard: Boolean, supportGenealogy: Boolean): IntProblem =
    new CompressedBitString(size)
      with OneMax.Compressed
      with NaiveScratchPopulation(allowDuplicates, disableDiscard, supportGenealogy)
      with ThreadLocalRandomProvider
  
  def incrementalTwoMaxFT(size: Int): IntProblem =
    new UnconstrainedBitString(size)
      with OneMax.BasicArray with OneMax.BasicArrayIncremental
      with SingleSlotMSTPopulation(allowDuplicates = false, disableDiscard = true)
      with ThreadLocalRandomProvider:
      // this is an on-the-fly conversion from OneMax to TwoMax fitness
      override def compare(lhs: Fitness, rhs: Fitness): Int = 
        super.compare(math.max(lhs, size - lhs), math.max(rhs, size - rhs))
  
  def incrementalCliffFT(size: Int, gap: Int, allowDuplicates: Boolean, disableDiscard: Boolean): IntProblem =
    new UnconstrainedBitString(size)
      with OneMax.BasicArray with Cliff(size, gap) with OneMax.BasicArrayIncremental
      with SingleSlotMSTPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider
  
  def compressedCliffFT(size: Int, gap: Int, allowDuplicates: Boolean, disableDiscard: Boolean): IntProblem =
    new CompressedBitString(size)
      with OneMax.Compressed with Cliff(size, gap)
      with NaiveScratchPopulation(allowDuplicates, disableDiscard, supportGenealogy = false)
      with ThreadLocalRandomProvider
  
  def incrementalPlateauFT(size: Int, gap: Int, allowDuplicates: Boolean, disableDiscard: Boolean): IntProblem =
    new UnconstrainedBitString(size)
      with OneMax.BasicArray with Plateau(size, gap) with OneMax.BasicArrayIncremental
      with SingleSlotMSTPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider
  
  def naiveLinearFT(weightCounts: IArray[Int], weightSeed: Long, allowDuplicates: Boolean, disableDiscard: Boolean): LongProblem =
    new UnconstrainedBitString(weightCounts.sum)
      with LinearIntegerWeights(weightCounts, weightSeed)
      with NaiveScratchPopulation(allowDuplicates, disableDiscard, supportGenealogy = false)
      with ThreadLocalRandomProvider

  def incrementalLinearFT(weightCounts: IArray[Int], weightSeed: Long, allowDuplicates: Boolean, disableDiscard: Boolean): LongProblem =
    new UnconstrainedBitString(weightCounts.sum)
      with LinearIntegerWeights(weightCounts, weightSeed) with LinearIntegerWeights.Incremental
      with SingleSlotMSTPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider

  def naiveLeadingOnesFT(size: Int, allowDuplicates: Boolean, disableDiscard: Boolean): IntProblem =
    new UnconstrainedBitString(size)
      with LeadingOnes
      with NaiveScratchPopulation(allowDuplicates, disableDiscard, supportGenealogy = false)
      with ThreadLocalRandomProvider

  def incrementalLeadingOnesFT(size: Int, allowDuplicates: Boolean, disableDiscard: Boolean): IntProblem =
    new UnconstrainedBitString(size)
      with LeadingOnes with LeadingOnes.Incremental
      with SingleSlotMSTPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider

  def naiveKnapsackFB(weights: IArray[Int], values: IArray[Int],
                      capacity: Int, allowDuplicates: Boolean, disableDiscard: Boolean): KnapsackProblem =
    new UnconstrainedBitString(weights.length)
      with Knapsack(weights, values, capacity)
      with NaiveScratchPopulation(allowDuplicates, disableDiscard, supportGenealogy = false)
      with ThreadLocalRandomProvider

  def incrementalKnapsackFB(weights: IArray[Int], values: IArray[Int], 
                            capacity: Int, allowDuplicates: Boolean, disableDiscard: Boolean): KnapsackProblem & SingleSlotMSTPopulation =
    new UnconstrainedBitString(weights.length)
      with Knapsack(weights, values, capacity) with Knapsack.Incremental
      with SingleSlotMSTPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider
