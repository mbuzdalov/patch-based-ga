package com.github.mbuzdalov.patchga.problem

import com.github.mbuzdalov.patchga.algorithm.Optimizer.MinimalRequirements
import com.github.mbuzdalov.patchga.config.*
import com.github.mbuzdalov.patchga.infra.*
import com.github.mbuzdalov.patchga.population.*
import com.github.mbuzdalov.patchga.problem
import com.github.mbuzdalov.patchga.representation.{CompressedBitString, UnconstrainedBitString}

object Problems:
  type FixedTargetProblem = MinimalRequirements & FixedTargetTerminator
  type FixedBudgetProblem = MinimalRequirements & FixedBudgetTerminator
  type KnapsackProblem = FixedBudgetProblem & FitnessType:
    type Fitness = Knapsack.FitnessObject

  def naiveOneMaxFT(size: Int, allowDuplicates: Boolean, disableDiscard: Boolean): FixedTargetProblem =
    new UnconstrainedBitString(size)
      with OneMax.BasicArray
      with NaiveScratchPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider with FixedTargetTerminator:
      override def targetFitness: Fitness = size

  def incrementalOneMaxFT(size: Int, allowDuplicates: Boolean, disableDiscard: Boolean): FixedTargetProblem =
    new UnconstrainedBitString(size)
      with OneMax.BasicArray with OneMax.BasicArrayIncremental
      with SingleSlotMSTPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider with FixedTargetTerminator:
      override def targetFitness: Fitness = size

  def compressedOneMaxFT(size: Int, allowDuplicates: Boolean, disableDiscard: Boolean): FixedTargetProblem =
    new CompressedBitString(size)
      with OneMax.Compressed
      with NaiveScratchPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider with FixedTargetTerminator:
      override def targetFitness: Fitness = size
  
  def incrementalTwoMaxFT(size: Int): FixedTargetProblem =
    new UnconstrainedBitString(size)
      with OneMax.BasicArray with OneMax.BasicArrayIncremental
      with SingleSlotMSTPopulation(allowDuplicates = false, disableDiscard = true)
      with ThreadLocalRandomProvider with FixedTargetTerminator:
      private var mask = 0
      
      override def targetFitness: Fitness = size

      // this is an on-the-fly conversion from OneMax to TwoMax fitness
      override def compare(lhs: Fitness, rhs: Fitness): Int = 
        super.compare(math.max(lhs, size - lhs), math.max(rhs, size - rhs))

      // this interferes with throwing target reached exceptions until all optima are found   
      override def recordEvaluation(individual: Array[Boolean], fitness: Int): Unit = 
        try super.recordEvaluation(individual, fitness) catch
          case e: TargetReached =>
            mask |= (if individual(0) then 1 else 2)
            if mask == 3 then throw e
  
  def incrementalCliffFT(size: Int, gap: Int, allowDuplicates: Boolean, disableDiscard: Boolean): FixedTargetProblem =
    new UnconstrainedBitString(size)
      with OneMax.BasicArray with Cliff(size, gap) with OneMax.BasicArrayIncremental
      with SingleSlotMSTPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider with FixedTargetTerminator:
      override def targetFitness: Fitness = size
  
  def compressedCliffFT(size: Int, gap: Int, allowDuplicates: Boolean, disableDiscard: Boolean): FixedTargetProblem =
    new CompressedBitString(size)
      with OneMax.Compressed with Cliff(size, gap)
      with NaiveScratchPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider with FixedTargetTerminator:
      override def targetFitness: Fitness = size
  
  def incrementalPlateauFT(size: Int, gap: Int, allowDuplicates: Boolean, disableDiscard: Boolean): FixedTargetProblem =
    new UnconstrainedBitString(size)
      with OneMax.BasicArray with Plateau(size, gap) with OneMax.BasicArrayIncremental
      with SingleSlotMSTPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider with FixedTargetTerminator:
      override def targetFitness: Fitness = size
  
  def naiveLinearFT(weightCounts: IArray[Int], weightSeed: Long, allowDuplicates: Boolean, disableDiscard: Boolean): FixedTargetProblem =
    new UnconstrainedBitString(weightCounts.sum)
      with LinearIntegerWeights(weightCounts, weightSeed)
      with NaiveScratchPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider with FixedTargetTerminator:
      override def targetFitness: Fitness = sumWeights

  def incrementalLinearFT(weightCounts: IArray[Int], weightSeed: Long, allowDuplicates: Boolean, disableDiscard: Boolean): FixedTargetProblem =
    new UnconstrainedBitString(weightCounts.sum)
      with LinearIntegerWeights(weightCounts, weightSeed) with LinearIntegerWeights.Incremental
      with SingleSlotMSTPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider with FixedTargetTerminator:
      override def targetFitness: Fitness = sumWeights

  def naiveLeadingOnesFT(size: Int, allowDuplicates: Boolean, disableDiscard: Boolean): FixedTargetProblem =
    new UnconstrainedBitString(size)
      with LeadingOnes
      with NaiveScratchPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider with FixedTargetTerminator:
      override def targetFitness: Fitness = size

  def incrementalLeadingOnesFT(size: Int, allowDuplicates: Boolean, disableDiscard: Boolean): FixedTargetProblem =
    new UnconstrainedBitString(size)
      with LeadingOnes with LeadingOnes.Incremental
      with SingleSlotMSTPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider with FixedTargetTerminator:
      override def targetFitness: Fitness = size

  def naiveKnapsackFB(weights: IArray[Int], values: IArray[Int],
                      capacity: Int, budget: Int, allowDuplicates: Boolean, disableDiscard: Boolean): KnapsackProblem =
    new UnconstrainedBitString(weights.length)
      with Knapsack(weights, values, capacity)
      with NaiveScratchPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider with FixedBudgetTerminator(budget)

  def incrementalKnapsackFB(weights: IArray[Int], values: IArray[Int], 
                            capacity: Int, budget: Int, allowDuplicates: Boolean, disableDiscard: Boolean): KnapsackProblem & TimePatchBudgetCorrelation =
    new UnconstrainedBitString(weights.length)
      with Knapsack(weights, values, capacity) with Knapsack.Incremental
      with SingleSlotMSTPopulation(allowDuplicates, disableDiscard)
      with ThreadLocalRandomProvider with FixedBudgetTerminator(budget)
      with TimePatchBudgetCorrelation(10)
