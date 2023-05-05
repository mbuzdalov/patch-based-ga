package com.github.mbuzdalov.patchga.problem

import com.github.mbuzdalov.patchga.config.*
import com.github.mbuzdalov.patchga.util.Loops

trait LinearIntegerWeights(maxWeight: Int) extends FitnessType, SimpleFitnessFunction, FitnessComparator:
  self: IndividualType { type Individual <: Array[Boolean] } & RandomProvider & MaximumPatchSize =>
  override type Fitness = Long

  protected val weights: IArray[Int] = IArray.fill(maximumPatchSize)(1 + random.nextInt(maxWeight)) 
  protected val sumWeights: Long = weights.map(_.toLong).sum
  
  override def computeFitness(ind: Individual): Fitness =
    var result = 0L
    Loops.loop(0, ind.length)(i => if ind(i) then result += weights(i))
    result

  override def compare(lhs: Fitness, rhs: Fitness): Int = java.lang.Long.compare(lhs, rhs)

object LinearIntegerWeights:
  trait Incremental extends LinearIntegerWeights, IncrementalFitnessFunction:
    self: IndividualType { type Individual <: Array[Boolean] } & PatchType { type ImmutablePatch <: IArray[Int] } 
      & RandomProvider & MaximumPatchSize =>

    override def computeFitnessFunctionIncrementally(individual: Individual, oldFitness: Fitness, patch: ImmutablePatch): Fitness =
      var newFitness = oldFitness
      Loops.loop(0, patch.length) { i =>
        val idx = patch(i)
        if individual(idx) then
          individual(idx) = false
          newFitness -= weights(idx)
        else
          individual(idx) = true
          newFitness += weights(idx)
      }
      newFitness
