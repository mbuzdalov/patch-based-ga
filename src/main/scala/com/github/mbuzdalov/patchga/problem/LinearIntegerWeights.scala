package com.github.mbuzdalov.patchga.problem

import com.github.mbuzdalov.patchga.config.*
import com.github.mbuzdalov.patchga.util.Loops

import java.util.Random

trait LinearIntegerWeights(maxWeight: Int, weightSeed: Long) extends FitnessType, SimpleFitnessFunction, FitnessComparator:
  self: IndividualType { type Individual <: Array[Boolean] } & MaximumPatchSize =>
  override type Fitness = Long

  protected val weights: IArray[Int] = locally:
    val weightRandom = new Random(weightSeed)
    IArray.fill(maximumPatchSize)(1 + weightRandom.nextInt(maxWeight)) 
  protected val sumWeights: Long = weights.map(_.toLong).sum
  
  override def computeFitness(ind: Individual): Fitness =
    var result = 0L
    Loops.foreach(0, ind.length)(i => if ind(i) then result += weights(i))
    result

  override def compare(lhs: Fitness, rhs: Fitness): Int = java.lang.Long.compare(lhs, rhs)

object LinearIntegerWeights:
  trait Incremental extends LinearIntegerWeights, IncrementalFitnessFunction:
    self: IndividualType { type Individual <: Array[Boolean] } 
      & PatchType { type ImmutablePatch <: IArray[Int] } 
      & MaximumPatchSize =>

    override def computeFitnessFunctionIncrementally(individual: Individual, oldFitness: Fitness, patch: ImmutablePatch): Fitness =
      var newFitness = oldFitness
      Loops.foreach(0, patch.length): i =>
        val idx = patch(i)
        if individual(idx) then
          individual(idx) = false
          newFitness -= weights(idx)
        else
          individual(idx) = true
          newFitness += weights(idx)
      newFitness
