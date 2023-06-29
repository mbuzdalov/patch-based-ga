package com.github.mbuzdalov.patchga.problem

import com.github.mbuzdalov.patchga.config.*
import com.github.mbuzdalov.patchga.util.Loops

trait LeadingOnes extends FitnessType, SimpleFitnessFunction, FitnessComparator:
  self: IndividualType { type Individual <: Array[Boolean] } =>
  override type Fitness = Int

  override def computeFitness(ind: Individual): Fitness = Loops.find(0, ind.length)(i => !ind(i))

  override def compare(lhs: Fitness, rhs: Fitness): Int = java.lang.Integer.compare(lhs, rhs)

object LeadingOnes:
  trait Incremental extends LeadingOnes, IncrementalFitnessFunction:
    self: IndividualType { type Individual <: Array[Boolean] } & PatchType { type ImmutablePatch <: IArray[Int] } =>

    override def computeFitnessFunctionIncrementally(individual: Individual, oldFitness: Fitness, patch: ImmutablePatch): Fitness =
      var newFitness = oldFitness
      // Phase 1: apply the patch, unroll fitness if the prefix is hit
      Loops.loop(0, patch.length): i =>
        val idx = patch(i)
        if individual(idx) then
          individual(idx) = false
          if newFitness > idx then newFitness = idx
        else
          individual(idx) = true
      // Phase 2: if the prefix is not hit, try to extend
      if newFitness == oldFitness then
        while newFitness < individual.length && individual(newFitness) do
          newFitness += 1
      // Now done    
      newFitness
