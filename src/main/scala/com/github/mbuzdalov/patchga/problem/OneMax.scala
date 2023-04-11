package com.github.mbuzdalov.patchga.problem

import com.github.mbuzdalov.patchga.config._
import com.github.mbuzdalov.patchga.util.Loops

trait OneMax extends FitnessType, SimpleFitnessFunction, FitnessComparator:
  self: IndividualType { type Individual <: Array[Boolean] } =>
  override type Fitness = Int

  override def computeFitness(ind: Individual): Fitness =
    var result = 0
    Loops.loop(0, ind.length)(i => if ind(i) then result += 1)
    result

  override def compare(lhs: Fitness, rhs: Fitness): Int = java.lang.Integer.compare(lhs, rhs)

object OneMax:
  trait Incremental extends OneMax, IncrementalFitnessFunction:
    self: IndividualType { type Individual <: Array[Boolean] } & PatchType { type ImmutablePatch <: IArray[Int] } =>

    override def computeFitnessFunctionIncrementally(individual: Individual, oldFitness: Fitness, patch: ImmutablePatch): Fitness =
      var newFitness = oldFitness
      Loops.loop(0, patch.length) { i =>
        val idx = patch(i)
        if individual(idx) then
          individual(idx) = false
          newFitness -= 1
        else
          individual(idx) = true
          newFitness += 1
      }
      newFitness
