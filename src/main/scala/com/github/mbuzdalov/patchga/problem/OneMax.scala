package com.github.mbuzdalov.patchga.problem

import com.github.mbuzdalov.patchga.config.*
import com.github.mbuzdalov.patchga.util.Loops

object OneMax:
  trait BasicArray extends FitnessType, SimpleFitnessFunction, FitnessComparator:
    self: IndividualType { type Individual <: Array[Boolean] } =>
    override type Fitness = Int
    override def compare(lhs: Fitness, rhs: Fitness): Int = java.lang.Integer.compare(lhs, rhs)
    override def computeFitness(ind: Individual): Fitness =
      Loops.count(0, ind.length)(i => ind(i))
  
  trait BasicArrayIncremental extends BasicArray, IncrementalFitnessFunction:
    self: IndividualType { type Individual <: Array[Boolean] } & PatchType { type ImmutablePatch <: IArray[Int] } =>

    override def computeFitnessFunctionIncrementally(individual: Individual, oldFitness: Fitness, patch: ImmutablePatch): Fitness =
      var newFitness = oldFitness
      Loops.foreach(0, patch.length): i =>
        val idx = patch(i)
        if individual(idx) then
          individual(idx) = false
          newFitness -= 1
        else
          individual(idx) = true
          newFitness += 1
      newFitness
  
  trait Compressed extends FitnessType, SimpleFitnessFunction, FitnessComparator:
    self: IndividualType { type Individual <: Array[Long] } =>
    override type Fitness = Int
    override def compare(lhs: Fitness, rhs: Fitness): Int = java.lang.Integer.compare(lhs, rhs)
    override def computeFitness(ind: Individual): Fitness =
      Loops.fold(0, ind.length, 0)(_ + _)(i => java.lang.Long.bitCount(ind(i)))
