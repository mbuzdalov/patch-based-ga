package com.github.mbuzdalov.patchga.problem

import com.github.mbuzdalov.patchga.config.*
import com.github.mbuzdalov.patchga.util.Loops

object OneMax:
  trait BasicArray extends FitnessType, SimpleFitnessFunction, FitnessComparator:
    self: IndividualType { type Individual <: Array[Boolean] } =>
    override type Fitness = Int
    
    override def computeFitness(ind: Individual): Fitness =
      var result = 0
      Loops.foreach(0, ind.length)(i => if ind(i) then result += 1)
      result
    
    override def compare(lhs: Fitness, rhs: Fitness): Int = java.lang.Integer.compare(lhs, rhs)
  
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
    
    override def computeFitness(ind: Individual): Fitness =
      var result = 0
      Loops.foreach(0, ind.length)(i => result += java.lang.Long.bitCount(ind(i)))
      result
    
    override def compare(lhs: Fitness, rhs: Fitness): Int = java.lang.Integer.compare(lhs, rhs)
