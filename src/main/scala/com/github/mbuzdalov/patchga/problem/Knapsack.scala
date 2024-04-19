package com.github.mbuzdalov.patchga.problem

import com.github.mbuzdalov.patchga.config.*
import com.github.mbuzdalov.patchga.util.Loops

trait Knapsack(val weights: IArray[Int], val values: IArray[Int], val capacity: Int)
  extends FitnessType, SimpleFitnessFunction, FitnessComparator:
  self: IndividualType { type Individual <: Array[Boolean] } =>

  override type Fitness = Knapsack.FitnessObject

  override def compare(lhs: Fitness, rhs: Fitness): Int =
    if lhs.isValid then
      if rhs.isValid then java.lang.Long.compare(lhs.sumValues, rhs.sumValues) else 1
    else
      if rhs.isValid then -1 else java.lang.Long.compare(rhs.sumWeights, lhs.sumWeights)

  override def computeFitness(ind: Individual): Fitness =
    var sumWeights, sumValues = 0L
    Loops.foreach(0, weights.length): i =>
      if ind(i) then
        sumWeights += weights(i)
        sumValues += values(i)
    Knapsack.FitnessObject(sumWeights, sumValues, sumWeights <= capacity)

object Knapsack:
  case class FitnessObject(sumWeights: Long, sumValues: Long, isValid: Boolean)

  trait Incremental extends Knapsack, IncrementalFitnessFunction:
    self: IndividualType { type Individual <: Array[Boolean] } & PatchType { type ImmutablePatch <: IArray[Int] } =>
    override def computeFitnessFunctionIncrementally(individual: Individual, oldFitness: Fitness,
                                                     patch: ImmutablePatch): Fitness =
      var sumWeights = oldFitness.sumWeights
      var sumValues = oldFitness.sumValues
      Loops.foreach(0, patch.length): i =>
        val idx = patch(i)
        if individual(idx) then
          individual(idx) = false
          sumWeights -= weights(idx)
          sumValues -= values(idx)
        else 
          individual(idx) = true
          sumWeights += weights(idx)
          sumValues += values(idx)
      FitnessObject(sumWeights, sumValues, sumWeights <= capacity)
