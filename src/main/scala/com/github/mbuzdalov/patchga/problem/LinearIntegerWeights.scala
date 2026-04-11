package com.github.mbuzdalov.patchga.problem

import com.github.mbuzdalov.patchga.config.*
import com.github.mbuzdalov.patchga.util.Loops

import java.util.Random

trait LinearIntegerWeights(weightCounts: IArray[Int], weightSeed: Long) extends FitnessType, SimpleFitnessFunction, FitnessComparator:
  self: IndividualType { type Individual <: Array[Boolean] } & MaximumPatchSize =>
  override type Fitness = Long

  protected val weights: IArray[Int] = locally:
    val weightRandom = new Random(weightSeed)
    val weightsTemp = new Array[Int](weightCounts.sum)
    // generate all weights in sequence
    var prefix = 0
    Loops.foreach(0, weightCounts.length): w =>
      java.util.Arrays.fill(weightsTemp, prefix, prefix + weightCounts(w), w)
      prefix += weightCounts(w)
    // shuffle the weights  
    Loops.foreach(1, weightsTemp.length): i =>
      val j = weightRandom.nextInt(i + 1)
      val tmp = weightsTemp(i)
      weightsTemp(i) = weightsTemp(j)
      weightsTemp(j) = tmp
    // store then in an immutable array  
    IArray.unsafeFromArray(weightsTemp) 

  protected val sumWeights: Long = locally:
    var result = 0L
    Loops.foreach(0, weights.length)(i => result += weights(i))
    result
  
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
