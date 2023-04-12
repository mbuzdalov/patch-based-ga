package com.github.mbuzdalov.patchga.infra

import com.github.mbuzdalov.patchga.config.*

trait FixedTargetTerminator extends SimpleFitnessFunction:
  self: IndividualType & FitnessType & FitnessComparator =>

  class TargetReached(val individual: Individual, val fitness: Fitness, val nEvaluations: Long) extends Exception

  private var nFitnessEvaluations: Long = 0
  def targetFitness: Fitness

  private[FixedTargetTerminator] def validateFitness(ind: Individual, fitness: Fitness): Unit =
    nFitnessEvaluations += 1
    if compare(fitness, targetFitness) >= 0 then
      throw new TargetReached(ind, fitness, nFitnessEvaluations)

  abstract override def computeFitness(ind: Individual): Fitness =
    val result = super.computeFitness(ind)
    validateFitness(ind, result)
    result

object FixedTargetTerminator:
  trait Incremental extends FixedTargetTerminator, IncrementalFitnessFunction:
    self: IndividualType & FitnessType & PatchType & FitnessComparator =>
    abstract override def computeFitnessFunctionIncrementally(individual: Individual, oldFitness: Fitness, patch: ImmutablePatch): Fitness =
      val result = super.computeFitnessFunctionIncrementally(individual, oldFitness, patch)
      validateFitness(individual, result)
      result
