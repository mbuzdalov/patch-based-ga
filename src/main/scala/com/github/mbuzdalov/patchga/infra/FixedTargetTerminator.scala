package com.github.mbuzdalov.patchga.infra

import com.github.mbuzdalov.patchga.algorithm.Optimizer
import com.github.mbuzdalov.patchga.config.*

trait FixedTargetTerminator extends SimpleFitnessFunction:
  self: IndividualType & FitnessType & FitnessComparator =>

  class TargetReached(val individual: Individual, val fitness: Fitness, val nEvaluations: Long) extends Exception

  private var nFitnessEvaluations: Long = 0
  def targetFitness: Fitness

  private[FixedTargetTerminator] def validateFitness(ind: Individual, fitness: Fitness): Fitness =
    nFitnessEvaluations += 1
    if compare(fitness, targetFitness) >= 0 then
      throw new TargetReached(ind, fitness, nFitnessEvaluations)
    fitness  

  abstract override def computeFitness(ind: Individual): Fitness = validateFitness(ind, super.computeFitness(ind))

object FixedTargetTerminator:
  trait Incremental extends FixedTargetTerminator, IncrementalFitnessFunction:
    self: IndividualType & FitnessType & PatchType & FitnessComparator =>
    abstract override def computeFitnessFunctionIncrementally(individual: Individual, oldFitness: Fitness, patch: ImmutablePatch): Fitness =
      validateFitness(individual, super.computeFitnessFunctionIncrementally(individual, oldFitness, patch))

  def runUntilTargetReached(optimizer: Optimizer)(config: optimizer.RequiredConfig & FixedTargetTerminator): config.TargetReached =
    try
      optimizer.optimize(config)
    catch
      case e: config.TargetReached => e
