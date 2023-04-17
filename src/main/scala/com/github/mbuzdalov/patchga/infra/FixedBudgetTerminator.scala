package com.github.mbuzdalov.patchga.infra

import com.github.mbuzdalov.patchga.algorithm.Optimizer
import com.github.mbuzdalov.patchga.config.{FitnessComparator, FitnessType, IncrementalFitnessFunction, IndividualType, PatchType, SimpleFitnessFunction}

trait FixedBudgetTerminator(val maxEvaluations: Long) extends SimpleFitnessFunction:
  self: IndividualType & FitnessType & FitnessComparator =>

  class BudgetReached(val individual: Individual, val fitness: Fitness, val nEvaluations: Long) extends Exception

  private var nFitnessEvaluations: Long = 0
  private var bestIndividual: Individual = scala.compiletime.uninitialized
  private var bestFitness: Fitness = scala.compiletime.uninitialized

  private[FixedBudgetTerminator] def validate(ind: Individual, fitness: Fitness): Unit =
    if nFitnessEvaluations == 0 || compare(fitness, bestFitness) > 0 then
      bestIndividual = copyOfIndividual(ind)
      bestFitness = fitness
      
    nFitnessEvaluations += 1
    if nFitnessEvaluations >= maxEvaluations then
      throw new BudgetReached(bestIndividual, bestFitness, nFitnessEvaluations)

  abstract override def computeFitness(ind: Individual): Fitness =
    val result = super.computeFitness(ind)
    validate(ind, result)
    result

object FixedBudgetTerminator:
  trait Incremental extends FixedBudgetTerminator, IncrementalFitnessFunction:
    self: IndividualType & FitnessType & PatchType & FitnessComparator =>
    abstract override def computeFitnessFunctionIncrementally(individual: Individual, oldFitness: Fitness,
                                                              patch: ImmutablePatch): Fitness =
      val result = super.computeFitnessFunctionIncrementally(individual, oldFitness, patch)
      validate(individual, result)
      result

  def runUntilBudgetReached(optimizer: Optimizer)(config: optimizer.RequiredConfig & FixedBudgetTerminator): config.BudgetReached =
    try
      optimizer.optimize(config)
    catch
      case e: config.BudgetReached => e  
      