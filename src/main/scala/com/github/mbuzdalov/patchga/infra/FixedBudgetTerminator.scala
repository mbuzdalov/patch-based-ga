package com.github.mbuzdalov.patchga.infra

import com.github.mbuzdalov.patchga.algorithm.Optimizer
import com.github.mbuzdalov.patchga.config.*

trait FixedBudgetTerminator(val maxEvaluations: Long) extends EvaluationLogger:
  self: IndividualType & FitnessType & FitnessComparator =>

  class BudgetReached(val individual: Individual, val fitness: Fitness, val nEvaluations: Long) extends Exception

  private var nFitnessEvaluations: Long = 0
  private var bestIndividual: Individual = scala.compiletime.uninitialized
  private var bestFitness: Fitness = scala.compiletime.uninitialized

  override def recordEvaluation(individual: Individual, fitness: Fitness): Unit =
    super.recordEvaluation(individual, fitness)
    if nFitnessEvaluations == 0 || compare(fitness, bestFitness) > 0 then
      bestIndividual = copyOfIndividual(individual)
      bestFitness = fitness
      
    nFitnessEvaluations += 1
    if nFitnessEvaluations >= maxEvaluations then
      throw new BudgetReached(bestIndividual, bestFitness, nFitnessEvaluations)

object FixedBudgetTerminator:
  def runUntilBudgetReached(optimizer: Optimizer)(config: optimizer.RequiredConfig & FixedBudgetTerminator): config.BudgetReached =
    try optimizer.optimize(config) catch
      case e: config.BudgetReached => e  
