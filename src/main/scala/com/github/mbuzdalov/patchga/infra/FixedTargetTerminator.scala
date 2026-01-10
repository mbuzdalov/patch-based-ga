package com.github.mbuzdalov.patchga.infra

import com.github.mbuzdalov.patchga.algorithm.Optimizer
import com.github.mbuzdalov.patchga.config.*

trait FixedTargetTerminator extends EvaluationLogger:
  self: IndividualType & FitnessType & FitnessComparator =>

  class TargetReached(val individual: Individual, val fitness: Fitness, val nEvaluations: Long) extends Exception

  private var nFitnessEvaluations: Long = 0
  def targetFitness: Fitness

  override def recordEvaluation(individual: Individual, fitness: Fitness): Unit =
    super.recordEvaluation(individual, fitness)
    nFitnessEvaluations += 1
    if compare(fitness, targetFitness) >= 0 then
      throw TargetReached(individual, fitness, nFitnessEvaluations)

object FixedTargetTerminator:
  def runUntilTargetReached(optimizer: Optimizer)(config: optimizer.RequiredConfig & FixedTargetTerminator): config.TargetReached =
    try optimizer.optimize(config) catch
      case e: config.TargetReached => e
