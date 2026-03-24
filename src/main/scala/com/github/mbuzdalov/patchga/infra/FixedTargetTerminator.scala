package com.github.mbuzdalov.patchga.infra

import com.github.mbuzdalov.patchga.algorithm.Optimizer
import com.github.mbuzdalov.patchga.config.*

object FixedTargetTerminator:
  class TargetReached[Fitness](val fitness: Fitness, val nEvaluations: Long)
  
  def runUntilTargetReached(optimizer: Optimizer, 
                            config: optimizer.RequiredConfig & EvaluationLogger & IndividualType & FitnessType & FitnessComparator,
                            targetFitness: config.Fitness,
                            nTargetHitsRequired: Int = 1): TargetReached[config.Fitness] =
    var nFitnessEvaluations: Long = 0
    var bestFitness: Option[config.Fitness] = None
    val TargetReachedEx = RuntimeException("Target Reached")
    var nTargetHits = 0
    
    config.addEvaluationListener: (ind, fitness) =>
      nFitnessEvaluations += 1
      if config.compare(fitness, targetFitness) >= 0 then
        bestFitness = Some(fitness)
        nTargetHits += 1
        if nTargetHits >= nTargetHitsRequired then throw TargetReachedEx
    
    try optimizer.optimize(config) catch
      case TargetReachedEx => TargetReached(bestFitness.get, nFitnessEvaluations)
