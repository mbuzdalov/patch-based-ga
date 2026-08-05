package com.github.mbuzdalov.patchga.infra

import com.github.mbuzdalov.patchga.algorithm.Optimizer

object CheapFixedBudgetTerminator:
  def runUntilBudgetReached(optimizer: Optimizer,
                            config: Optimizer.Config,
                            maxEvaluations: Long): config.Fitness =
    var nFitnessEvaluations: Long = 0
    var bestFitness: Option[config.Fitness] = None
    val BudgetReached = RuntimeException("Budget Reached")
    
    config.addEvaluationListener: (ind, fitness, handle) =>
      if nFitnessEvaluations == 0 || config.compare(fitness, bestFitness.get) > 0 then
        bestFitness = Some(fitness)
      nFitnessEvaluations += 1
      if nFitnessEvaluations >= maxEvaluations then throw BudgetReached

    try optimizer.optimize(config) catch
      case BudgetReached => bestFitness.get
    