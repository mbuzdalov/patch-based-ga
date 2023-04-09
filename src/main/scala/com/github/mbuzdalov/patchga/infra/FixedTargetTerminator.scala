package com.github.mbuzdalov.patchga.infra

import com.github.mbuzdalov.patchga.config._

trait FixedTargetTerminator extends SimpleFitnessFunction:
  self: IndividualType & FitnessType & FitnessComparator =>

    class TargetReached(val individual: Individual, val fitness: Fitness, val nEvaluations: Long) extends Exception

    private var nFitnessEvaluations: Long = 0
    def targetFitness: Fitness

    abstract override def computeFitness(ind: Individual): Fitness =
      val result = super.computeFitness(ind)
      nFitnessEvaluations += 1
      if compare(result, targetFitness) >= 0 then
        throw new TargetReached(ind, result, nFitnessEvaluations)
      result
