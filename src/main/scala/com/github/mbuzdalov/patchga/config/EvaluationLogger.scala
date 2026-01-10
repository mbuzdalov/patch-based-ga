package com.github.mbuzdalov.patchga.config

import scala.collection.mutable.ArrayBuffer

trait EvaluationLogger:
  self: IndividualType & FitnessType =>

  private val allFitnessValues = ArrayBuffer[Fitness]()
  
  final def fitnessValuesInOrder: IndexedSeq[Fitness] = allFitnessValues.toIndexedSeq
  def recordEvaluation(individual: Individual, fitness: Fitness): Unit =
    allFitnessValues.addOne(fitness)
