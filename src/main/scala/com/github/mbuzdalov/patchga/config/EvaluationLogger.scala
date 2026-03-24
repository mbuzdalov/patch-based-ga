package com.github.mbuzdalov.patchga.config

import scala.collection.mutable.ArrayBuffer

trait EvaluationLogger:
  self: IndividualType & FitnessType =>

  private val listeners = ArrayBuffer[(Individual, Fitness) => Unit]()

  def addEvaluationListener(listener: (Individual, Fitness) => Unit): Unit =
    listeners.addOne(listener)
  
  protected final def recordEvaluation(individual: Individual, fitness: Fitness): Unit =
    listeners.foreach(_(individual, fitness))
