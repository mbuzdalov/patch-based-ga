package com.github.mbuzdalov.patchga.config

import scala.collection.mutable.ArrayBuffer

trait Population:
  self: IndividualType & FitnessType =>

  type IndividualHandle <: IndividualHandleProto[IndividualHandle]

  def newRandomIndividualH(): IndividualHandle
  def mutateH(handle: IndividualHandle, distance: Int): IndividualHandle
  def crossoverH(mainParent: IndividualHandle, auxParent: IndividualHandle, 
                 inDifferingBits: Int => Int, inSameBits: Int => Int): IndividualHandle
  def fitnessH(handle: IndividualHandle): Fitness
  def discardH(handle: IndividualHandle): Unit

  def collectDistanceToHandles(base: IndividualHandle)(consumer: (IndividualHandle, Int) => Unit): Unit
  def collectHandlesAtDistance(base: IndividualHandle, distancePredicate: Int => Boolean, buffer: ArrayBuffer[IndividualHandle]): Unit

  private val listeners = ArrayBuffer[(Individual, Fitness, IndividualHandle) => Unit]()
  
  def addEvaluationListener(listener: (Individual, Fitness, IndividualHandle) => Unit): Unit =
    listeners.addOne(listener)
  
  protected final def recordEvaluation(individual: Individual, fitness: Fitness, handle: IndividualHandle): Unit =
    listeners.foreach(_(individual, fitness, handle))
