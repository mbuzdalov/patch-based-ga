package com.github.mbuzdalov.patchga.config

import scala.collection.mutable.ArrayBuffer

trait Population extends EvaluationLogger:
  self: IndividualType & FitnessType =>

  type IndividualHandle <: WithReferenceCount

  def newRandomIndividualH(): IndividualHandle
  def mutateH(handle: IndividualHandle, distance: Int): IndividualHandle
  def crossoverH(mainParent: IndividualHandle, auxParent: IndividualHandle, 
                 inDifferingBits: Int => Int, inSameBits: Int => Int): IndividualHandle
  def fitnessH(handle: IndividualHandle): Fitness
  def discardH(handle: IndividualHandle): Unit

  def collectDistanceToHandles(base: IndividualHandle, consumer: (IndividualHandle, Int) => Unit): Unit
  def collectHandlesAtDistance(base: IndividualHandle, distancePredicate: Int => Boolean, buffer: ArrayBuffer[IndividualHandle]): Unit
