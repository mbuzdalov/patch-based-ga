package com.github.mbuzdalov.patchga.population

import com.github.mbuzdalov.patchga.config.*

import scala.collection.mutable.ArrayBuffer

trait NaiveScratchPopulation(allowDuplicates: Boolean, disableDiscard: Boolean) extends Population:
  self: IndividualType & FitnessType & NewRandomIndividual & IndividualDistance
    & SimpleMutationOperator & SimpleCrossoverOperator & SimpleFitnessFunction =>

  class FitIndividual(val individual: Individual) extends WithReferenceCount:
    var referenceCount: Int = 1
    val fitness: Fitness = computeFitness(individual)

  private val allIndividuals = new scala.collection.mutable.HashSet[FitIndividual]()
  private def handleFor(ind: Individual): FitIndividual =
    if allowDuplicates then
      val result = FitIndividual(ind)
      allIndividuals.addOne(result)
      result
    else allIndividuals.find(h => distance(h.individual, ind) == 0) match
      case Some(existing) =>
        existing.referenceCount += 1
        existing
      case None =>
        val result = FitIndividual(ind)
        allIndividuals.addOne(result)
        result

  override type IndividualHandle = FitIndividual

  override def newRandomIndividualH(): IndividualHandle =
    handleFor(newRandomIndividual())
  override def mutateH(handle: IndividualHandle, distance: Int): IndividualHandle =
    handleFor(mutate(handle.individual, distance))
  override def crossoverH(mainParent: IndividualHandle, auxParent: IndividualHandle,
                          inDifferingBits: Int => Int, inSameBits: Int => Int): IndividualHandle =
    handleFor(crossover(mainParent.individual, auxParent.individual, inDifferingBits, inSameBits))

  override def fitnessH(handle: IndividualHandle): Fitness = handle.fitness
  override def discardH(handle: IndividualHandle): Unit = if !disableDiscard then
    handle.referenceCount -= 1
    if handle.referenceCount == 0 then
      allIndividuals.remove(handle)
  
  override def collectDistanceToHandles(base: IndividualHandle, consumer: (IndividualHandle, Int) => Unit): Unit =
    for ind <- allIndividuals do
      consumer(ind, distance(base.individual, ind.individual))
  
  override def collectHandlesAtDistance(base: IndividualHandle, distancePredicate: Int => Boolean, buffer: ArrayBuffer[IndividualHandle]): Unit =
    buffer.clear()
    for ind <- allIndividuals do
      if distancePredicate(distance(base.individual, ind.individual))
        then buffer.addOne(ind)
