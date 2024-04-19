package com.github.mbuzdalov.patchga.population

import com.github.mbuzdalov.patchga.config.*

trait NaiveScratchPopulation extends Population:
  self: IndividualType & FitnessType & NewRandomIndividual & SimpleMutationOperator & SimpleCrossoverOperator & SimpleFitnessFunction =>

    class FitIndividual(val individual: Individual):
      val fitness: Fitness = computeFitness(individual)

    override type IndividualHandle = FitIndividual

    override def newRandomIndividualH(): IndividualHandle =
      new FitIndividual(newRandomIndividual())
    override def mutateH(handle: IndividualHandle, distance: Int): IndividualHandle =
      new FitIndividual(mutate(handle.individual, distance))
    override def crossoverH(mainParent: IndividualHandle, auxParent: IndividualHandle,
                            inDifferingBits: Int => Int, inSameBits: Int => Int): IndividualHandle =
      new FitIndividual(crossover(mainParent.individual, auxParent.individual, inDifferingBits, inSameBits))

    override def fitnessH(handle: IndividualHandle): Fitness = handle.fitness
    override def discardH(handle: IndividualHandle): Unit = ()
