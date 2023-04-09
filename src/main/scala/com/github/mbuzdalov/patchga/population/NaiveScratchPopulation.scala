package com.github.mbuzdalov.patchga.population

import com.github.mbuzdalov.patchga.config._

trait NaiveScratchPopulation extends Population:
  self: IndividualType & FitnessType & PatchSizeType & NewRandomIndividual & SimpleMutationOperator & SimpleCrossoverOperator & SimpleFitnessFunction =>

    class LazyFitIndividual(val individual: Individual):
      lazy val fitness: Fitness = computeFitness(individual)

    override type IndividualHandle = LazyFitIndividual

    override def newRandomIndividualH(): IndividualHandle =
      new LazyFitIndividual(newRandomIndividual())
    override def mutateH(handle: IndividualHandle, distance: PatchSize): IndividualHandle =
      new LazyFitIndividual(mutate(handle.individual, distance))
    override def crossoverH(mainParent: IndividualHandle, auxParent: IndividualHandle, distanceToMain: PatchSize): IndividualHandle =
      new LazyFitIndividual(crossover(mainParent.individual, auxParent.individual, distanceToMain))

    override def fitnessH(handle: IndividualHandle): Fitness =
      handle.fitness

    override def discardH(handle: IndividualHandle): Unit = ()
