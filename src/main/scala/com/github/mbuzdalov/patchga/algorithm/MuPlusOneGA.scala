package com.github.mbuzdalov.patchga.algorithm

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

import com.github.mbuzdalov.patchga.config.*
import com.github.mbuzdalov.patchga.distribution.{BinomialDistribution, IntegerDistribution}

class MuPlusOneGA(populationSize: Int, pCrossover: Double, mutationDistributionSource: Int => IntegerDistribution) extends Optimizer:
  type RequiredConfig = FitnessType & PatchSizeType & Population & IntegralPatchSize & MaximumPatchSize & FitnessComparator & RandomProvider

  override def optimize(config: RequiredConfig): Nothing =
    import config._

    // Preparation
    val mutationOperator = mutationDistributionSource(maximumPatchSize)

    // Population initialization
    val population = new ArrayBuffer[IndividualHandle](populationSize)
    for _ <- 0 until populationSize do
      population.addOne(newRandomIndividualH())

    val smallestFitnessIndices = new Array[Int](populationSize)
    var nSmallestIndividuals = 1
    var smallestFitness = fitnessH(population(0))

    def populateSmallest(): Unit =
      nSmallestIndividuals = 1
      smallestFitnessIndices(0) = 0
      smallestFitness = fitnessH(population(0))

      for i <- 1 until populationSize do
        val currFitness = fitnessH(population(i))
        if compare(smallestFitness, currFitness) > 0 then
          smallestFitness = currFitness
          nSmallestIndividuals = 0
        if compare(smallestFitness, currFitness) == 0 then
          smallestFitnessIndices(nSmallestIndividuals) = i
          nSmallestIndividuals += 1

    populateSmallest()

    @tailrec
    def go(): Nothing =
      val next = if random.nextDouble() < pCrossover then
        // Crossover and mutation
        val i1, i2 = random.nextInt(populationSize)
        val crossoverResult = crossoverH(population(i1), population(i2), d => BinomialDistribution(d, 0.5).sample(random))
        val mutationResult = mutateH(crossoverResult, mutationOperator.sample(random))
        discardH(crossoverResult)
        mutationResult
      else
        // Mutation only
        val idx = random.nextInt(populationSize)
        mutateH(population(idx), mutationOperator.sample(random))

      assert(nSmallestIndividuals >= 1)
      val comparison = compare(smallestFitness, fitnessH(next))
      if comparison > 0 then
        // the new offspring is worse than anyone and has to die
        discardH(next)
      else if comparison < 0 then
        // a random of the smallest ones is to die
        val toDie = random.nextInt(nSmallestIndividuals)
        val indexToDie = smallestFitnessIndices(toDie)
        discardH(population(indexToDie))
        population(indexToDie) = next
        nSmallestIndividuals -= 1
        smallestFitnessIndices(toDie) = smallestFitnessIndices(nSmallestIndividuals)
        if nSmallestIndividuals == 0 then populateSmallest()
      else
        val toDie = random.nextInt(nSmallestIndividuals + 1)
        if toDie == nSmallestIndividuals then
          // the new offspring dies
          discardH(next)
        else
          val indexToDie = smallestFitnessIndices(toDie)
          discardH(population(indexToDie))
          population(indexToDie) = next
      go()
    go()
