package com.github.mbuzdalov.patchga.representation

import com.github.mbuzdalov.patchga.config._

trait UnconstrainedBitString(size: Int) extends IndividualType, PatchSizeType, IntegralPatchSize, SimpleMutationOperator, SimpleCrossoverOperator, NewRandomIndividual:
  self: RandomProvider =>
    override type Individual = Array[Boolean]
    override type PatchSize = Int

    override def fromInt(distance: Int): PatchSize = distance

    override def mutate(individual: Individual, distance: PatchSize): Individual =
      assert(size == individual.length)
      val result = individual.clone()
      var remaining = distance
      for i <- 0 until size do
        if random.nextInt(size - i) < remaining then
          result(i) ^= true
          remaining -= 1
      result

    override def crossover(mainParent: Individual, auxParent: Individual, distanceToMain: PatchSize): Individual =
      assert(size == mainParent.length)
      assert(size == auxParent.length)
      // First, count the number of differing bits between the parents  
      var countDifferences = 0
      for i <- 0 until size do
        if mainParent(i) != auxParent(i) then countDifferences += 1

      // Second, iterate over the differing bits again and mutate them in the result as appropriately  
      var remaining = distanceToMain
      var scanned = 0
      val result = mainParent.clone()
      for i <- 0 until size do 
        if mainParent(i) != auxParent(i) then
          if random.nextInt(countDifferences - scanned) < remaining then
            result(i) ^= true
            remaining -= 1
          scanned += 1

      // Note that if distanceToMain is greater than the number of differing bits, we flip all of them     
      result    

    override def newRandomIndividual(): Individual =
      Array.fill(size)(random.nextBoolean())
