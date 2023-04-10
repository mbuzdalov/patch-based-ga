package com.github.mbuzdalov.patchga.representation

import scala.annotation.tailrec

import com.github.mbuzdalov.patchga.config.*

trait UnconstrainedBitString(size: Int) extends IndividualType, MaximumPatchSize, SimpleMutationOperator, SimpleCrossoverOperator, NewRandomIndividual:
  self: RandomProvider =>
    override type Individual = Array[Boolean]

    override def maximumPatchSize: Int = size

    override def mutate(individual: Individual, distance: Int): Individual =
      assert(size == individual.length)
      mutateImpl(individual.clone(), 0, distance)

    override def crossover(mainParent: Individual, auxParent: Individual, distanceToMainFunction: Int => Int): Individual =
      assert(size == mainParent.length)
      assert(size == auxParent.length)
      // First, count the number of differing bits between the parents
      var countDifferences = 0
      for i <- 0 until size do
        if mainParent(i) != auxParent(i) then countDifferences += 1

      // Second, iterate over the differing bits again and mutate them in the result as appropriately
      var remaining = distanceToMainFunction(countDifferences)
      val result = mainParent.clone()
      if remaining > 0 then
        var scanned = 0
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

    @tailrec
    private def mutateImpl(individual: Individual, current: Int, remaining: Int): Individual =
      if remaining == 1 then
        // Only one bit remains to be flipped. Can be done in a simple way.
        individual(current + random.nextInt(size - current)) ^= true
        individual
      else if size - current <= remaining then
        // All remaining bits need to be flipped. Can be done in a simple way too.
        for i <- current until size do individual(i) ^= true
        individual
      else if random.nextInt(size - current) < remaining then
        // With probability remaining / (size - current), the current bit needs to be flipped.
        individual(current) ^= true
        mutateImpl(individual, current + 1, remaining - 1)
      else
        // With the remaining probability, continue with the next bit.
        mutateImpl(individual, current + 1, remaining)
