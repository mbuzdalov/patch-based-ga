package com.github.mbuzdalov.patchga.representation

import scala.annotation.tailrec

import com.github.mbuzdalov.patchga.config.*
import com.github.mbuzdalov.patchga.util.{Loops, MutableIntSet}

trait UnconstrainedBitString(size: Int)
  extends IndividualType, MaximumPatchSize, PatchType, SimpleMutationOperator, SimpleCrossoverOperator,
    IndividualDistance, NewRandomIndividual:
  self: RandomProvider =>
  override type Individual = Array[Boolean]
  override type MutablePatch = MutableIntSet
  override type ImmutablePatch = IArray[Int]

  override def maximumPatchSize: Int = size

  override def copyOfIndividual(ind: Individual): Individual = ind.clone()
  
  override def distance(lhs: Individual, rhs: Individual): Int =
    Loops.count(0, lhs.length)(i => lhs(i) != rhs(i))
  
  override def mutate(individual: Individual, distance: Int): Individual =
    assert(size == individual.length)
    mutateImpl(individual.clone(), 0, distance)

  override def crossover(mainParent: Individual, auxParent: Individual, 
                         inDifferingBits: Int => Int, inSameBits: Int => Int): Individual =
    assert(size == mainParent.length)
    assert(size == auxParent.length)
    // First, count the number of differing bits between the parents
    var countDifferences = 0
    Loops.foreach(0, size): i =>
      if mainParent(i) != auxParent(i) then countDifferences += 1

    // Second, iterate over the differing bits again and mutate them in the result as appropriately
    var remainingInDiff = inDifferingBits(countDifferences)
    var remainingInSame = inSameBits(size - countDifferences)
    val result = mainParent.clone()
    if remainingInDiff > 0 || remainingInSame > 0 then
      var scannedDiff, scannedSame = 0
      Loops.foreach(0, size): i =>
        if mainParent(i) != auxParent(i) then
          if remainingInDiff > 0 && random.nextInt(countDifferences - scannedDiff) < remainingInDiff then
            result(i) ^= true
            remainingInDiff -= 1
          scannedDiff += 1
        else
          if remainingInSame > 0 && random.nextInt(size - countDifferences - scannedSame) < remainingInSame then
            result(i) ^= true
            remainingInSame -= 1
          scannedSame += 1

    // Note that if distanceToMain is greater than the number of differing bits, we flip all of them
    result

  override def newRandomIndividual(): Individual =
    Array.fill(size)(random.nextBoolean())

  override def createImmutableVersion(patch: MutableIntSet): IArray[Int] = patch.toIArray

  override def immutablePatchSize(patch: ImmutablePatch): Int = patch.length

  override def reversedImmutablePatch(patch: IArray[Int]): IArray[Int] = patch

  override def createMutablePatch(): MutableIntSet = new MutableIntSet(size)

  override def appendToMutablePatch(patch: MutableIntSet, toAppend: IArray[Int]): Unit =
    Loops.foreach(0, toAppend.length): i =>
      val idx = toAppend(i)
      if patch.contains(idx) then
        patch.remove(idx)
      else
        patch.add(idx)

  override def prependToMutablePatch(patch: MutableIntSet, toPrepend: IArray[Int]): Unit =
    appendToMutablePatch(patch, toPrepend)
  
  override def clearMutablePatch(patch: MutableIntSet): Unit = patch.clear()

  override def mutablePatchSize(patch: MutableIntSet): Int = patch.size

  override def applyToIndividual(individual: Array[Boolean], patch: IArray[Int]): Unit =
    Loops.foreach(0, patch.length)(i => individual(patch(i)) ^= true)

  override def initializeMutablePatchFromDistance(patch: MutableIntSet, distance: Int): Unit =
    patch.clear()
    Loops.repeat(distance)(patch.add(patch.sampleElementNotInSet(random)))

  override def initializeMutablePatchFromTwoIndividuals(patch: MutableIntSet, source: Array[Boolean], target: Array[Boolean]): Unit =
    patch.clear()
    Loops.foreach(0, size)(i => if source(i) != target(i) then patch.add(i))

  override def applyCrossoverRequest(patch: MutablePatch, nRemove: Int, nAdd: Int): Unit =
    patch.groupAddRemove(nRemove, nAdd, random)

  @tailrec
  private def mutateImpl(individual: Individual, current: Int, remaining: Int): Individual =
    if remaining == 1 then
      // Only one bit remains to be flipped. Can be done in a simple way.
      individual(current + random.nextInt(size - current)) ^= true
      individual
    else if size - current <= remaining then
      // All remaining bits need to be flipped. Can be done in a simple way too.
      Loops.foreach(current, size)(i => individual(i) ^= true)
      individual
    else if random.nextInt(size - current) < remaining then
      // With probability remaining / (size - current), the current bit needs to be flipped.
      individual(current) ^= true
      mutateImpl(individual, current + 1, remaining - 1)
    else
      // With the remaining probability, continue with the next bit.
      mutateImpl(individual, current + 1, remaining)
