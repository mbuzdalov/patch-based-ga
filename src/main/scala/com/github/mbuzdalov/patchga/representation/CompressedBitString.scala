package com.github.mbuzdalov.patchga.representation

import com.github.mbuzdalov.patchga.config.*
import com.github.mbuzdalov.patchga.util.Loops

import scala.annotation.tailrec

trait CompressedBitString(size: Int)
  extends IndividualType, SimpleMutationOperator, SimpleCrossoverOperator,
    IndividualDistance, NewRandomIndividual, MaximumPatchSize:
  self: RandomProvider =>

  override type Individual = Array[Long]
  private val buffer = new Array[Int](size)
  
  override def maximumPatchSize: Int = size
  
  override def copyOfIndividual(ind: Array[Long]): Array[Long] =
    ind.clone()
  
  override def mutate(individual: Array[Long], distance: Int): Array[Long] =
    val result = individual.clone()
    
    if distance * 2 < size
    then mutateImpl2(individual, result, distance)
    else mutateImpl1(result, 0, distance)
    
    result
  
  override def crossover(mainParent: Array[Long], auxParent: Array[Long], inDifferingBits: Int => Int, inSameBits: Int => Int): Array[Long] =
    buffer.synchronized:
      var diff = 0
      var same = 0
      var index = 0
      var xorV = 0L
      // writing out indices for differences
      Loops.foreach(0, size): i =>
        if (i & 63) == 0 then
          xorV = mainParent(i >>> 6) ^ auxParent(i >>> 6)
        if (xorV & 1) == 1 then
          buffer(diff) = i
          diff += 1
        else
          same += 1
          buffer(size - same) = i
        xorV >>>= 1
      
      val result = mainParent.clone()

      // flipping random subset of differing bits of requested size
      Loops.foreach(0, inDifferingBits(diff)): i =>
        val choice = random.nextInt(i, diff)
        val change = buffer(choice)
        buffer(choice) = buffer(i)
        result(change >>> 6) ^= 1L << change

      // flipping random subset of same bits of requested size
      Loops.foreach(0, inSameBits(same)): i =>
        val choice = random.nextInt(diff + i, size)
        val change = buffer(choice)
        buffer(choice) = buffer(diff + i)
        result(change >>> 6) ^= 1L << change
      result
  
  override def distance(lhs: Array[Long], rhs: Array[Long]): Int =
    var result = 0
    Loops.foreach(0, lhs.length)(i => result += java.lang.Long.bitCount(lhs(i) ^ rhs(i)))
    result
  
  override def newRandomIndividual(): Array[Long] =
    val length = (size + 63) >>> 6
    val result = Array.fill(length)(random.nextLong())
    if (size & 63) != 0 then result(length - 1) >>>= -size
    result
    
  @tailrec
  private def mutateImpl1(individual: Array[Long], current: Int, remaining: Int): Unit =
    if remaining == 1 then
      // Only one bit remains to be flipped. Can be done in a simple way.
      val index = current + random.nextInt(size - current)
      individual(index >>> 6) ^= 1L << index
    else if size - current <= remaining then
      // All remaining bits need to be flipped. Can be done in a simple way too.
      // Can be made faster but...
      Loops.foreach(current, size)(i => individual(i >>> 6) ^= 1L << i)
    else if random.nextInt(size - current) < remaining then
      // With probability remaining / (size - current), the current bit needs to be flipped.
      individual(current >>> 6) ^= 1L << current
      mutateImpl1(individual, current + 1, remaining - 1)
    else
      // With the remaining probability, continue with the next bit.
      mutateImpl1(individual, current + 1, remaining)

  private def mutateImpl2(original: Array[Long], mutated: Array[Long], nFlip: Int): Unit =
    var remaining = nFlip
    while remaining > 0 do
      val index = random.nextInt(size)
      val off = index >>> 6
      if (((original(off) ^ mutated(off)) >>> index) & 1) == 0 then
        remaining -= 1
        mutated(off) ^= 1L << index
