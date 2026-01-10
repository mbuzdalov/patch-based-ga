package com.github.mbuzdalov.patchga.algorithm

import com.github.mbuzdalov.patchga.config.*
import com.github.mbuzdalov.patchga.distribution.PowerLawDistribution
import com.github.mbuzdalov.patchga.util.Loops

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class NeverForgettingGA(firstParentSelectionBeta: Double,
                        mutationDistanceBeta: Double,
                        crossoverProbability: Double,
                        crossoverParentMinimumDistanceBeta: Double,
                        secondParentSelectionBeta: Double,
                        crossoverDistanceBeta: Double) extends Optimizer:
  type RequiredConfig = FitnessType & Population & MaximumPatchSize & FitnessComparator & RandomProvider
  
  override def optimize(config: RequiredConfig): Nothing =
    import config.*
    
    val crossoverSecondParentBuffer = new ArrayBuffer[IndividualHandle]()
    val distanceBuffer = new ArrayBuffer[Int]()
    val distanceSeen = new Array[Boolean](maximumPatchSize + 1)
    val inverseFitnessOrdering = Ordering.by(fitnessH).reverse
    val nodesSorted = new ArrayBuffer[IndividualHandle]()
    nodesSorted.addOne(newRandomIndividualH())

    def insertionSortAdd(h: IndividualHandle): Unit =
      nodesSorted.addOne(h)
      var idx = nodesSorted.size - 2
      while idx >= 0 && inverseFitnessOrdering.gt(nodesSorted(idx), h) do
        nodesSorted(idx + 1) = nodesSorted(idx)
        nodesSorted(idx) = h
        idx -= 1
    
    def sampleParent(source: ArrayBuffer[IndividualHandle], beta: Double): IndividualHandle =
      val index0 = PowerLawDistribution.sample(source.size, beta, random) - 1
      val fitness0 = fitnessH(source(index0))
      var indexLo = index0
      while indexLo > 0 && compare(fitnessH(source(indexLo - 1)), fitness0) == 0 do indexLo -= 1
      var indexHi = index0
      while indexHi + 1 < source.size && compare(fitnessH(source(indexHi + 1)), fitness0) == 0 do indexHi += 1
      val index = random.nextInt(indexLo, indexHi + 1)
      source(index)
    
    def sampleFirstParent(): IndividualHandle = sampleParent(nodesSorted, firstParentSelectionBeta)
    def sampleSecondParent(): IndividualHandle = sampleParent(crossoverSecondParentBuffer, secondParentSelectionBeta)
    
    @tailrec
    def sampleFirstParentWithDistantEnoughNeighbors(): IndividualHandle =
      val parent = sampleFirstParent()
      distanceBuffer.clear()
      collectDistanceToHandles(parent, (_, d) => if d > 1 && !distanceSeen(d) then
        distanceSeen(d) = true
        distanceBuffer.addOne(d))
      distanceBuffer.foreach(v => distanceSeen(v) = false)
      if distanceBuffer.isEmpty then sampleFirstParentWithDistantEnoughNeighbors() else parent
    
    Loops.forever:
      // with three different nodes, at least two of them have distance > 1.
      val nextNode = if nodesSorted.size >= 3 && random.nextDouble() < crossoverProbability then
        // crossover
        // sample a parent such that it has individuals at distance > 1
        val firstParent = sampleFirstParentWithDistantEnoughNeighbors()
        // sample a distance out of the valid ones
        distanceBuffer.sortInPlace()
        val secondParentDistance = distanceBuffer(PowerLawDistribution.sample(distanceBuffer.size, crossoverParentMinimumDistanceBeta, random) - 1)
        // collect individuals at distance which is at least as much as the found distance, and sample one
        collectHandlesAtDistance(firstParent, _ >= secondParentDistance, crossoverSecondParentBuffer)
        crossoverSecondParentBuffer.sortInPlace()(using inverseFitnessOrdering)
        // sample crossover distance and perform crossover
        val secondParent = sampleSecondParent()
        val crossoverDistance = PowerLawDistribution.sample(secondParentDistance - 1, crossoverDistanceBeta, random)
        if random.nextBoolean()
        then crossoverH(firstParent, secondParent, _ => crossoverDistance, _ => 0)
        else crossoverH(secondParent, firstParent, _ => crossoverDistance, _ => 0)
      else
        // mutation
        val change = PowerLawDistribution.sample(maximumPatchSize, mutationDistanceBeta, random)
        mutateH(sampleFirstParent(), change)
      end nextNode
      
      if nextNode.referenceCount == 1 then insertionSortAdd(nextNode)
