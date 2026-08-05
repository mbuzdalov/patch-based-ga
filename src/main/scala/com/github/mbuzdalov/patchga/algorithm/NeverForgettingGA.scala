package com.github.mbuzdalov.patchga.algorithm

import com.github.mbuzdalov.patchga.distribution.{IntegerDistribution, PowerLawDistribution}
import com.github.mbuzdalov.patchga.util.Loops

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class NeverForgettingGA(mutationParentSelectionBeta: Double,
                        mutationDistanceBeta: Double,
                        firstParentSelectionBeta: Double,
                        crossoverProbability: Double,
                        crossoverParentMinimumDistanceBeta: Double,
                        crossoverParentMaximumDistance: Option[Int => Int],
                        secondParentSelectionBeta: Double,
                        crossoverDistanceSource: Int => IntegerDistribution) extends Optimizer:
  override def optimize(config: Optimizer.Config): Nothing =
    import config.*

    val crossoverParentDistanceCap = crossoverParentMaximumDistance.map(f => f(maximumPatchSize))
    crossoverParentDistanceCap.foreach(d => require(d >= 2, "Maximum distance cannot be smaller than 2"))
    
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
    
    def crossoverDistanceOK(d: Int): Boolean = crossoverParentDistanceCap match
      case None => 2 <= d
      case Some(dMax) => 2 <= d && d <= dMax
    
    @tailrec
    def sampleFirstParentWithDistantEnoughNeighbors(nRemaining: Int): Option[IndividualHandle] =
      val parent = sampleParent(nodesSorted, firstParentSelectionBeta)
      distanceBuffer.clear()
      collectDistanceToHandles(parent): (_, d) => 
        if crossoverDistanceOK(d) && !distanceSeen(d) then
          distanceSeen(d) = true
          distanceBuffer.addOne(d)
      distanceBuffer.foreach(v => distanceSeen(v) = false)
      if distanceBuffer.nonEmpty then Some(parent)
      else if nRemaining == 0 then None
      else sampleFirstParentWithDistantEnoughNeighbors(nRemaining - 1)
    
    Loops.forever:
      // If crossover is to be invoked, there is a possibility that finding a parent fails.
      //
      // In the past, the presence of a possible parent was found deterministically.
      // As we started playing with parent selection using funny distributions, this becomes tedious.
      //
      // So we have the following to be None if either:
      // - it's time to do mutation, or
      // - crossover parent sampling fails within some 30 attempts.
      val nextNodeIfCrossover = if random.nextDouble() >= crossoverProbability then None else
        sampleFirstParentWithDistantEnoughNeighbors(30)

      // Then, based on whether the crossover parent is found, we perform either crossover or mutation
      val nextNode = nextNodeIfCrossover match
        case Some(firstParent) => // crossover
          // the first parent is sampled such that it has individuals at distance > 1
          // sample a distance out of the valid ones
          distanceBuffer.sortInPlace()
          val secondParentDistance = distanceBuffer(PowerLawDistribution.sample(distanceBuffer.size, crossoverParentMinimumDistanceBeta, random) - 1)
          // collect individuals at distance which is at least as much as the found distance, and sample one
          collectHandlesAtDistance(firstParent, _ >= secondParentDistance, crossoverSecondParentBuffer)
          crossoverSecondParentBuffer.sortInPlace()(using inverseFitnessOrdering)
          // sample crossover distance and perform crossover
          val secondParent = sampleParent(crossoverSecondParentBuffer, secondParentSelectionBeta)
          val crossoverDistanceDistribution = crossoverDistanceSource(secondParentDistance)
          assert(crossoverDistanceDistribution.min >= 1)
          assert(crossoverDistanceDistribution.max < secondParentDistance)
          val crossoverDistance = crossoverDistanceDistribution.sample(random)
          crossoverH(firstParent, secondParent, _ => crossoverDistance, _ => 0)
        case None => // mutation
          val parent = sampleParent(nodesSorted, mutationParentSelectionBeta)
          val change = PowerLawDistribution.sample(maximumPatchSize, mutationDistanceBeta, random)
          mutateH(parent, change)

      // if the just-sampled node is new, add it to the pool
      if nextNode.referenceCount == 1 then insertionSortAdd(nextNode)
