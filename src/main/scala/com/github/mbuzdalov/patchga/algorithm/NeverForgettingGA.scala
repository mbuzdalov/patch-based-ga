package com.github.mbuzdalov.patchga.algorithm

import com.github.mbuzdalov.patchga.config.*
import com.github.mbuzdalov.patchga.distribution.PowerLawDistribution
import com.github.mbuzdalov.patchga.population.SingleSlotMSTPopulation
import com.github.mbuzdalov.patchga.util.SortedVector

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class NeverForgettingGA(mutationParentSelectionBeta: Double,
                        mutationDistanceBeta: Double,
                        crossoverProbability: Double,
                        crossoverParentDistanceSelectionBeta: Double,
                        crossoverParentPairSelectionBeta: Double,
                        crossoverDistanceBeta: Double) extends Optimizer:
  type RequiredConfig = FitnessType & SingleSlotMSTPopulation & MaximumPatchSize & FitnessComparator & RandomProvider

  override def optimize(config: RequiredConfig): Nothing =
    import config.*

    val crossoverSecondParentBuffer = new ArrayBuffer[IndividualHandle]()
    val distanceBuffer = new ArrayBuffer[Int]()
    val distanceSeen = new Array[Boolean](maximumPatchSize + 1)
    val inverseFitnessOrdering = Ordering.by(fitnessH).reverse
    val nodesSorted = new SortedVector[IndividualHandle](using inverseFitnessOrdering)
    nodesSorted.add(newRandomIndividualH())

    def sampleFirstParent(): IndividualHandle =
      val index0 = PowerLawDistribution.sample(nodesSorted.size, mutationParentSelectionBeta, random) - 1
      val fitness0 = fitnessH(nodesSorted(index0))
      var indexLo = index0
      while indexLo > 0 && compare(fitnessH(nodesSorted(indexLo - 1)), fitness0) == 0 do indexLo -= 1
      var indexHi = index0
      while indexHi + 1 < nodesSorted.size && compare(fitnessH(nodesSorted(indexHi + 1)), fitness0) == 0 do indexHi += 1
      val index = random.nextInt(indexLo, indexHi + 1)
      nodesSorted(index)
    
    def sampleSecondParent(): IndividualHandle =
      val index0 = PowerLawDistribution.sample(crossoverSecondParentBuffer.size, crossoverParentPairSelectionBeta, random) - 1
      val fitness0 = fitnessH(crossoverSecondParentBuffer(index0))
      var indexLo = index0
      while indexLo > 0 && compare(fitnessH(crossoverSecondParentBuffer(indexLo - 1)), fitness0) == 0 do indexLo -= 1
      var indexHi = index0
      while indexHi + 1 < crossoverSecondParentBuffer.size && compare(fitnessH(crossoverSecondParentBuffer(indexHi + 1)), fitness0) == 0 do indexHi += 1
      val index = random.nextInt(indexLo, indexHi + 1)
      crossoverSecondParentBuffer(index)
    
    @tailrec
    def go(): Nothing =
      // with three different nodes, at least two of them have distance > 1.
      val nextNode = if nodesSorted.size >= 3 && random.nextDouble() < crossoverProbability then
        // crossover
        // sample a parent such that it has individuals at distance > 1
        var firstParent: IndividualHandle = null
        while
          firstParent = sampleFirstParent()
          distanceBuffer.clear()
          collectDistanceToHandles(firstParent, (_, d) => if d > 1 && !distanceSeen(d) then
            distanceSeen(d) = true
            distanceBuffer.addOne(d))
          distanceBuffer.foreach(v => distanceSeen(v) = false)
          distanceBuffer.isEmpty
        do ()
        // sample a distance out of the valid ones
        distanceBuffer.sortInPlace()
        val secondParentDistance = distanceBuffer(PowerLawDistribution.sample(distanceBuffer.size, crossoverParentDistanceSelectionBeta, random) - 1)
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
      
      if nextNode.getReferenceCount == 1 then nodesSorted.add(nextNode)
      go()
    go()
