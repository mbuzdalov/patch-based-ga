package com.github.mbuzdalov.patchga.algorithm

import com.github.mbuzdalov.patchga.distribution.{IntegerDistribution, PowerLawDistribution}
import com.github.mbuzdalov.patchga.util.Loops

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class NeverForgettingGA(mutationParentSelectionSource: Int => IntegerDistribution,
                        mutationDistanceSource: Int => IntegerDistribution,
                        firstParentSelectionSource: Int => IntegerDistribution,
                        crossoverProbability: Double,
                        crossoverParentMinimumDistanceSource: Int => IntegerDistribution,
                        crossoverParentMaximumDistanceSource: Int => Int,
                        secondParentSelectionSource: Int => IntegerDistribution,
                        crossoverDistanceSource: Int => IntegerDistribution) extends Optimizer:
  override def optimize(config: Optimizer.Config): Nothing =
    import config.*

    val crossoverParentDistanceCap = crossoverParentMaximumDistanceSource(maximumPatchSize)
    require(crossoverParentDistanceCap >= 2, "Maximum distance cannot be smaller than 2")
    
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
    
    def sampleParent(source: ArrayBuffer[IndividualHandle], distSource: Int => IntegerDistribution): IndividualHandle =
      val dist = distSource(source.size)
      require(dist.min == 1, s"Parent sampling distribution with minimum ${dist.min} is likely a bug, should be 1")
      require(dist.max <= source.size)
      val index0 = dist.sample(random) - 1
      val fitness0 = fitnessH(source(index0))
      var indexLo = index0
      while indexLo > 0 && compare(fitnessH(source(indexLo - 1)), fitness0) == 0 do indexLo -= 1
      var indexHi = index0
      while indexHi + 1 < source.size && compare(fitnessH(source(indexHi + 1)), fitness0) == 0 do indexHi += 1
      val index = random.nextInt(indexLo, indexHi + 1)
      source(index)
    
    def crossoverDistanceOK(d: Int): Boolean = 2 <= d && d <= crossoverParentDistanceCap
    
    @tailrec
    def sampleFirstParentWithDistantEnoughNeighbors(nRemaining: Int): Option[IndividualHandle] =
      val parent = sampleParent(nodesSorted, firstParentSelectionSource)
      distanceBuffer.clear()
      collectDistanceToHandles(parent): (_, d) => 
        if crossoverDistanceOK(d) && !distanceSeen(d) then
          distanceSeen(d) = true
          distanceBuffer.addOne(d)
      distanceBuffer.foreach(v => distanceSeen(v) = false)
      if distanceBuffer.nonEmpty then Some(parent)
      else if nRemaining == 0 then None
      else sampleFirstParentWithDistantEnoughNeighbors(nRemaining - 1)
    
    val mutationDistanceDistribution = mutationDistanceSource(maximumPatchSize)
    require(mutationDistanceDistribution.min >= 0)
    require(mutationDistanceDistribution.max <= maximumPatchSize)
    
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
          val secondParentDistanceDistribution = crossoverParentMinimumDistanceSource(distanceBuffer.size)
          require(secondParentDistanceDistribution.min == 1, 
            s"Second parent distance distribution with min = ${secondParentDistanceDistribution.min} is likely a bug, should be 1")
          require(secondParentDistanceDistribution.max <= distanceBuffer.size)
          val secondParentDistance = distanceBuffer(secondParentDistanceDistribution.sample(random) - 1)
          // collect individuals at distance which is at least as much as the found distance, and sample one
          collectHandlesAtDistance(firstParent, _ >= secondParentDistance, crossoverSecondParentBuffer)
          crossoverSecondParentBuffer.sortInPlace()(using inverseFitnessOrdering)
          // sample crossover distance and perform crossover
          val secondParent = sampleParent(crossoverSecondParentBuffer, secondParentSelectionSource)
          val crossoverDistanceDistribution = crossoverDistanceSource(secondParentDistance)
          assert(crossoverDistanceDistribution.min >= 1)
          assert(crossoverDistanceDistribution.max < secondParentDistance)
          val crossoverDistance = crossoverDistanceDistribution.sample(random)
          crossoverH(firstParent, secondParent, _ => crossoverDistance, _ => 0)
        case None => // mutation
          val parent = sampleParent(nodesSorted, mutationParentSelectionSource)
          val change = mutationDistanceDistribution.sample(random)
          mutateH(parent, change)

      // if the just-sampled node is new, add it to the pool
      if nextNode.referenceCount == 1 then insertionSortAdd(nextNode)

object NeverForgettingGA:
  def withPowerLaw(mutationParentSelectionBeta: Double, 
                   mutationDistanceBeta: Double, 
                   firstParentSelectionBeta: Double, 
                   crossoverProbability: Double,
                   crossoverParentMinimumDistanceBeta: Double,
                   crossoverParentMaximumDistance: Int => Int,
                   secondParentSelectionBeta: Double,
                   crossoverDistanceSource: Int => IntegerDistribution): NeverForgettingGA = NeverForgettingGA(
    mutationParentSelectionSource = PowerLawDistribution(_, mutationParentSelectionBeta),
    mutationDistanceSource = PowerLawDistribution(_, mutationDistanceBeta),
    firstParentSelectionSource = PowerLawDistribution(_, firstParentSelectionBeta),
    crossoverProbability = crossoverProbability,
    crossoverParentMinimumDistanceSource = PowerLawDistribution(_, crossoverParentMinimumDistanceBeta),
    crossoverParentMaximumDistanceSource = crossoverParentMaximumDistance,
    secondParentSelectionSource = PowerLawDistribution(_, secondParentSelectionBeta),
    crossoverDistanceSource = crossoverDistanceSource,
  )
  