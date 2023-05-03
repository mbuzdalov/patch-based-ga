package com.github.mbuzdalov.patchga.algorithm

import java.util.HashMap as JHashMap
import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, TreeSet as MuTreeSet}
import com.github.mbuzdalov.patchga.config.*
import com.github.mbuzdalov.patchga.distribution.PowerLawDistribution
import com.github.mbuzdalov.patchga.population.SingleSlotMSTPopulation
import com.github.mbuzdalov.patchga.util.{DestructiveOrderedStatistics, SortedVector}

class NeverForgettingGA(mutationParentSelectionBeta: Double,
                        mutationDistanceBeta: Double,
                        crossoverProbability: Double,
                        crossoverParentDistanceSelectionBeta: Double,
                        crossoverParentPairSelectionBeta: Double,
                        crossoverDistanceBeta: Double) extends Optimizer:
  type RequiredConfig = FitnessType & SingleSlotMSTPopulation & MaximumPatchSize & FitnessComparator & RandomProvider

  override def optimize(config: RequiredConfig): Nothing =
    import config._

    val crossoverSecondParentBuffer = new ArrayBuffer[IndividualHandle]()

    def sampleDistanceForCrossover(distance: Int) = PowerLawDistribution.sample(distance, crossoverDistanceBeta, random)

    class CounterRecord(val handle: IndividualHandle, var count: Int, val recordId: Int) extends Comparable[CounterRecord]:
      override def compareTo(o: CounterRecord): Int =
        val byFitness = summon[Ordering[Fitness]].compare(o.handle.fitness, handle.fitness)
        if byFitness != 0 then byFitness else
          if count != o.count then o.count - count else
            o.recordId - recordId


    val inverseFitnessOrdering = Ordering.by[IndividualHandle, Fitness](_.fitness).reverse

    class DistanceSampler(val distance: Int):
      private val handleMap = new JHashMap[IndividualHandle, CounterRecord]()
      private val recordSet = new MuTreeSet[CounterRecord]()
      private var nPairs = 0L
      private var nRecords = 0

      def isEmpty: Boolean = handleMap.isEmpty
      private def newCounterRecord(handle: IndividualHandle) =
        val result = new CounterRecord(handle, 0, nRecords)
        nRecords += 1
        recordSet.addOne(result)
        result

      def registerParents(h1: IndividualHandle, h2: IndividualHandle): Unit =
        nPairs += 2
        handleMap.computeIfAbsent(h1, newCounterRecord).count += 1
        handleMap.computeIfAbsent(h2, newCounterRecord).count += 1

      def sample(): IndividualHandle =
        val realisticSampleSize = math.min(1000000, nPairs).toInt
        var sample = PowerLawDistribution.sample(realisticSampleSize, crossoverParentPairSelectionBeta, random) - 1
        val iterator = recordSet.iterator
        var current = iterator.next()
        while sample >= current.count do
          sample -= current.count
          current = iterator.next()

        val firstParent = current.handle
        collectHandlesAtDistance(firstParent, distance, crossoverSecondParentBuffer)
        val secondParent = DestructiveOrderedStatistics.find(crossoverSecondParentBuffer, sample)(using inverseFitnessOrdering)
        crossoverH(firstParent, secondParent, sampleDistanceForCrossover, _ => 0)

    val nodesSorted = new SortedVector[IndividualHandle](using inverseFitnessOrdering)
    nodesSorted.add(newRandomIndividualH())

    val allDistanceSamplers = Array.tabulate(maximumPatchSize - 1)(s => new DistanceSampler(s + 2))
    val aliveDistanceSamplers = new SortedVector[DistanceSampler](using Ordering.by(_.distance))

    def registerPair(n1: IndividualHandle, n2: IndividualHandle, distance: Int): Unit =
      if distance >= 2 then
        val sampler = allDistanceSamplers(distance - 2)
        if sampler.isEmpty then aliveDistanceSamplers.add(sampler)
        sampler.registerParents(n1, n2)

    @tailrec
    def go(): Nothing =
      val totalPatchesBefore = totalSizeOfPatches
      val nextNode = if aliveDistanceSamplers.size > 0 && random.nextDouble() < crossoverProbability then
        // crossover
        val index = PowerLawDistribution.sample(aliveDistanceSamplers.size, crossoverParentDistanceSelectionBeta, random)
        aliveDistanceSamplers(index - 1).sample()
      else
        // mutation
        val index = PowerLawDistribution.sample(nodesSorted.size, mutationParentSelectionBeta, random)
        val change = PowerLawDistribution.sample(maximumPatchSize, mutationDistanceBeta, random)
        mutateH(nodesSorted(index - 1), change)
      if totalPatchesBefore != totalSizeOfPatches then
        nodesSorted.add(nextNode)
        collectDistanceToHandles(nextNode, (adjNode, distance) => registerPair(nextNode, adjNode, distance))
      go()

    go()
