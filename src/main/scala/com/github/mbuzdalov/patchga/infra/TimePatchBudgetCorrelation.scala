package com.github.mbuzdalov.patchga.infra

import scala.collection.mutable.ArrayBuffer

import com.github.mbuzdalov.patchga.config.*
import com.github.mbuzdalov.patchga.population.SingleSlotMSTPopulation

trait TimePatchBudgetCorrelation(stepsToAverage: Int) extends SimpleFitnessFunction, IncrementalFitnessFunction:
  self: IndividualType & FitnessType & PatchType & MaximumPatchSize & NewRandomIndividual & IncrementalFitnessFunction & SingleSlotMSTPopulation =>

  private var nEvaluations: Long = 0
  private var sumPatchSizesOverPeriod: Double = 0
  private var lastEntryTime: Long = System.nanoTime()
  private val buffer = new ArrayBuffer[TimePatchBudgetCorrelation.Record]()

  private[TimePatchBudgetCorrelation] def record(ind: Individual, fitness: Fitness): Unit =
    nEvaluations += 1
    sumPatchSizesOverPeriod += totalSizeOfPatches
    if nEvaluations % stepsToAverage == 0 then
      val currTime = System.nanoTime()
      buffer += TimePatchBudgetCorrelation.Record(totalEvaluations = nEvaluations,
                                                  averagePatchSize = sumPatchSizesOverPeriod / stepsToAverage,
                                                  averageOperationTime = (currTime - lastEntryTime) * 1e-9 / stepsToAverage)
      lastEntryTime = currTime
      sumPatchSizesOverPeriod = 0

  abstract override def computeFitness(individual: Individual): Fitness =
    val result = super.computeFitness(individual)
    record(individual, result)
    result

  abstract override def computeFitnessFunctionIncrementally(individual: Individual, oldFitness: Fitness,
                                                            patch: ImmutablePatch): Fitness =
    val result = super.computeFitnessFunctionIncrementally(individual, oldFitness, patch)
    record(individual, result)
    result

  def timePatchBudgetCorrelations: IndexedSeq[TimePatchBudgetCorrelation.Record] = buffer.toIndexedSeq

object TimePatchBudgetCorrelation:
  case class Record(totalEvaluations: Long, averagePatchSize: Double, averageOperationTime: Double)
