package com.github.mbuzdalov.patchga.infra

import com.github.mbuzdalov.patchga.config.{FitnessType, IndividualType}
import com.github.mbuzdalov.patchga.population.SingleSlotMSTPopulation

import scala.collection.mutable.ArrayBuffer

object TimePatchBudgetCorrelation:
  case class Record(totalEvaluations: Long, averagePatchSize: Double, averageOperationTime: Double)

class TimePatchBudgetCorrelation(stepsToAverage: Int, config: IndividualType & FitnessType & SingleSlotMSTPopulation):
  private var nEvaluations: Long = 0
  private var sumPatchSizesOverPeriod: Double = 0
  private var lastEntryTime: Long = System.nanoTime()
  private val buffer = ArrayBuffer[TimePatchBudgetCorrelation.Record]()
  
  def timePatchBudgetCorrelations: IndexedSeq[TimePatchBudgetCorrelation.Record] = buffer.toIndexedSeq

  private def ping(): Unit =
    nEvaluations += 1
    sumPatchSizesOverPeriod += config.totalSizeOfPatches
    if nEvaluations % stepsToAverage == 0 then
      val currTime = System.nanoTime()
      buffer += TimePatchBudgetCorrelation.Record(totalEvaluations = nEvaluations,
        averagePatchSize = sumPatchSizesOverPeriod / stepsToAverage,
        averageOperationTime = (currTime - lastEntryTime) * 1e-9 / stepsToAverage)
      lastEntryTime = currTime
      sumPatchSizesOverPeriod = 0

  config.addEvaluationListener((ind, fit, hdl) => ping())
