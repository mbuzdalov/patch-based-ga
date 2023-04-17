package com.github.mbuzdalov.patchga.util

class MeanAndStandardDeviation(window: Int = -1):
  private var sum, sumSq: Double = 0.0
  private var nValues: Long = 0
  private var index: Int = 0
  private val windowData = if window > 0 then new Array[Double](window) else null

  def record(value: Double): Unit =
    nValues += 1
    sum += value
    sumSq += value * value
    if windowData != null then
      if nValues > windowData.length then
        val removed = windowData(index)
        sum -= removed
        sumSq -= removed * removed
        nValues -= 1
      windowData(index) = value
      index = (index + 1) % windowData.length

  def count: Long = nValues
  def mean: Double = sum / nValues
  def stdDev: Double = math.sqrt((sumSq / nValues - mean * mean) * count / (count - 1))
