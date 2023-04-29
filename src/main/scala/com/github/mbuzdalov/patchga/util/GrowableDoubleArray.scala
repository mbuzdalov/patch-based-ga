package com.github.mbuzdalov.patchga.util

import scala.collection.mutable.ArrayBuffer

class GrowableDoubleArray:
  import GrowableDoubleArray._

  private val sliceBuffer = new ArrayBuffer[Array[Double]]()
  private var mySize: Int = 0

  def length: Int = mySize
  def size: Int = mySize

  def last: Double = apply(mySize - 1)

  def add(value: Double): Unit =
    if (mySize & Mask) == 0 then
      sliceBuffer.addOne(new Array[Double](Mask + 1))
    mySize += 1
    update(mySize - 1, value)

  def update(index: Int, value: Double): Unit =
    require(0 <= index && index < mySize)
    sliceBuffer(index >>> Shift)(index & Mask) = value

  def apply(index: Int): Double =
    require(0 <= index && index < mySize)
    sliceBuffer(index >>> Shift)(index & Mask)

object GrowableDoubleArray:
  private inline val Shift = 13
  private inline val Mask = (1 << Shift) - 1
