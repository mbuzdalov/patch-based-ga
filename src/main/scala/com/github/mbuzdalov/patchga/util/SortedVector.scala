package com.github.mbuzdalov.patchga.util

import java.util.Random

import scala.collection.mutable.ArrayBuffer

import com.github.mbuzdalov.patchga.distribution.PowerLawDistribution

class SortedVector[T: Ordering]:
  private val contents = new ArrayBuffer[T]()

  def add(element: T): Unit =
    contents.addOne(element)
    var idx = contents.size - 2
    val ordering = summon[Ordering[T]]
    while idx >= 0 && ordering.gt(contents(idx), element) do
      contents(idx + 1) = contents(idx)
      contents(idx) = element
      idx -= 1

  def apply(index: Int): T = contents(index)

  def size: Int = contents.size
