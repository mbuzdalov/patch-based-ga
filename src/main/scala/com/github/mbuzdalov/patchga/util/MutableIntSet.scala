package com.github.mbuzdalov.patchga.util

import java.util.Random

class MutableIntSet(maxSize: Int):
  private val used = new Array[Boolean](maxSize)
  private val contents, reversePerm = Array.tabulate(maxSize)(i => i)
  private var nElements: Int = 0

  def size: Int = nElements

  def contains(element: Int): Boolean = used(element)

  def add(element: Int): Unit =
    if !used(element) then
      used(element) = true
      moveGivenElementToNElements(element)
      nElements += 1

  def remove(element: Int): Unit =
    if used(element) then
      used(element) = false
      nElements -= 1
      moveGivenElementToNElements(element)

  def sampleElementInSet(rng: Random): Int =
    contents(rng.nextInt(nElements))
  
  def sampleElementNotInSet(rng: Random): Int =
    contents(nElements + rng.nextInt(maxSize - nElements))
  
  def clear(): Unit =
    while size > 0 do
      remove(contents(size - 1))

  def toIArray: IArray[Int] =
    IArray.unsafeFromArray(java.util.Arrays.copyOfRange(contents, 0, size))

  private def moveGivenElementToNElements(element: Int): Unit =
    val elementOrig = reversePerm(element)
    if elementOrig != nElements then
      val replacement = contents(nElements)
      reversePerm(element) = nElements
      reversePerm(replacement) = elementOrig
      contents(elementOrig) = replacement
      contents(nElements) = element
