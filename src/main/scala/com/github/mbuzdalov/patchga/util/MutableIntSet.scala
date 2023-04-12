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
      moveGivenElementToPosition(element, nElements)
      nElements += 1

  def remove(element: Int): Unit =
    if used(element) then
      used(element) = false
      nElements -= 1
      moveGivenElementToPosition(element, nElements)

  def groupAddRemove(toRemove: Int, toAdd: Int, rng: Random): Unit =
    val oldNElements = nElements
    // First, we normally remove first random `toRemove` elements, noting that the removed elements will be under the old nElements value
    Loops.loop(0, toRemove)(_ => remove(contents(rng.nextInt(nElements))))
    // Second, we add the elements that come from the range above `oldNElements` in two stages:
    // first, we move an element to the lower bound of the "old removed" range, then we normally add it
    Loops.loop(0, toAdd) { i =>
      val what = contents(oldNElements + i + rng.nextInt(maxSize - oldNElements - i))
      moveGivenElementToPosition(what, oldNElements + i)
      add(what)
    }

  def sampleElementInSet(rng: Random): Int =
    contents(rng.nextInt(nElements))

  def sampleElementNotInSet(rng: Random): Int =
    contents(nElements + rng.nextInt(maxSize - nElements))

  def clear(): Unit =
    while size > 0 do
      remove(contents(size - 1))

  def toIArray: IArray[Int] =
    IArray.unsafeFromArray(java.util.Arrays.copyOfRange(contents, 0, size))

  private def moveGivenElementToPosition(element: Int, position: Int): Unit =
    val elementOrig = reversePerm(element)
    if elementOrig != position then
      val replacement = contents(position)
      reversePerm(element) = position
      reversePerm(replacement) = elementOrig
      contents(elementOrig) = replacement
      contents(position) = element
