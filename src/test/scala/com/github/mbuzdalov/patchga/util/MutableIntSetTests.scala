package com.github.mbuzdalov.patchga.util

import java.util.Random
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MutableIntSetTests extends AnyFlatSpec with Matchers:
  "MutableIntSet" should "pass a torture test" in:
    val n = 20

    val mutableIntSet = new MutableIntSet(n)
    val referenceSet = new scala.collection.mutable.HashSet[Int]
    val random = new Random(32234324)

    for _ <- 0 until 1000000 do
      Loops.loop(0, n)(i => mutableIntSet.contains(i) shouldBe referenceSet.contains(i))
      random.nextInt(3) match
        case 0 =>
          val idx = random.nextInt(20)
          if referenceSet.contains(idx) then
            referenceSet.remove(idx)
            mutableIntSet.remove(idx)
          else
            referenceSet.add(idx)
            mutableIntSet.add(idx)
        case 1 =>
          if mutableIntSet.size > 0 then
            val toRemove = mutableIntSet.sampleElementInSet(random)
            mutableIntSet.contains(toRemove) shouldBe true
            referenceSet.contains(toRemove) shouldBe true
            mutableIntSet.remove(toRemove)
            referenceSet.remove(toRemove)
        case 2 =>
          if mutableIntSet.size < n then
            val toAdd = mutableIntSet.sampleElementNotInSet(random)
            mutableIntSet.contains(toAdd) shouldBe false
            referenceSet.contains(toAdd) shouldBe false
            mutableIntSet.add(toAdd)
            referenceSet.add(toAdd)
