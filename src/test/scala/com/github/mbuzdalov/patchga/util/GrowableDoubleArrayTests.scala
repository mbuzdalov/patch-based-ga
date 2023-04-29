package com.github.mbuzdalov.patchga.util

import java.util.Random

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GrowableDoubleArrayTests extends AnyFlatSpec with Matchers:
  "A GrowableDoubleArray" should "pass a smoke test" in {
    val rng = new Random(2355325124234L)
    val reference = Array.fill(1000000)(rng.nextDouble())
    val growable = new GrowableDoubleArray
    reference.foreach(v => growable.add(v))
    reference.indices.foreach(i => growable(i) shouldBe reference(i))
  }
