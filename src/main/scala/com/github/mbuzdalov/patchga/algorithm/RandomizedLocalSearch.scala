package com.github.mbuzdalov.patchga.algorithm

import scala.annotation.tailrec

import com.github.mbuzdalov.patchga.config.*

object RandomizedLocalSearch extends Optimizer:
  type RequiredConfig = FitnessType & PatchSizeType & Population & IntegralPatchSize & FitnessComparator
  def optimize(conf: RequiredConfig): Nothing =
    import conf._

    @tailrec
    def go(curr: IndividualHandle): Nothing =
      val next = mutateH(curr, fromInt(1))
      if compare(fitnessH(curr), fitnessH(next)) <= 0 then
        discardH(curr)
        go(next)
      else
        discardH(next)
        go(curr)

    go(newRandomIndividualH())
