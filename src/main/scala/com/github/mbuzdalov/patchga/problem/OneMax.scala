package com.github.mbuzdalov.patchga.problem

import com.github.mbuzdalov.patchga.config.{FitnessComparator, FitnessType, IndividualType, PatchSizeType, SimpleFitnessFunction}

trait OneMax extends FitnessType, SimpleFitnessFunction, FitnessComparator:
  self: IndividualType {
    type Individual <: Array[Boolean]
  } =>
    override type Fitness = Int

    override def computeFitness(ind: Individual): Fitness =
      var idx, result = 0
      val size = ind.length
      while idx < size do
        if ind(idx) then result += 1
        idx += 1
      result

    override def compare(lhs: Fitness, rhs: Fitness): Int = java.lang.Integer.compare(lhs, rhs)
