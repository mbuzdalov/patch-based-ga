package com.github.mbuzdalov.patchga.problem

import com.github.mbuzdalov.patchga.config.{FitnessComparator, FitnessType, IndividualType, SimpleFitnessFunction}
import com.github.mbuzdalov.patchga.infra.Loops

trait OneMax extends FitnessType, SimpleFitnessFunction, FitnessComparator:
  self: IndividualType {
    type Individual <: Array[Boolean]
  } =>
    override type Fitness = Int

    override def computeFitness(ind: Individual): Fitness =
      var result = 0
      Loops.inRange(0, ind.length)(i => if ind(i) then result += 1)
      result

    override def compare(lhs: Fitness, rhs: Fitness): Int = java.lang.Integer.compare(lhs, rhs)
