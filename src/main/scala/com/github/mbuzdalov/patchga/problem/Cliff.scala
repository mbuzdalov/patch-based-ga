package com.github.mbuzdalov.patchga.problem

import com.github.mbuzdalov.patchga.config.{FitnessComparator, FitnessType, IndividualType, SimpleFitnessFunction}

trait Cliff(size: Int, gap: Int) extends FitnessComparator:
  self: FitnessType { type Fitness = Int } =>

  override def compare(lhs: Fitness, rhs: Fitness): Int =
    val lf = Cliff.transform(lhs, size, gap)
    val rf = Cliff.transform(rhs, size, gap)
    Integer.compare(lf, rf)

object Cliff:
  def transform(oneMaxFitness: Int, size: Int, gap: Int): Int =
    if oneMaxFitness + gap > size
    then 2 * oneMaxFitness - 2 * gap + 1
    else 2 * oneMaxFitness
