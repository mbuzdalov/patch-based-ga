package com.github.mbuzdalov.patchga.problem

import com.github.mbuzdalov.patchga.config.{FitnessComparator, FitnessType}

trait Plateau(size: Int, gap: Int) extends FitnessComparator:
  self: FitnessType { type Fitness = Int } =>

  override def compare(lhs: Fitness, rhs: Fitness): Int =
    val lf = Plateau.transform(lhs, size, gap)
    val rf = Plateau.transform(rhs, size, gap)
    Integer.compare(lf, rf)

object Plateau:
  def transform(oneMaxFitness: Int, size: Int, gap: Int): Int =
    if oneMaxFitness == size || oneMaxFitness + gap <= size
    then oneMaxFitness
    else size - gap
