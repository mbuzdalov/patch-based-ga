package com.github.mbuzdalov.patchga.algorithm

import com.github.mbuzdalov.patchga.config.*

trait Optimizer:
  def optimize(config: Optimizer.Config): Nothing

object Optimizer:
  type Config = IndividualType & FitnessType & Population & MaximumPatchSize & FitnessComparator & RandomProvider
