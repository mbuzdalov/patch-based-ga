package com.github.mbuzdalov.patchga.algorithm

import com.github.mbuzdalov.patchga.config.*

trait Optimizer:
  type RequiredConfig
  def optimize(config: RequiredConfig): Nothing

object Optimizer:
  type Any = Optimizer:
    type RequiredConfig >: FitnessType & Population & MaximumPatchSize & FitnessComparator & RandomProvider
  