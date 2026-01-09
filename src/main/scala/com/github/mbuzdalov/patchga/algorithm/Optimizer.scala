package com.github.mbuzdalov.patchga.algorithm

import com.github.mbuzdalov.patchga.config.*

trait Optimizer:
  type RequiredConfig
  def optimize(config: RequiredConfig): Nothing

object Optimizer:
  type MinimalRequirements = FitnessType & Population & MaximumPatchSize & FitnessComparator & RandomProvider 
  type Any = Optimizer:
    type RequiredConfig >: MinimalRequirements
