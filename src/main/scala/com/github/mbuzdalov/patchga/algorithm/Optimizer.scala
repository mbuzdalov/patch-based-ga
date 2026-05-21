package com.github.mbuzdalov.patchga.algorithm

import com.github.mbuzdalov.patchga.config.*

trait Optimizer:
  type RequiredConfig >: Optimizer.MinimalRequirements
  def optimize(config: RequiredConfig): Nothing

object Optimizer:
  type MinimalRequirements = IndividualType & FitnessType & Population & MaximumPatchSize & FitnessComparator & RandomProvider
