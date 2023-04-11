package com.github.mbuzdalov.patchga.config

trait IncrementalFitnessFunction:
  self: IndividualType & FitnessType & PatchType =>
    // Note that this function is expected to apply the patch to the individual!
    def computeFitnessFunctionIncrementally(individual: Individual, oldFitness: Fitness, patch: ImmutablePatch): Fitness
    