package com.github.mbuzdalov.patchga.config

trait SimpleFitnessFunction:
  self: IndividualType & FitnessType =>
    def computeFitness(ind: Individual): Fitness
