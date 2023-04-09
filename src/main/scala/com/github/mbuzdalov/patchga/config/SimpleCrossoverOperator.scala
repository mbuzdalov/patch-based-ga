package com.github.mbuzdalov.patchga.config

trait SimpleCrossoverOperator:
  self: IndividualType & PatchSizeType =>
    def crossover(mainParent: Individual, auxParent: Individual, distanceToMain: PatchSize): Individual
    