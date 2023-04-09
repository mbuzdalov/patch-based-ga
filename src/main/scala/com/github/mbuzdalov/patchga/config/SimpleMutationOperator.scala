package com.github.mbuzdalov.patchga.config

trait SimpleMutationOperator:
  self: IndividualType & PatchSizeType =>
    def mutate(individual: Individual, distance: PatchSize): Individual
