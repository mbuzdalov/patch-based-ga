package com.github.mbuzdalov.patchga.config

trait SimpleMutationOperator:
  self: IndividualType =>
    def mutate(individual: Individual, distance: Int): Individual
