package com.github.mbuzdalov.patchga.config

trait SimpleCrossoverOperator:
  self: IndividualType =>
    def crossover(mainParent: Individual, auxParent: Individual, distanceToMainFunction: Int => Int): Individual
    