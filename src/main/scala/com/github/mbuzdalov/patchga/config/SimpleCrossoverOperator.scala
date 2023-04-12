package com.github.mbuzdalov.patchga.config

trait SimpleCrossoverOperator:
  self: IndividualType =>
    def crossover(mainParent: Individual, auxParent: Individual, 
                  inDifferingBits: Int => Int, inSameBits: Int => Int): Individual
    