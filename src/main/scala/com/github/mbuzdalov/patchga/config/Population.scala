package com.github.mbuzdalov.patchga.config

trait Population:
  self: FitnessType =>
    type IndividualHandle
    def newRandomIndividualH(): IndividualHandle
    def mutateH(handle: IndividualHandle, distance: Int): IndividualHandle
    def crossoverH(mainParent: IndividualHandle, auxParent: IndividualHandle, 
                   inDifferingBits: Int => Int, inSameBits: Int => Int): IndividualHandle
    def fitnessH(handle: IndividualHandle): Fitness
    def discardH(handle: IndividualHandle): Unit
