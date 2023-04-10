package com.github.mbuzdalov.patchga.config

trait Population:
  self: FitnessType & PatchSizeType =>
    type IndividualHandle
    def newRandomIndividualH(): IndividualHandle
    def mutateH(handle: IndividualHandle, distance: PatchSize): IndividualHandle
    def crossoverH(mainParent: IndividualHandle, auxParent: IndividualHandle, distanceToMainFunction: PatchSize => PatchSize): IndividualHandle
    def fitnessH(handle: IndividualHandle): Fitness
    def discardH(handle: IndividualHandle): Unit
