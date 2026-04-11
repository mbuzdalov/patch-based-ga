package com.github.mbuzdalov.patchga.config

trait IndividualHandleProto[T <: IndividualHandleProto[T]]:
  this: T =>
  def referenceCount: Int
  def genealogy: IndividualHandleProto.Genealogy[T]

object IndividualHandleProto:
  enum Genealogy[+T]:
    case Unknown
    case RandomCreation
    case Mutation(parent: T, distance: Int)
    case Crossover(mainParent: T, auxParent: T, nSameBits: Int, nDiffBits: Int, changedInSame: Int, changedInDiff: Int)
  