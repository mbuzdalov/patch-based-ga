package com.github.mbuzdalov.patchga.config

trait IndividualHandleProto[T <: IndividualHandleProto[T]]:
  this: T =>
  def referenceCount: Int
  def genealogy: IndividualHandleProto.Genealogy[T]

object IndividualHandleProto:
  sealed trait Genealogy[+T]
  case object Unknown extends Genealogy[Nothing]
  case object RandomCreation extends Genealogy[Nothing]
  case class Mutation[+T](parent: T, distance: Int) extends Genealogy[T]
  case class Crossover[+T](mainParent: T, auxParent: T, nSameBits: Int, nDiffBits: Int, changedInSame: Int, changedInDiff: Int) extends Genealogy[T]
  