package com.github.mbuzdalov.patchga.config

trait PatchType:
  self: IndividualType =>

  type ImmutablePatch
  type MutablePatch
  
  def reversedImmutablePatch(patch: ImmutablePatch): ImmutablePatch
  def applyToIndividual(individual: Individual, patch: ImmutablePatch): Unit
  def immutablePatchSize(patch: ImmutablePatch): Int
  
  def createMutablePatch(): MutablePatch
  def mutablePatchSize(patch: MutablePatch): Int
  def clearMutablePatch(patch: MutablePatch): Unit
  def applyCrossoverRequest(patch: MutablePatch, nRemove: Int, nAdd: Int): Unit
  def initializeMutablePatchFromDistance(patch: MutablePatch, distance: Int): Unit
  def initializeMutablePatchFromTwoIndividuals(patch: MutablePatch, source: Individual, target: Individual): Unit
  def appendToMutablePatch(patch: MutablePatch, toAppend: ImmutablePatch): Unit
  def prependToMutablePatch(patch: MutablePatch, toPrepend: ImmutablePatch): Unit
  def createImmutableVersion(patch: MutablePatch): ImmutablePatch
  