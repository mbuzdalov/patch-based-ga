package com.github.mbuzdalov.patchga.config

trait PatchType:
  self: IndividualType =>

  type ImmutablePatch
  type MutablePatch
  
  def reversedImmutablePatch(patch: ImmutablePatch): ImmutablePatch
  def applyToIndividual(individual: Individual, patch: ImmutablePatch): Unit
  
  def createMutablePatch(): MutablePatch
  def mutablePatchSize(patch: MutablePatch): Int
  def clearMutablePatch(patch: MutablePatch): Unit
  def subSampleMutablePatchToSize(patch: MutablePatch, newSize: Int): Unit
  def initializeMutablePatchFromDistance(patch: MutablePatch, distance: Int): Unit
  def initializeMutablePatchFromTwoIndividuals(patch: MutablePatch, source: Individual, target: Individual): Unit
  def addToMutablePatch(patch: MutablePatch, toAdd: ImmutablePatch): Unit
  def createImmutableVersion(patch: MutablePatch): ImmutablePatch
  