package com.github.mbuzdalov.patchga.config

trait IndividualHandleProto[T <: IndividualHandleProto[T]]:
  this: T =>
  def referenceCount: Int
