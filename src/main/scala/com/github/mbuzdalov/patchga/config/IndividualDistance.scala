package com.github.mbuzdalov.patchga.config

trait IndividualDistance:
  self: IndividualType =>

  def distance(lhs: Individual, rhs: Individual): Int
