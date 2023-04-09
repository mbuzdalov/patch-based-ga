package com.github.mbuzdalov.patchga.config

trait FitnessComparator:
  self: FitnessType =>
    def compare(lhs: Fitness, rhs: Fitness): Int
    