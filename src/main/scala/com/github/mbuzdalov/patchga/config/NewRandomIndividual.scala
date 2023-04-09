package com.github.mbuzdalov.patchga.config

trait NewRandomIndividual:
  self: IndividualType =>
    def newRandomIndividual(): Individual
    