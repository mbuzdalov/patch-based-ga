package com.github.mbuzdalov.patchga.config

trait IndividualType:
  type Individual
  
  def copyOfIndividual(ind: Individual): Individual
  