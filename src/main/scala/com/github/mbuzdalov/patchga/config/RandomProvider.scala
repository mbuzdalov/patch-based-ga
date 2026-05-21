package com.github.mbuzdalov.patchga.config

import java.util.random.RandomGenerator

trait RandomProvider:
  def random: RandomGenerator
  