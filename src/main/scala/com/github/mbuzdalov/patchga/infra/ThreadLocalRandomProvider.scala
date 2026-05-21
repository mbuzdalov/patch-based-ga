package com.github.mbuzdalov.patchga.infra

import java.util.concurrent.ThreadLocalRandom
import com.github.mbuzdalov.patchga.config.RandomProvider

import java.util.random.RandomGenerator

trait ThreadLocalRandomProvider extends RandomProvider:
  override def random: RandomGenerator = ThreadLocalRandom.current()
  