package com.github.mbuzdalov.patchga.infra

import java.util.Random
import java.util.concurrent.ThreadLocalRandom

import com.github.mbuzdalov.patchga.config.RandomProvider

trait ThreadLocalRandomProvider extends RandomProvider:
  override def random: Random = ThreadLocalRandom.current()
  