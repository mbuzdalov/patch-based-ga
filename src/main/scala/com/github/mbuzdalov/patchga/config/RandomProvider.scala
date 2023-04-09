package com.github.mbuzdalov.patchga.config

import java.util.Random

trait RandomProvider:
  def random: Random
  