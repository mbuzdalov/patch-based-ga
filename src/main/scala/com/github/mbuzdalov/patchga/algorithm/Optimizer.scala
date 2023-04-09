package com.github.mbuzdalov.patchga.algorithm

trait Optimizer:
  type RequiredConfig
  def optimize(config: RequiredConfig): Nothing
  