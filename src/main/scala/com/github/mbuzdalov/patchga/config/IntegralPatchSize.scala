package com.github.mbuzdalov.patchga.config

trait IntegralPatchSize:
  self: PatchSizeType =>
    def fromInt(distance: Int): PatchSize
    