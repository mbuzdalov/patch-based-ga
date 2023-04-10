package com.github.mbuzdalov.patchga.config

trait IntegralPatchSize:
  self: PatchSizeType =>
    def patchSizeFromInt(distance: Int): PatchSize
    def patchSizeToInt(patchSize: PatchSize): Int
    