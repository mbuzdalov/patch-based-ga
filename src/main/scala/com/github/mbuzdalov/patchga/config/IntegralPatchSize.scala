package com.github.mbuzdalov.patchga.config

trait IntegralPatchSize:
  self: PatchSizeType =>
    def patchSizeFromInt(distance: Int): PatchSize
    def patchSizeToInt(patchSize: PatchSize): Int

    given Conversion[PatchSize, Int] = patchSizeToInt
    given Conversion[Int, PatchSize] = patchSizeFromInt
