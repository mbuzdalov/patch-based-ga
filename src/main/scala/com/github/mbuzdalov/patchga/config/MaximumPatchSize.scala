package com.github.mbuzdalov.patchga.config

trait MaximumPatchSize:
  self: PatchSizeType =>
    def maximumPatchSize: PatchSize
    