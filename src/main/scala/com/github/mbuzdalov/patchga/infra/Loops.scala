package com.github.mbuzdalov.patchga.infra

object Loops:
  inline def inRange(from: Int, until: Int)(inline function: Int => Any): Unit =
    var idx = from
    while idx < until do
      function(idx)
      idx += 1
      