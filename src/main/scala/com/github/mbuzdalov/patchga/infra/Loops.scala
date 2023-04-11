package com.github.mbuzdalov.patchga.infra

object Loops:
  inline def loop(from: Int, until: Int)(inline function: Int => Any): Unit =
    var idx = from
    while idx < until do
      function(idx)
      idx += 1
  
  inline def exists(from: Int, until: Int)(inline function: Int => Boolean): Boolean =
    var idx = from
    var result = false
    while idx < until && !result do
      result = function(idx)
      idx += 1
    result  
    