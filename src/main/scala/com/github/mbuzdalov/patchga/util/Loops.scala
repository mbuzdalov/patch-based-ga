package com.github.mbuzdalov.patchga.util

object Loops:
  inline def repeat(times: Int)(inline function: => Any): Unit =
    var idx = 0
    while idx < times do
      function
      idx += 1
  
  inline def loop(from: Int, until: Int)(inline function: Int => Any): Unit =
    var idx = from
    while idx < until do
      function(idx)
      idx += 1
  
  inline def exists(from: Int, until: Int)(inline function: Int => Boolean): Boolean =
    find(from, until)(function) < until  
  
  inline def find(from: Int, until: Int)(inline function: Int => Boolean): Int =
    var idx = from
    while idx < until && !function(idx) do
      idx += 1
    idx
    