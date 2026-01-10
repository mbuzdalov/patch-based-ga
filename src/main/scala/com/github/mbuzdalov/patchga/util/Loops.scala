package com.github.mbuzdalov.patchga.util

object Loops:
  inline def repeat(times: Int)(inline function: => Any): Unit =
    var idx = 0
    while idx < times do
      function
      idx += 1
  
  inline def foreach(from: Int, until: Int)(inline function: Int => Any): Unit =
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
  
  inline def count(from: Int, until: Int)(inline function: Int => Boolean): Int =
    var idx = from
    var result = 0
    while idx < until do
      if function(idx) then result += 1
      idx +=1
    result

  inline def forever(inline function: => Any): Nothing =
    while true do function
    throw new AssertionError("This line should not be reached")
end Loops
