package com.github.mbuzdalov.patchga.util

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object DestructiveOrderedStatistics:
  def find[T](data: ArrayBuffer[T], index: Int)(using ord: Ordering[T]): T =
    @tailrec
    def findImpl(left: Int, right: Int): T =
      if left == right then
        assert(left == index)
        data(index)
      else 
        var l = left
        var r = right
        val pivot = data((left + right) >>> 1)
        while l <= r do
          while ord.lt(data(l), pivot) do l += 1
          while ord.gt(data(r), pivot) do r -= 1
          if l <= r then
            val tmp = data(l)
            data(l) = data(r)
            data(r) = tmp
            l += 1
            r -= 1
        if index <= r then
          findImpl(left, r)
        else if index >= l then
          findImpl(l, right)
        else
          data(index)

    findImpl(0, data.length - 1)
