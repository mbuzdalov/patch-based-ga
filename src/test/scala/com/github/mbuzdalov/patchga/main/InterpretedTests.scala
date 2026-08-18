package com.github.mbuzdalov.patchga.main

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import Interpreted.*

class InterpretedTests extends AnyFlatSpec with should.Matchers:
  "Conjoin on tuples" should "work for a pair with known successes/failures" in:
    val bothOK = (Success(1), Success(2.0))
    val result1: Interpreted[(Int, Double)] = bothOK.conjoinTuple
    result1 shouldEqual Success((1, 2.0))

    val okFail = (Success(1), error(1, "test"))
    val result2: Interpreted[(Int, Nothing)] = okFail.conjoinTuple
    result2 shouldEqual error(1, "test")
    
    val failOK = (error(2, "aaa"), Success(0.2))
    val result3: Interpreted[(Nothing, Double)] = failOK.conjoinTuple
    result3 shouldEqual error(2, "aaa")
    
    val bothFail = (error(3, "uuu"), error(4, "vvv"))
    val result4: Interpreted[(Nothing, Nothing)] = bothFail.conjoinTuple
    result4 shouldEqual Failure(IndexedSeq(Error(3, "uuu"), Error(4, "vvv")))
    
  it should "work for a pair with unknown successes/failures" in:
    val bothOK: (Interpreted[Int], Interpreted[Double]) = (Success(1), Success(2.0))
    val result1: Interpreted[(Int, Double)] = bothOK.conjoinTuple
    result1 shouldEqual Success((1, 2.0))
    
    val okFail: (Interpreted[Int], Interpreted[Double]) = (Success(1), error(1, "test"))
    val result2: Interpreted[(Int, Double)] = okFail.conjoinTuple
    result2 shouldEqual error(1, "test")
    
    val failOK: (Interpreted[Int], Interpreted[Double]) = (error(2, "aaa"), Success(0.2))
    val result3: Interpreted[(Int, Double)] = failOK.conjoinTuple
    result3 shouldEqual error(2, "aaa")
    
    val bothFail: (Interpreted[Int], Interpreted[Double]) = (error(3, "uuu"), error(4, "vvv"))
    val result4: Interpreted[(Int, Double)] = bothFail.conjoinTuple
    result4 shouldEqual Failure(IndexedSeq(Error(3, "uuu"), Error(4, "vvv")))
  
  "Conjoin on sequences" should "work for IndexedSeq" in:
    val allOK = IndexedSeq(Success(5), Success(4), Success(3))
    val result1: Interpreted[IndexedSeq[Int]] = allOK.conjoinSeq
    result1 shouldEqual Success(IndexedSeq(5, 4, 3))
    
    val oneNotOK = IndexedSeq(Success(5), error(1, "aaa"), Success(3))
    val result2: Interpreted[IndexedSeq[Int]] = oneNotOK.conjoinSeq
    result2 shouldEqual error(1, "aaa")
    
    val allNotOK: IndexedSeq[Interpreted[Int]] = IndexedSeq(error(1, "aaa"), error(2, "bbb"), error(3, "ccc"))
    val result3: Interpreted[IndexedSeq[Int]] = allNotOK.conjoinSeq
    result3 shouldEqual Failure(IndexedSeq(Error(1, "aaa"), Error(2, "bbb"), Error(3, "ccc")))
    