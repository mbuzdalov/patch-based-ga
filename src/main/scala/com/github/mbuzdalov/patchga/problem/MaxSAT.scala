package com.github.mbuzdalov.patchga.problem

import com.github.mbuzdalov.patchga.config.{FitnessComparator, FitnessType, IncrementalFitnessFunction, IndividualType, PatchType, SimpleFitnessFunction}
import com.github.mbuzdalov.patchga.util.Loops

import java.util.Random

trait MaxSAT(referencer: MaxSAT.Referencer)
  extends FitnessType, SimpleFitnessFunction, FitnessComparator, IncrementalFitnessFunction:

  self: IndividualType { type Individual <: Array[Boolean] } & PatchType { type ImmutablePatch <: IArray[Int] } =>
  
  override type Fitness = Int
  
  override def computeFitness(ind: Individual): Int =
    Loops.count(0, referencer.clauses)(clIdx => referencer.computeClause(clIdx, ind))
  
  override def compare(lhs: Int, rhs: Int): Int =
    java.lang.Integer.compare(lhs, rhs)
    
  override def computeFitnessFunctionIncrementally(individual: Individual, oldFitness: Int, patch: ImmutablePatch): Int =
    Loops.foreach(0, patch.length)(i => referencer.affectClausesOf(patch(i)))
    var newFitness = oldFitness
    newFitness -= referencer.countSatisfiedAffectedClauses(individual)
    Loops.foreach(0, patch.length)(i => individual(patch(i)) ^= true)
    newFitness += referencer.countSatisfiedAffectedClauses(individual)
    referencer.clearAffectedClauses()
    newFitness

object MaxSAT:
  class Referencer(val variables: Int, val clauses: Int, seed: Long, isHard: Boolean):
    private val affectedClauseList = Array.ofDim[Int](clauses)
    private val affectedClauses = Array.ofDim[Boolean](clauses)
    private var nAffectedClauses: Int = 0
    
    private val rng = Random(seed)
    private val clauseVar = IArray.fill(3 * clauses)(rng.nextInt(variables))
    private val clauseVal = IArray.fill(clauses):
      if isHard then rng.nextInt(6) match
        case 0 => 7
        case 1 => 7 - (1 << rng.nextInt(3))
        case _ => 1 << rng.nextInt(3)
      else 1 + rng.nextInt(7)

    private val clausesOfVar = locally:
      val counts = Array.ofDim[Int](variables)
      Loops.foreach(0, 3 * clauses): i =>
        val v = clauseVar(i)
        counts(v) += 1
      val result = Array.tabulate(variables)(i => Array.fill(counts(i))(-1))
      Loops.foreach(0, 3 * clauses): i =>
        val v = clauseVar(i)
        counts(v) -= 1
        result(v)(counts(v)) = i / 3
      Loops.foreach(0, variables)(i => assert(counts(i) == 0))
      result
    
    def affectClausesOf(variable: Int): Unit =
      val clauses = clausesOfVar(variable)
      Loops.foreach(0, clauses.length): i =>
        val clause = clauses(i)
        if !affectedClauses(clause) then
          affectedClauses(clause) = true
          affectedClauseList(nAffectedClauses) = clause
          nAffectedClauses += 1

    def countSatisfiedAffectedClauses(individual: Array[Boolean]): Int =
      Loops.count(0, nAffectedClauses): i =>
        computeClause(affectedClauseList(i), individual)
    
    def clearAffectedClauses(): Unit =
      Loops.foreach(0, nAffectedClauses): i =>
        affectedClauses(affectedClauseList(i)) = false
      nAffectedClauses = 0
    
    def computeClause(clause: Int, individual: Array[Boolean]): Boolean =
      val offset = clause * 3
      var result = clauseVal(clause)
      Loops.foreach(0, 3): j =>
        if individual(clauseVar(offset + j)) then result ^= (1 << j)
      result != 7
      