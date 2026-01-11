package com.github.mbuzdalov.patchga.algorithm

import com.github.mbuzdalov.patchga.config.{FitnessComparator, FitnessType, MaximumPatchSize, Population, RandomProvider}
import com.github.mbuzdalov.patchga.distribution.PowerLawDistribution
import com.github.mbuzdalov.patchga.util.Loops

import scala.annotation.tailrec

class OnePlusLLGA(mutationDistanceBeta: Double, crossoverDistanceBeta: Double) extends Optimizer:
  type RequiredConfig = FitnessType & Population & MaximumPatchSize & FitnessComparator & RandomProvider
  
  override def optimize(config: RequiredConfig): Nothing =
    import config.*
  
    inline def bestOfN(size: Int, gen: => IndividualHandle): IndividualHandle =
      var theBest = gen
      Loops.repeat(size - 1):
        val next = gen
        if compare(fitnessH(next), fitnessH(theBest)) > 0 then
          discardH(theBest)
          theBest = next
        else discardH(next)
      theBest  
    
    @tailrec
    def go(parent: IndividualHandle): Nothing =
      val mutSize = PowerLawDistribution.sample(maximumPatchSize, mutationDistanceBeta, random)
      val competitor = if mutSize == 1 then mutateH(parent, 1) else
        val bestMutant = bestOfN(mutSize, mutateH(parent, mutSize))
        if compare(fitnessH(bestMutant), fitnessH(parent)) > 0 then bestMutant else
          // it is important to not factor out sampling, because the second argument of bestOfN is by-name   
          bestOfN(mutSize, crossoverH(parent, bestMutant,
            inDifferingBits = _ => PowerLawDistribution.sample(mutSize - 1, crossoverDistanceBeta, random),
            inSameBits = _ => 0))
      if compare(fitnessH(competitor), fitnessH(parent)) >= 0 then
        discardH(parent)
        go(competitor)
      else
        discardH(competitor)
        go(parent)
    go(newRandomIndividualH())
