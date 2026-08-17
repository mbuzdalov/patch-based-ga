package com.github.mbuzdalov.patchga.algorithm

import com.github.mbuzdalov.patchga.distribution.IntegerDistribution
import com.github.mbuzdalov.patchga.util.Loops

import scala.annotation.tailrec

class OnePlusLLGA(mutationDistanceDistributionSource: Int => IntegerDistribution,
                  crossoverDistanceDistributionSource: Int => IntegerDistribution) extends Optimizer:
  override def optimize(config: Optimizer.Config): Nothing =
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
    
    val mutationDistanceDist = mutationDistanceDistributionSource(maximumPatchSize)
    require(mutationDistanceDist.min >= 0)
    require(mutationDistanceDist.max <= maximumPatchSize)
    
    @tailrec
    def go(parent: IndividualHandle): Nothing =
      val mutSize = mutationDistanceDist.sample(random)
      val competitor = if mutSize == 0 then parent 
      else if mutSize == 1 then mutateH(parent, 1) 
      else
        val crossoverDistanceDist = crossoverDistanceDistributionSource(mutSize - 1)
        require(crossoverDistanceDist.min >= 0)
        require(crossoverDistanceDist.max < mutSize)
        val bestMutant = bestOfN(mutSize, mutateH(parent, mutSize))
        if compare(fitnessH(bestMutant), fitnessH(parent)) > 0 then bestMutant else
          // it is important to not factor out sampling, because the second argument of bestOfN is by-name   
          bestOfN(mutSize, crossoverH(parent, bestMutant,
            inDifferingBits = _ => crossoverDistanceDist.sample(random),
            inSameBits = _ => 0))
      if compare(fitnessH(competitor), fitnessH(parent)) >= 0 then
        discardH(parent)
        go(competitor)
      else
        discardH(competitor)
        go(parent)
    go(newRandomIndividualH())
