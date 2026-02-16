package com.github.mbuzdalov.patchga.util

import java.nio.file.{Files, Path, Paths}
import scala.util.Using

object CollectorPlotter:
  private case class Algorithm(name: String, style: String, priority: Int)
  private case class ResultHeader(problem: String, size: Int, algorithm: Algorithm)
  
  private def parseProblemName(name: String): (String, Int) = name match
    case s"linear-$size-$suf" => (s"linear-$suf", size.toInt)
    case s"$name-$size" => (name, size.toInt)
  
  private def divisor(name: String, size: Int): Double = name match
    case "onemax" => size
    case "twomax" => size * math.sqrt(size)
    case "lo" => size * size
    case s"linear-$suf" => size /* * math.log(size)*/
    case s"plateau$suf" => size
    case s"cliff$suf" => size
  
  private def algorithmRewrite(name: String): Option[Algorithm] = name match
    case "rls" => Some(Algorithm("RLS", "very thick, black, dashed", 0))
    case "one-plus-one-ea-std" => Some(Algorithm("(1+1) EA", "very thick, blue, dashed", 1))
    case "two-plus-one-ga-std" => Some(Algorithm("(2+1) GA", "very thick, cyan, dashed", 2))
    case "ten-plus-one-ga-std" => Some(Algorithm("(10+1) GA", "very thick, cyan", 3))
    case "heavy-ollga" => None /* this one was broken */
    case "heavy-ollga-ok" => Some(Algorithm("$(1+(\\lambda,\\lambda))$ GA", "very thick, blue", 4))
    case "dega+" => Some(Algorithm("DEGA+", "very thick, black", 5))
    case "nfga-local" => Some(Algorithm("$\\text{NFGA}_{l}$", "very thick, red", 6))
    case "nfga-global" => None /* no big benefits; Some(Algorithm("$\\text{NFGA}_{g}$", "very thick, red, dashed", 7)) */
    case "nfga-flat" => Some(Algorithm("$\\text{NFGA}_{f}$", "very thick, green", 8))
    case s"$prefix-heavy" => None
  
  private def collect(path: Path): Map[ResultHeader, IndexedSeq[Int]] =
    val builder = IndexedSeq.newBuilder[(ResultHeader, Int)]
    Files.list(path).filter(_.getFileName.toString.endsWith(".log")).forEach: logFile =>
      Files.lines(logFile).forEach:
        case s"$algo on $problem: run $runNo finished, $ordNo out of $totalNo, in $time ms, result $evaluations" =>
          algorithmRewrite(algo) match
            case None =>
            case Some(algoName) =>
              val (pName, pSize) = parseProblemName(problem)
                builder.addOne(ResultHeader(pName, pSize, algoName) -> evaluations.toInt)
        case other => if other.nonEmpty then println(s"$logFile: string $other not parsed")
    builder.result().groupBy(_._1).map((k, v) => (k, v.map(_._2)))
  
  def main(args: Array[String]): Unit =
    val src = Paths.get(args(0))
    val resultRoot = src.resolveSibling("plots")
    Files.createDirectories(resultRoot)
    val results = collect(src)
    val allProblems = results.keys.map(_.problem).toIndexedSeq.distinct.sorted
    for problem <- allProblems do
      Using.resource(Files.newBufferedWriter(resultRoot.resolve(s"$problem.tex"))): out =>
        val allAlgorithms = results.keys.filter(_.problem == problem).map(_.algorithm).toIndexedSeq.distinct.sortBy(_.priority)
        for algorithm <- allAlgorithms do
          out.write(s"\\addplot[${algorithm.style}, error bars/.cd, y dir=both, y explicit] coordinates{")
          for size <- results.keys.filter(k => k.problem == problem && k.algorithm == algorithm).map(_.size).toIndexedSeq.distinct.sorted do
            val myDivisor = divisor(problem, size)
            for h <- results.keys.filter(k => k.problem == problem && k.algorithm == algorithm && k.size == size) do
              val evaluations = results(h).sorted.map(_ / myDivisor)
              //assert(evaluations.size == 51)
              val avg = evaluations.sum / evaluations.size
              val std = math.sqrt(evaluations.map(e => math.pow(e - avg, 2)).sum / (evaluations.size - 1))
              out.write(s"($size,$avg)+-(0,$std)")
          out.write(s"};\n\\addlegendentry{${algorithm.name}}\n")
          