package com.github.mbuzdalov.patchga.util

import java.io.BufferedReader
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
  
  private def collect(path: Path, rewriteMap: Map[String, Option[Algorithm]]): Map[ResultHeader, IndexedSeq[Int]] =
    val builder = IndexedSeq.newBuilder[(ResultHeader, Int)]
    Files.list(path).filter(_.getFileName.toString.endsWith(".log")).forEach: logFile =>
      Files.lines(logFile).forEach:
        case s"$algo on $problem: run $runNo finished, $ordNo out of $totalNo, in $time ms, result $evaluations" =>
          rewriteMap(algo) match
            case None =>
            case Some(algoName) =>
              val (pName, pSize) = parseProblemName(problem)
                builder.addOne(ResultHeader(pName, pSize, algoName) -> evaluations.toInt)
        case other => if other.nonEmpty then println(s"$logFile: string $other not parsed")
    builder.result().groupBy(_._1).map((k, v) => (k, v.map(_._2)))
  
  private def expect(line: String, what: String): String =
    val prefix = s"$what: "
    if line.startsWith(prefix)
    then line.substring(prefix.length)
    else throw IllegalArgumentException(s"Expected option '$what'")
  
  private def parseRewrites(br: BufferedReader): Map[String, Option[Algorithm]] =
    val builder = Map.newBuilder[String, Option[Algorithm]]
    var line: String = null
    while
      line = br.readLine()
      line != null
    do
      val key = line.trim
      val name = br.readLine()
      if name == "- ignore" then
        builder.addOne(name -> None)
      else
        val realName = expect(name, "- name")
        val style = expect(br.readLine(), "- style")
        val priority = expect(br.readLine(), "- priority").toInt
        builder.addOne(key -> Some(Algorithm(realName, style, priority)))
      end if
    end while
    builder.result()
  
  def main(args: Array[String]): Unit =
    val cfgFile = Paths.get(args(0))
    Using.resource(Files.newBufferedReader(Paths.get(args(0)))): cfg =>
      val src = cfgFile.resolveSibling(expect(cfg.readLine(), "source-dir"))
      val resultRoot = cfgFile.resolveSibling(expect(cfg.readLine(), "dest-dir"))
      val rewrites = parseRewrites(cfg)
      Files.createDirectories(resultRoot)
      val results = collect(src, rewrites)
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
            