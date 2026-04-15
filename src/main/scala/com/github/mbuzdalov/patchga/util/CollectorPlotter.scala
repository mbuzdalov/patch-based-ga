package com.github.mbuzdalov.patchga.util

import java.io.BufferedReader
import java.nio.file.{Files, Path, Paths}
import scala.util.Using

object CollectorPlotter:
  private case class ResultHeader(problem: String, size: Int, algorithm: String)
  
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
  
  private class AlgoRun(path: Path):
    def queries(): IndexedSeq[Int] = Using.resource(Files.newBufferedReader(path)): br =>
      val builder = IndexedSeq.newBuilder[Int]
      var line: String = null
      while
        line = br.readLine()
        line != null
      do
        builder += line.toInt
      builder.result()
  
  private def collect(path: Path): Map[ResultHeader, IndexedSeq[AlgoRun]] =
    val builder = IndexedSeq.newBuilder[(ResultHeader, AlgoRun)]
    Files.list(path).filter(_.getFileName.toString.endsWith(".log")).forEach: logFile =>
      Files.lines(logFile).forEach:
        case s"$algo on $problem: run $runNo finished, $ordNo out of $totalNo, in $time ms, result $evaluations" =>
          val detailFileRoot = logFile.resolveSibling(logFile.getFileName.toString.replace(".log", "")).resolve(problem).resolve(algo)
          val df1 = detailFileRoot.resolve(s"${runNo.toInt}.txt")
          val df2 = detailFileRoot.resolve(f"${runNo.toInt}%02d.txt")
          val detailFile = if Files.exists(df1) then df1 else df2
          val run = AlgoRun(detailFile)
          val (pName, pSize) = parseProblemName(problem)
          builder.addOne(ResultHeader(pName, pSize, algo) -> run)
        case other => if other.nonEmpty then println(s"$logFile: string $other not parsed")
    builder.result().groupBy(_._1).map((k, v) => (k, v.map(_._2)))
  
  private def expect(line: String, what: String): String =
    val prefix = s"$what: "
    if line.startsWith(prefix)
    then line.substring(prefix.length)
    else throw IllegalArgumentException(s"Expected option '$what'")
  
  private def maxOut(seq: IndexedSeq[Int]): IndexedSeq[Int] =
    seq.foldLeft(IndexedSeq.empty[Int]): (seq, v) =>
      if seq.isEmpty
      then seq.appended(v)
      else seq.appended(math.max(v, seq.last))
  
  private def readSizes(br: BufferedReader): IArray[Int] =
    IArray.unsafeFromArray(expect(br.readLine(), "anytime-sizes").split(" ").map(_.toInt))
  
  def main(args: Array[String]): Unit =
    val cfgFile = Paths.get(args(0))
    Using.resource(Files.newBufferedReader(Paths.get(args(0)))): cfg =>
      val src = cfgFile.resolveSibling(expect(cfg.readLine(), "source-dir"))
      val resultRoot = cfgFile.resolveSibling(expect(cfg.readLine(), "dest-dir"))
      val anytimeSizes = readSizes(cfg)
      Files.createDirectories(resultRoot)
      val results = collect(src)
      for (problem, withProblem) <- results.groupBy(_._1.problem) do
        for (algorithm, withAlgorithm) <- withProblem.groupBy(_._1.algorithm) do
          Using.resource(Files.newBufferedWriter(resultRoot.resolve(s"ev-$problem-$algorithm.csv"))): out =>
            out.write("size,avg,std,avg-norm,std-norm,q1,q2,q3,q1-norm,q2-norm,q3-norm\n")
            for (size, withSize) <- withAlgorithm.groupBy(_._1.size).toIndexedSeq.sortBy(_._1).map(p => { assert(p._2.size == 1); (p._1, p._2.head._2) }) do
              val myDivisor = divisor(problem, size)
              val evaluations = withSize.map(r => r.queries().size.toDouble).sorted
              //assert(evaluations.size == 51)
              val avg = evaluations.sum / evaluations.size
              val std = math.sqrt(evaluations.map(e => math.pow(e - avg, 2)).sum / (evaluations.size - 1))
              val median = evaluations(evaluations.size / 2)
              val q1 = evaluations(evaluations.size / 4)
              val q3 = evaluations(evaluations.size - 1 - evaluations.size / 4)
              out.write(s"$size,$avg,$std,${avg / myDivisor},${std / myDivisor},$q1,$median,$q3,${q1 / myDivisor},${median / myDivisor},${q3 / myDivisor}\n")
        for mySize <- anytimeSizes do
          for (algorithm, withAlgorithm) <- withProblem.filter(_._1.size == mySize).groupBy(_._1.algorithm).map(p => { assert(p._2.size == 1); (p._1, p._2.head._2)}) do
            Using.resource(Files.newBufferedWriter(resultRoot.resolve(s"rt-$problem-$mySize-$algorithm.csv"))): out =>
              out.write("query,q1,q2,q3,q1-inv,q2-inv,q3-inv\n")
              val allRunsRaw = withAlgorithm.map(r => maxOut(r.queries()))
              val maxSize = allRunsRaw.map(_.size).max
              val quads = IndexedSeq.tabulate(maxSize): i =>
                val slice = allRunsRaw.map(seq => if seq.size > i then seq(i) else seq.last).sorted
                (slice(slice.size / 4), slice(slice.size / 2), slice(slice.size - 1 - slice.size / 4))
              Loops.foreach(0, maxSize): i =>
                if i == 0 || i == maxSize - 1 || quads(i) != quads(i - 1) || quads(i) != quads(i + 1) then
                  val (q1, q2, q3) = quads(i)
                  out.write(s"${i + 1},$q1,$q2,$q3,${mySize - q1 + 1},${mySize - q2 + 1},${mySize - q3 + 1}\n")
