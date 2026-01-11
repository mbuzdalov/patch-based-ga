package com.github.mbuzdalov.patchga.main

import com.github.mbuzdalov.patchga.algorithm.{MuPlusOneGA, NeverForgettingGA, OnePlusLLGA, OnePlusOneEA, Optimizer}
import com.github.mbuzdalov.patchga.distribution.{BinomialDistribution, PowerLawDistribution}
import com.github.mbuzdalov.patchga.infra.FixedTargetTerminator
import com.github.mbuzdalov.patchga.problem.Problems
import com.github.mbuzdalov.patchga.util.Loops

import java.io.{Closeable, PrintWriter}
import java.nio.file.{Files, Path, Paths}
import java.util.StringTokenizer
import java.util.concurrent.ScheduledThreadPoolExecutor
import java.util.concurrent.atomic.AtomicInteger
import scala.util.Using
import scala.jdk.CollectionConverters.*

object DistinctSamplesToOptimality:
  private class KindaYamlReader(path: Path) extends Closeable:
    private val br = Files.newBufferedReader(path)
    private var currLine = br.readLine()
    
    override def close(): Unit = br.close()
    
    def currentOffset: Int = if currLine == null then -1 else currLine.indexOf('-')
    def current: String = currLine.substring(currentOffset + 2)
    private def skip(): Unit = currLine = br.readLine()
    def readAndMove: String =
      val result = current
      skip()
      result
  end KindaYamlReader
  
  extension (token: String)
    private def toInt(min: Int, max: Int, msg: => String): Int =
      token.toIntOption match
        case Some(v) if min <= v && v <= max => v
        case _ => throw IllegalArgumentException(msg)
    private def intFrom(map: scala.collection.mutable.HashMap[String, String], min: Int, max: Int, prefix: String) =
      map.get(token) match
        case None => throw IllegalArgumentException(s"$prefix: parameter '$token' is not set")
        case Some(v) => v.toInt(min, max, if max == Int.MaxValue
          then s"$prefix: parameter '$token' should be an integer at least $min"
          else s"$prefix: parameter '$token' should be an integer in [$min..$max]")
    private def toDouble(min: Double, max: Double, msg: => String): Double =
      token.toDoubleOption match
        case Some(v) if min <= v && v <= max => v
        case _ => throw IllegalArgumentException(msg)
    private def doubleFrom(map: scala.collection.mutable.HashMap[String, String], min: Double, max: Double, prefix: String) =
      map.get(token) match
        case None => throw IllegalArgumentException(s"$prefix: parameter '$token' is not set")
        case Some(v) => v.toDouble(min, max, if max.isPosInfinity
          then s"$prefix: parameter '$token' should be at least $min"
          else s"$prefix: parameter '$token' should be in [$min; $max]")
  
  private def readParams(r: KindaYamlReader) =
    val params = scala.collection.mutable.HashMap[String, String]()
    val offset = r.currentOffset
    while r.currentOffset == offset do
      val tok = StringTokenizer(r.readAndMove, ":, ")
      params.put(tok.nextToken(), tok.nextToken())
    params
  
  // Reading algorithm configurations
  
  private def readOnePlusOneEA(r: KindaYamlReader): Optimizer.Any =
    val line = r.readAndMove
    val tok = StringTokenizer(line, ":, ")
    tok.nextToken() match
      case "standard" =>
        val constVal = tok.nextToken().toDouble(1e-5, Double.PositiveInfinity, "(1+1) EA: expected positive constant for 'standard'")
        OnePlusOneEA(n => BinomialDistribution(n, math.min(1, constVal / n)))
      case "power-law" =>
        val beta = tok.nextToken().toDouble(1, 3, "(1+1) EA: expected a constant in [1;3] for 'power-law'")
        OnePlusOneEA(n => PowerLawDistribution(n, beta))
      case other =>
        throw IllegalArgumentException("(1+1) EA: Expected one of 'standard' or 'power-law'")
        
  private def readNFGA(r: KindaYamlReader): Optimizer.Any =
    val params = readParams(r)
    NeverForgettingGA(
      firstParentSelectionBeta = "first-parent-selection-beta".doubleFrom(params, 1, 3, "NFGA: "),
      mutationDistanceBeta = "mutation-distance-beta".doubleFrom(params, 1, 3, "NFGA: "),
      crossoverProbability = "crossover-probability".doubleFrom(params, 0, 1, "NFGA: "),
      crossoverParentMinimumDistanceBeta = "crossover-parent-minimum-distance-beta".doubleFrom(params, 1, 3, "NFGA: "),
      secondParentSelectionBeta = "second-parent-selection-beta".doubleFrom(params, 1, 3, "NFGA: "),
      crossoverDistanceBeta = "crossover-distance-beta".doubleFrom(params, 1, 3, "NFGA: "),
    )
  
  private def readOnePlusLLGA(r: KindaYamlReader): Optimizer.Any =
    val params = readParams(r)
    OnePlusLLGA(
      mutationDistanceBeta = "mutation-distance-beta".doubleFrom(params, 1, 3, "(1+(L,L)) GA: "),
      crossoverDistanceBeta = "crossover-distance-beta".doubleFrom(params, 1, 3, "(1+(L,L)) GA: "),
    )
  
  private def readMuPlusOneGA(mu: Int, r: KindaYamlReader): Optimizer.Any =
    val offset = r.currentOffset
    val firstLine = StringTokenizer(r.readAndMove, ":, ")
    if firstLine.nextToken() != "crossover-probability" then
      throw new IllegalArgumentException("(mu+1) GA: expected 'crossover-probability'")
    val crossoverProbability = firstLine.nextToken().toDouble(0, 1, "(mu+1) GA: 'crossover-probability' should be in [0;1]")
    if r.currentOffset != offset then
      throw new IllegalArgumentException("(mu+1) GA: expected a distribution parameter, one of 'standard' or 'power-law'")
    val secondLine = StringTokenizer(r.readAndMove, ":, ")
    secondLine.nextToken() match
      case "standard" =>
        val constVal = secondLine.nextToken().toDouble(1e-5, Double.PositiveInfinity, "(mu+1) GA: expected positive constant for 'standard'")
        MuPlusOneGA(mu, crossoverProbability, n => BinomialDistribution(n, math.min(1, constVal / n)))
      case "power-law" =>
        val beta = secondLine.nextToken().toDouble(1, 3, "(mu+1) GA: expected a constant in [1;3] for 'power-law'")
        MuPlusOneGA(mu, crossoverProbability, n => PowerLawDistribution(n, beta))
      case other =>
        throw IllegalArgumentException("(mu+1) GA: Expected one of 'standard' or 'power-law'")
  
  private def readAlgorithm(r: KindaYamlReader): Optimizer.Any =
    val offset = r.currentOffset
    r.readAndMove match
      case "RLS" =>
        if r.currentOffset <= offset then OnePlusOneEA.randomizedLocalSearch
        else throw IllegalArgumentException(s"RLS does not take parameters, found one: '${r.current}'")
      case "(1+1) EA" =>
        if r.currentOffset <= offset then OnePlusOneEA.withStandardBitMutation
        else readOnePlusOneEA(r)
      case s"($mu+1) GA" => mu.toIntOption match
        case Some(v) if v > 1 => readMuPlusOneGA(v, r)
        case _ => throw IllegalArgumentException(s"(mu+1) GA: mu should be an integer > 1")
      case "NFGA" =>
        if r.currentOffset <= offset then throw IllegalArgumentException("NFGA: Parameters are required")
        else readNFGA(r)
      case "(1+(L,L)) GA" =>
        if r.currentOffset <= offset then throw IllegalArgumentException("(1+(L,L)) GA: Parameters are required")
        else readOnePlusLLGA(r)
      case other =>
        throw IllegalArgumentException(s"Unknown algorithm type: '$other'")
  
  private def readAlgorithms(r: KindaYamlReader): IndexedSeq[(String, Optimizer.Any)] =
    assert(r.readAndMove == "algorithms")
    val off = r.currentOffset
    val algos = IndexedSeq.newBuilder[(String, Optimizer.Any)]
    val usedNames = scala.collection.mutable.HashSet[String]()
    while r.currentOffset == off do
      val name = r.readAndMove
      if usedNames.contains(name) then
        throw new IllegalArgumentException(s"Algorithm name '$name' already used")
      if r.currentOffset <= off then
        throw new IllegalArgumentException(s"Expect algorithm description at an offset under algorithm name")
      algos.addOne(name -> readAlgorithm(r))
    algos.result()
  
  // Reading problem configurations
  
  private def readProblem(r: KindaYamlReader): () => Problems.FixedTargetProblem =
    val offset = r.currentOffset
    val name = r.readAndMove
    if r.currentOffset <= offset then
      throw new IllegalArgumentException("Problem type is always followed by problem parameters")
    val params = readParams(r)
    name match
      case "OneMax" =>
        val size = "size".intFrom(params, 1, Int.MaxValue, "OneMax: ")
        () => Problems.incrementalOneMaxFT(size, allowDuplicates = false, disableDiscard = true)
      case "LeadingOnes" =>
        val size = "size".intFrom(params, 1, Int.MaxValue, "LeadingOnes: ")
        () => Problems.incrementalLeadingOnesFT(size, allowDuplicates = false, disableDiscard = true)
      case "Cliff" =>
        val size = "size".intFrom(params, 4, Int.MaxValue, "Cliff: ")
        val gap = "gap".intFrom(params, 2, size / 2, "Cliff: ")
        () => Problems.incrementalCliffFT(size, gap, allowDuplicates = false, disableDiscard = true)
      case "Linear" =>
        val paramsToInt = params.map:
          case (k, v) => (k.toInt(1, Int.MaxValue, "Linear: weight must be positive"), v.toInt(0, Int.MaxValue, "Linear: weight count must be non-negative"))
        val maxW = paramsToInt.keys.max
        val arr = Array.ofDim[Int](maxW + 1)
        for (k, v) <- paramsToInt do arr(k) = v
        () => Problems.incrementalLinearFT(IArray.unsafeFromArray(arr), 1L, allowDuplicates = false, disableDiscard = true)
  
  private def readProblems(r: KindaYamlReader): IndexedSeq[(String, () => Problems.FixedTargetProblem)] =
    assert(r.readAndMove == "problems")
    val off = r.currentOffset
    val problems = IndexedSeq.newBuilder[(String, () => Problems.FixedTargetProblem)]
    val usedNames = scala.collection.mutable.HashSet[String]()
    while r.currentOffset == off do
      val name = r.readAndMove
      if usedNames.contains(name) then
        throw new IllegalArgumentException(s"Problem name '$name' already used")
      if r.currentOffset <= off then
        throw IllegalArgumentException(s"Expect problem description at an offset under problem name")
      problems.addOne(name -> readProblem(r))
    problems.result()
  
  // Reading runtime configuration
  
  private case class RuntimeConfig(nProcessors: Int, nRuns: Int)
  private def readRuntime(r: KindaYamlReader): RuntimeConfig =
    val offset = r.currentOffset
    assert(r.readAndMove == "runtime")
    if r.currentOffset <= offset then
      throw new IllegalArgumentException("Empty runtime section")
    val params = readParams(r)
    RuntimeConfig(
      nProcessors = "processors".intFrom(params, 0, Int.MaxValue, "Runtime: "),
      nRuns = "runs".intFrom(params, 1, Int.MaxValue, "Runtime: ")
    )
  
  private case class Result(problemName: String, algorithmName: String, result: Long)
  
  def main(args: Array[String]): Unit =
    if args.length == 0 then
      System.err.println("Usage: DistinctSamplesToOptimality <config.yaml>")
      System.exit(1)
    else Using.resource(KindaYamlReader(Paths.get(args(0)))): r =>
      val algorithms = readAlgorithms(r)
      val problems = readProblems(r)
      val runtime = readRuntime(r)
      val runFmtString = s"%0${s"${runtime.nRuns - 1}".length}d.txt"
      val nProcessors = if runtime.nProcessors > 0 then runtime.nProcessors else Runtime.getRuntime.availableProcessors()
      val allResults = IndexedSeq.newBuilder[Result]
      // run everything first, saving phenotype plots to separate files and writing general logs to a log file
      Using.resource(Files.newBufferedWriter(Paths.get(args(0).replace(".yaml", ".log")))): log =>
        Using.resource(ScheduledThreadPoolExecutor(nProcessors)): pool =>
          Loops.foreach(0, runtime.nRuns): run =>
            val tasksRemaining = AtomicInteger(problems.size * algorithms.size)
            val tasks = for
              (probName, problemFun) <- problems
              (algoName, algorithm) <- algorithms
            yield pool.submit: () =>
              val t0 = System.currentTimeMillis()
              val config = problemFun()
              val reached = FixedTargetTerminator.runUntilTargetReached(algorithm)(config)
              val time = System.currentTimeMillis() - t0
              val fitnessRecord = config.fitnessValuesInOrder.map(_.toString).asJava
              val directory = Paths.get(args(0).replace(".yaml", "")).resolve(probName).resolve(algoName)
              Files.createDirectories(directory)
              Files.write(directory.resolve(runFmtString.format(run)), fitnessRecord)
              tasksRemaining.synchronized:
                val tasks = tasksRemaining.decrementAndGet()
                log.write(s"Run $run of $algoName on $probName finished in $time ms, result ${reached.nEvaluations}, $tasks tasks remaining in the run\n")
                log.flush()
              Result(probName, algoName, reached.nEvaluations)
            tasks.foreach(t => allResults.addOne(t.get()))
            log.write(s"Run $run (${run + 1} of ${runtime.nRuns} runs) finished\n\n")
            log.flush()
      // then present easy stats
      Using.resource(PrintWriter(args(0).replace(".yaml", ".out"))): pw =>
        val allByProblem = allResults.result().groupBy(_.problemName)
        for (probName, _) <- problems do
          pw.println(s"$probName:")
          val allForProblemByAlgo = allByProblem(probName).groupBy(_.algorithmName)
          for (algoName, _) <- algorithms do
            val results = allForProblemByAlgo(algoName).map(_.result).sorted
            pw.println(f"  $algoName: mean = ${results.sum.toDouble / results.size}%.2f, min = ${results.head}, median = ${results(results.size / 2)}, max = ${results.last}")
            