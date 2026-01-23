package com.github.mbuzdalov.patchga.main

import com.github.mbuzdalov.patchga.algorithm.{DEGAPlus, MuPlusOneGA, NeverForgettingGA, OnePlusLLGA, OnePlusOneEA, Optimizer}
import com.github.mbuzdalov.patchga.distribution.{BinomialDistribution, PowerLawDistribution}
import com.github.mbuzdalov.patchga.infra.FixedTargetTerminator
import com.github.mbuzdalov.patchga.problem.Problems
import com.github.mbuzdalov.patchga.util.Loops

import java.io.{BufferedReader, Closeable, InputStreamReader, PrintWriter}
import java.nio.file.{Files, Path, Paths}
import java.util.StringTokenizer
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Future, ScheduledThreadPoolExecutor, ThreadFactory}
import scala.collection.mutable.ArrayBuffer
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
      crossoverDistanceBeta = "crossover-distance-beta".doubleFrom(params, 0, 3, "NFGA: "),
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
        MuPlusOneGA.withStandardBitMutation(mu, crossoverProbability, constVal)
      case "power-law" =>
        val beta = secondLine.nextToken().toDouble(1, 3, "(mu+1) GA: expected a constant in [1;3] for 'power-law'")
        MuPlusOneGA.withPowerLawMutation(mu, crossoverProbability, beta)
      case other =>
        throw IllegalArgumentException("(mu+1) GA: Expected one of 'standard' or 'power-law'")
  
  private def readAlgorithm(r: KindaYamlReader): Optimizer.Any =
    val offset = r.currentOffset
    r.readAndMove match
      case "RLS" =>
        if r.currentOffset <= offset then OnePlusOneEA.randomizedLocalSearch
        else throw IllegalArgumentException(s"RLS does not take parameters, found one: '${r.current}'")
      case "DEGA+" =>
        if r.currentOffset <= offset then DEGAPlus
        else throw IllegalArgumentException(s"DEGA+ does not take parameters, found one: '${r.current}'")
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
    if r.readAndMove != "algorithms" then
      throw IllegalArgumentException("An 'algorithms' section expected")
    val off = r.currentOffset
    val algos = IndexedSeq.newBuilder[(String, Optimizer.Any)]
    val usedNames = scala.collection.mutable.HashSet[String]()
    while r.currentOffset == off do
      val name = r.readAndMove
      if usedNames.contains(name) then
        throw new IllegalArgumentException(s"Algorithm name '$name' already used")
      usedNames.add(name)
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
      case "Plateau" =>
        val size = "size".intFrom(params, 4, Int.MaxValue, "Plateau: ")
        val gap = "gap".intFrom(params, 2, size / 2, "Plateau: ")
        () => Problems.incrementalPlateauFT(size, gap, allowDuplicates = false, disableDiscard = true)
      case "Linear" =>
        val paramsToInt = params.map:
          case (k, v) => (k.toInt(1, Int.MaxValue, "Linear: weight must be positive"), v.toInt(0, Int.MaxValue, "Linear: weight count must be non-negative"))
        val maxW = paramsToInt.keys.max
        val arr = Array.ofDim[Int](maxW + 1)
        for (k, v) <- paramsToInt do arr(k) = v
        () => Problems.incrementalLinearFT(IArray.unsafeFromArray(arr), 1L, allowDuplicates = false, disableDiscard = true)
  
  private def readProblems(r: KindaYamlReader): IndexedSeq[(String, () => Problems.FixedTargetProblem, Set[String])] =
    if r.readAndMove != "problems" then
      throw IllegalArgumentException("A 'problems' section expected")
    val off = r.currentOffset
    val problems = IndexedSeq.newBuilder[(String, () => Problems.FixedTargetProblem, Set[String])]
    val usedNames = scala.collection.mutable.HashSet[String]()
    while r.currentOffset == off do
      val name = r.readAndMove
      if usedNames.contains(name) then
        throw new IllegalArgumentException(s"Problem name '$name' already used")
      usedNames.add(name)
      if r.currentOffset <= off then
        throw IllegalArgumentException(s"Expect problem description at an offset under problem name")
      val problem = readProblem(r)
      if r.currentOffset <= off then
        throw IllegalArgumentException(s"Expect algorithm allowance description at an offset under problem name")
      val allowOffset = r.currentOffset
      if r.readAndMove != "allow" then
        throw IllegalArgumentException("Expected 'allow' section")
      val set = Set.newBuilder[String]
      while r.currentOffset > allowOffset do
        set.addOne(r.readAndMove)
      problems.addOne((name, problem, set.result()))
    problems.result()
  
  // Reading runtime configuration
  
  private case class RuntimeConfig(nProcessors: Int, nRuns: Int, stackSize: Int)
  private def readRuntime(r: KindaYamlReader): RuntimeConfig =
    val offset = r.currentOffset
    if r.readAndMove != "runtime" then
      throw IllegalArgumentException("A 'runtime' section is expected")
    if r.currentOffset <= offset then
      throw IllegalArgumentException("Empty runtime section")
    val params = readParams(r)
    RuntimeConfig(
      nProcessors = "processors".intFrom(params, 0, Int.MaxValue, "Runtime: "),
      nRuns = "runs".intFrom(params, 1, Int.MaxValue, "Runtime: "),
      stackSize = "stack".intFrom(params, 1 << 13, Int.MaxValue, "Runtime: ")
    )
  
  private case class ProblemAlgorithmPair(problemName: String, algorithmName: String)
  private class RunInfo:
    val futures = ArrayBuffer[Future[?]]()
    val results = ArrayBuffer[Long]()
  
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
      val runInfoMap = scala.collection.mutable.HashMap[ProblemAlgorithmPair, RunInfo]()

      // start the command line reader
      val cmdLine: Runnable = () =>
        Using.resource(new BufferedReader(new InputStreamReader(System.in))): stdin =>
          Loops.forever:
            val cmd = StringTokenizer(stdin.readLine(), " ")
            cmd.nextToken() match
              case "help" =>
                println("Supported commands are:")
                println("  dump [unfinished]: for each problem-algorithm pair, show how many runs are finished,")
                println("                     omitting fully completed pairs if 'unfinished' is specified.")
              case "dump" =>
                val showUnfinishedOnly = cmd.hasMoreTokens && cmd.nextToken() == "unfinished"
                runInfoMap.synchronized:
                  for (probName, _, _) <- problems do
                    println(s"$probName:")
                    for
                      (algoName, _) <- algorithms
                      info <- runInfoMap.get(ProblemAlgorithmPair(probName, algoName))
                      if info.results.size < info.futures.size || !showUnfinishedOnly
                    do println(s"  $algoName: ${info.results.size} out of ${info.futures.size} finished")
              case other => if other.nonEmpty then println(s"Unknown command '$other'")
      val cmdLineRunner = Thread(cmdLine, "Command line processor")
      cmdLineRunner.setDaemon(true)
      cmdLineRunner.start()
      
      val threadFactory = new ThreadFactory:
        private val counter = AtomicInteger()
        override def newThread(r: Runnable): Thread =
          new Thread(Thread.currentThread().getThreadGroup, r, s"worker-${counter.getAndIncrement()}", runtime.stackSize)
      
      // run the main computation
      Using.resource(Files.newBufferedWriter(Paths.get(args(0).replace(".yaml", ".log")))): log =>
        Using.resource(ScheduledThreadPoolExecutor(nProcessors, threadFactory)): pool =>
          // schedule all configs to run
          Loops.foreach(0, runtime.nRuns): run =>
            for
              (probName, problemFun, allowance) <- problems
              (algoName, algorithm) <- algorithms
              if allowance.contains("all") || allowance.contains(algoName)
            do
              val key = ProblemAlgorithmPair(probName, algoName)
              val runInfo = runInfoMap.synchronized(runInfoMap.getOrElseUpdate(key, RunInfo()))
              val task: Runnable = () =>
                val t0 = System.currentTimeMillis()
                val config = problemFun()
                val reached = FixedTargetTerminator.runUntilTargetReached(algorithm)(config)
                val time = System.currentTimeMillis() - t0
                val fitnessRecord = config.fitnessValuesInOrder.map(_.toString).asJava
                val directory = Paths.get(args(0).replace(".yaml", "")).resolve(probName).resolve(algoName)
                Files.createDirectories(directory)
                Files.write(directory.resolve(runFmtString.format(run)), fitnessRecord)
                runInfoMap.synchronized:
                  log.write(s"$algoName on $probName: run $run finished, ${runInfo.results.size + 1} out of ${runtime.nRuns}, in $time ms, result ${reached.nEvaluations}\n")
                  log.flush()
                  runInfo.results.addOne(reached.nEvaluations)
              runInfo.futures.addOne(pool.submit(task))

          // then wait for them to finish execution
          for
            runInfo <- runInfoMap.values
            future <- runInfo.futures
          do future.get()

      // present easy stats
      Using.resource(PrintWriter(args(0).replace(".yaml", ".out"))): pw =>
        for (probName, _, _) <- problems do
          pw.println(s"$probName:")
          for (algoName, _) <- algorithms do
            runInfoMap.get(ProblemAlgorithmPair(probName, algoName)) match
              case Some(r) =>
                val results = r.results.toIndexedSeq.sorted
                pw.println(f"  $algoName: mean = ${results.sum.toDouble / results.size}%.2f, min = ${results.head}, median = ${results(results.size / 2)}, max = ${results.last}")
              case None =>
