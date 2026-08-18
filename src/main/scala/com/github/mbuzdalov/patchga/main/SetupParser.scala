package com.github.mbuzdalov.patchga.main

import com.github.mbuzdalov.patchga.algorithm.{DEGAPlus, MuPlusOneGA, NeverForgettingGA, OnePlusLLGA, OnePlusOneEA, Optimizer}
import com.github.mbuzdalov.patchga.distribution.*
import com.github.mbuzdalov.patchga.main.Interpreted.conjoinSeq
import com.github.mbuzdalov.patchga.util.Loops
import fastparse.*
import fastparse.Parsed.{Failure, Success}

import scala.reflect.ClassTag

object SetupParser:
  import SingleLineWhitespace.*
  
  // Intermediate representation structures

  private sealed trait MathExpression:
    def index: Int
  private case class Variable(index: Int, name: String) extends MathExpression
  private case class Constant(index: Int, value: String) extends MathExpression
  private case class Application(index: Int, function: String, arguments: IArray[MathExpression]) extends MathExpression
  private case class Lambda(index: Int, variable: String, expression: MathExpression) extends MathExpression
  
  private sealed trait TreeExpression:
    def index: Int
  private case class MathNode(index: Int, expression: MathExpression) extends TreeExpression
  private case class MapNode(index: Int, name: String, values: Map[String, TreeExpression]) extends TreeExpression
  private case class ArrayNode(index: Int, values: IArray[TreeExpression]) extends TreeExpression
  
  // The parser
  
  private def plusMinusOpt[S: P] = P(CharIn("+\\-").?)
  private def digits[$: P] = P(CharsWhileIn("0-9"))
  private def exponent[$: P] = P(CharIn("eE") ~~ plusMinusOpt ~~ digits)
  private def fractional[$: P] = P("." ~~ digits)
  private def integral[$: P] = P("0" | CharIn("1-9") ~~ digits.?)
  private def identifier[$: P] = P(CharIn("A-Za-z") ~~ CharsWhileIn("0-9A-Za-z", min = 0))
  private def nnConstant[S: P] = P(integral ~~ fractional.? ~~ exponent.?)
  
  private def unwrapInfix(tree: (MathExpression, Seq[(Int, String, MathExpression)])): MathExpression =
    tree._2.foldLeft(tree._1)((left, arg) => Application(arg._1, arg._2, IArray(left, arg._3)))
  
  private def unwrapInfix2(tree: (Int, String, MathExpression, Seq[(Int, String, MathExpression)])): MathExpression =
    val newHead = tree._2 match
      case "" | "+" => tree._3
      case "-" => Application(tree._1, "-", IArray(Constant(tree._1, "0"), tree._3))
      case _ => throw AssertionError(s"tree._1 is '${tree._1}'")
    unwrapInfix((newHead, tree._4))
  
  private def nnConstantExp[S: P]: P[MathExpression] = P(Index ~~ nnConstant.!).map((index, value) => Constant(index, value))
  private def variableOrApplication[$: P]: P[MathExpression] = P(
    Index ~~ identifier.! ~ ("(" ~/ mathExpression.rep(sep = ",") ~ ")").?
  ).map: (index, id, maybeArgs) =>
    maybeArgs match
      case None => Variable(index, id)
      case Some(args) => Application(index, id, IArray(args*))
  
  private def parentheses[$: P]: P[MathExpression] = P("(" ~/ mathExpression ~ ")")
  private def factor[$: P]: P[MathExpression] = P(variableOrApplication | parentheses | nnConstantExp)
  private def product[$: P]: P[MathExpression] = P(factor ~ (Index ~ StringIn("*", "/", "div").! ~/ factor).rep).map(unwrapInfix)
  private def sum[$: P]: P[MathExpression] = P(Index ~ plusMinusOpt.! ~ product ~ (Index ~ CharIn("+\\-").! ~/ product).rep).map(unwrapInfix2)
  private def lambda[$: P]: P[MathExpression] = P(Index ~ identifier.! ~ "=>" ~/ mathExpression).map(Lambda.apply)
  private def mathExpression[$: P]: P[MathExpression] = P(lambda | sum)
  private def mathExpressionExact[$: P]: P[MathExpression] = P(Start ~ mathExpression ~ End)
  
  private class TreeParser(indent: Int):
    private def prefix[$: P] = P(" ".repX(exactly = indent * 2))
    private def mathNode[$: P]: P[TreeExpression] = P(Index ~ mathExpression).map(MathNode.apply)
    private def mapNode[$: P]: P[TreeExpression] =
      val sub = TreeParser.get(indent + 1)
      P(Index ~ identifier.! ~ ":" ~ "\n" ~~ (sub.prefix ~~ "-" ~/ identifier.! ~ ":" ~ sub.expression ~ "\n").repX(min = 1))
        .filter((index, id, args) => args.size == args.map(_._1).distinct.size)
        .map((index, id, args) => MapNode(index, id, args.toMap))
    private def arrayNode[$: P]: P[TreeExpression] =
      val sub = TreeParser.get(indent + 1)
      P(Index ~ "\n" ~~ (sub.prefix ~~ "-" ~/ sub.expression ~ "\n").repX(min = 1))
        .map((index, values) => ArrayNode(index, IArray(values *)))
    
    def expression[$: P]: P[TreeExpression] = P( mapNode | arrayNode | mathNode )
    def expressionExact[$: P]: P[TreeExpression] = P(Start ~ expression ~ End)
  
  private object TreeParser:
    private val cache = scala.collection.mutable.HashMap[Int, TreeParser]()
    def get(indent: Int): TreeParser = cache.getOrElseUpdate(indent, new TreeParser(indent))
  
  // Interpretation error reporting machinery
  
  private def prettyPrintErrors[T](text: String)(errors: IndexedSeq[Interpreted.Error]): String =
    val builder = StringBuilder()
    for e <- errors do
      builder.append(text).append('\n')
      Loops.repeat(e.index)(builder.append(' '))
      builder.append("^\n")
      builder.append(s"Error at index ${e.index + 1}: ${e.message}\n")
    builder.result()
  
  // Math expressions: Interpretation special functions

  private def roundDbl(a: Double): Double = math.round(a).toDouble

  // Math expressions: Some kind of metaprogramming
  
  private inline def lift[A, U, V](inline op: U => V): (A => U) => A => V =
    a => (x: A) => op(a(x))
  
  private inline def lift[A, R](inline op: (R, R) => R): (A => R, A => R) => A => R =
    (a, b) => (x: A) => op(a(x), b(x))
  
  private def ensureNonEmpty[T](args: IndexedSeq[Interpreted[T]])(using application: Application): Interpreted[IndexedSeq[T]] =
    if args.nonEmpty
    then args.conjoinSeq
    else Interpreted.error(application.index, s"'${application.function}' requires at least one argument")
  
  private def ensureOne[T](args: IndexedSeq[Interpreted[T]])(using application: Application): Interpreted[T] =
    if args.length == 1
    then args(0)
    else Interpreted.error(application.index, s"'${application.function}' requires one argument")
  
  private def ensureTwo[T](args: IndexedSeq[Interpreted[T]])(using application: Application): Interpreted[(T, T)] =
    args.conjoinSeq.flatMap: aa =>
      if aa.length == 2 then Interpreted.Success(aa(0), aa(1))
      else Interpreted.error(application.index, s"'${application.function}' requires two arguments")
  
  // Math expressions: Interpreters

  private def interpretAsInt(e: MathExpression): Interpreted[Int] = e match
      case Variable(index, name) => Interpreted.error(index, s"Variable '$name' is not an Int")
      case Constant(index, value) => value.toIntOption match
        case Some(v) => Interpreted.Success(v)
        case None => Interpreted.error(index, s"Constant '$value' cannot be parsed as an Int")
      case l: Lambda => Interpreted.error(l.index, s"Lambda expression cannot be parsed as an Int")
      case a@Application(index, fun, args) =>
        given Application = a // puts the current application in the context for removing some boilerplate
        fun match
          case "+" => ensureTwo(args.map(interpretAsInt)).map(_ + _)
          case "-" => ensureTwo(args.map(interpretAsInt)).map(_ - _)
          case "*" => ensureTwo(args.map(interpretAsInt)).map(_ * _)
          case "div" => ensureTwo(args.map(interpretAsInt)).map(_ / _)
          case "min" => ensureNonEmpty(args.map(interpretAsInt)).map(_.reduce(math.min))
          case "max" => ensureNonEmpty(args.map(interpretAsInt)).map(_.reduce(math.max))
          case "floor" => ensureOne(args.map(interpretAsDouble)).map(v => math.floor(v).toInt)
          case "ceil"  => ensureOne(args.map(interpretAsDouble)).map(v => math.ceil(v).toInt)
          case "round" => ensureOne(args.map(interpretAsDouble)).map(v => roundDbl(v).toInt)
          case _ => Interpreted.error(index, s"Unknown function in the Int context: '$fun'")
  
  private def interpretAsDouble(e: MathExpression): Interpreted[Double] = e match
    case Variable(index, name) => Interpreted.error(index, s"Variable '$name' is not a Double")
    case Constant(index, value) => value.toDoubleOption match
      case Some(v) => Interpreted.Success(v)
      case None => Interpreted.error(index, s"Constant '$value' cannot be parsed as a Double")
    case l: Lambda => Interpreted.error(l.index, s"Lambda expression cannot be parsed as a Double")
    case a@Application(index, fun, args) =>
      given Application = a // puts the current application in the context for removing some boilerplate
      fun match
        case "+" => ensureTwo(args.map(interpretAsDouble)).map(_ + _)
        case "-" => ensureTwo(args.map(interpretAsDouble)).map(_ - _)
        case "*" => ensureTwo(args.map(interpretAsDouble)).map(_ * _)
        case "/" => ensureTwo(args.map(interpretAsDouble)).map(_ / _)
        case "min" => ensureNonEmpty(args.map(interpretAsDouble)).map(_.reduce(math.min))
        case "max" => ensureNonEmpty(args.map(interpretAsDouble)).map(_.reduce(math.max))
        case "log" => ensureOne(args.map(interpretAsDouble)).map(math.log)
        // the following functions are supported because doubles can sometimes use int contexts
        case "div"   => ensureTwo(args.map(interpretAsInt)).map(_ / _).map(v => v)
        case "floor" => ensureOne(args.map(interpretAsDouble)).map(math.floor)
        case "ceil"  => ensureOne(args.map(interpretAsDouble)).map(math.ceil)
        case "round" => ensureOne(args.map(interpretAsDouble)).map(roundDbl)
        case _ => Interpreted.error(index, s"Unknown function in the Double context: '$fun'")
  
  private def interpretAsIntIntFunction(varName: Option[String])(e: MathExpression): Interpreted[Int => Int] =
    // First, try to greedily parse this as int
    interpretAsInt(e) match
      case Interpreted.Success(v) =>
        Interpreted.Success((_: Int) => v)
      case f: Interpreted.Failure => e match
        // and only if greedily parsing as int fails, try to parse as a function
        case Variable(index, name) => varName match
          case Some(vn) if name == vn => Interpreted.Success((n: Int) => n)
          case _ => Interpreted.error(index, s"Variable '$name' is not known")
        case Constant(index, value) => f
        case Lambda(index, name, expr) => varName match
          case Some(vn) => Interpreted.error(index, "Nested lambda expressions are not supported")
          case None => interpretAsIntIntFunction(Some(name))(expr)
        case a@Application(index, fun, args) =>
          given Application = a // puts the current application in the context for removing some boilerplate
          fun match
            case "+" => ensureTwo(args.map(interpretAsIntIntFunction(varName))).map(lift[Int, Int](_ + _).tupled)
            case "-" => ensureTwo(args.map(interpretAsIntIntFunction(varName))).map(lift[Int, Int](_ - _).tupled)
            case "*" => ensureTwo(args.map(interpretAsIntIntFunction(varName))).map(lift[Int, Int](_ * _).tupled)
            case "div" => ensureTwo(args.map(interpretAsIntIntFunction(varName))).map(lift[Int, Int](_ / _).tupled)
            case "min" => ensureNonEmpty(args.map(interpretAsIntIntFunction(varName))).map(_.reduce(lift(math.min)))
            case "max" => ensureNonEmpty(args.map(interpretAsIntIntFunction(varName))).map(_.reduce(lift(math.max)))
            case "floor" => ensureOne(args.map(interpretAsIntDoubleFunction(varName))).map(lift(v => math.floor(v).toInt))
            case "ceil"  => ensureOne(args.map(interpretAsIntDoubleFunction(varName))).map(lift(v => math.ceil(v).toInt))
            case "round" => ensureOne(args.map(interpretAsIntDoubleFunction(varName))).map(lift(v => roundDbl(v).toInt))
            case _ => Interpreted.error(index, s"Unknown function in the Int=>Int context: '$fun'")
  
  private def interpretAsIntDoubleFunction(varName: Option[String])(e: MathExpression): Interpreted[Int => Double] =
    // First, try to greedily parse this as double
    interpretAsDouble(e) match
      case Interpreted.Success(v) => Interpreted.Success((_: Int) => v)
      case f: Interpreted.Failure => e match
        // and only if greedily parsing as double fails, try to parse as a function
        case Variable(index, name) => varName match
          case Some(vn) if name == vn => Interpreted.Success((n: Int) => n.toDouble)
          case _ => Interpreted.error(index, s"Variable '$name' is not known")
        case Constant(index, value) => f
        case Lambda(index, name, expr) => varName match
          case Some(vn) => Interpreted.error(index, "Nested lambda expressions are not supported")
          case None => interpretAsIntDoubleFunction(Some(name))(expr)
        case a@Application(index, fun, args) =>
          given Application = a // puts the current application in the context for removing some boilerplate
          fun match
            case "+" => ensureTwo(args.map(interpretAsIntDoubleFunction(varName))).map(lift[Int, Double](_ + _).tupled)
            case "-" => ensureTwo(args.map(interpretAsIntDoubleFunction(varName))).map(lift[Int, Double](_ - _).tupled)
            case "*" => ensureTwo(args.map(interpretAsIntDoubleFunction(varName))).map(lift[Int, Double](_ * _).tupled)
            case "/" => ensureTwo(args.map(interpretAsIntDoubleFunction(varName))).map(lift[Int, Double](_ / _).tupled)
            case "min" => ensureNonEmpty(args.map(interpretAsIntDoubleFunction(varName))).map(_.reduce(lift((a, b) => math.min(a, b))))
            case "max" => ensureNonEmpty(args.map(interpretAsIntDoubleFunction(varName))).map(_.reduce(lift((a, b) => math.max(a, b))))
            case "log" => ensureOne(args.map(interpretAsIntDoubleFunction(varName))).map(lift(math.log))
            // integer division and rounding functions still useful here
            case "floor" => ensureOne(args.map(interpretAsIntDoubleFunction(varName))).map(lift(math.floor))
            case "ceil"  => ensureOne(args.map(interpretAsIntDoubleFunction(varName))).map(lift(math.ceil))
            case "round" => ensureOne(args.map(interpretAsIntDoubleFunction(varName))).map(lift(roundDbl))
            case "div" => ensureTwo(args.map(interpretAsIntIntFunction(varName))).map(lift[Int, Int](_ / _).tupled).map(f => (v: Int) => f(v).toDouble)
            case _ => Interpreted.error(index, s"Unknown function in the Int=>Double context: '$fun'")
  
  private def interpretAsIntDistributionFunction(varName: Option[String])(e: MathExpression): Interpreted[Int => IntegerDistribution] =
    // First, try to interpret this as int => int
    interpretAsIntIntFunction(varName)(e) match
      case Interpreted.Success(v) => Interpreted.Success((n: Int) => ConstantDistribution(v(n)))
      case f: Interpreted.Failure => e match
        case a@Application(index, fun, args) =>
          given Application = a
          fun match
            case "+" => ensureTwo(args.map(interpretAsIntDistributionFunction(varName))).map(lift[Int, IntegerDistribution](_ + _).tupled)
            case "-" => ensureTwo(args.map(interpretAsIntDistributionFunction(varName))).map(lift[Int, IntegerDistribution](_ - _).tupled)
            case "*" => ensureTwo(args.map(interpretAsIntDistributionFunction(varName))).map(lift[Int, IntegerDistribution](_ * _).tupled)
            case "symmetric" => ensureOne(args.map(interpretAsIntDistributionFunction(varName))).map(lift(_.symmetric))
            case "uniform" => ensureTwo(args.map(interpretAsIntIntFunction(varName))).map:
              (min, max) => (n: Int) => UniformDistribution(min(n), max(n))
            case "powerLaw" =>
              if args.length == 2 then
                (interpretAsIntIntFunction(varName)(args(0)), interpretAsIntDoubleFunction(varName)(args(1))).conjoinTuple.map:
                  (size, beta) => (n: Int) => PowerLawDistribution(size(n), beta(n))
              else Interpreted.error(index, s"'powerLaw' expects two arguments: maximum number n, exponent beta")
            case "binomial" =>
              if args.length == 2 then
                (interpretAsIntIntFunction(varName)(args(0)), interpretAsIntDoubleFunction(varName)(args(1))).conjoinTuple.map:
                  (size, p) => (n: Int) => BinomialDistribution(size(n), p(n))
              else Interpreted.error(index, s"'binomial' expects two arguments: number of trials n, probability p")
            case _ => Interpreted.error(index, s"Unknown function in the Int=>IntegerDistribution context: '$fun'")
        case Lambda(index, name, expr) => varName match
          case Some(vn) => Interpreted.error(index, "Nested lambda expressions are not supported")
          case None => interpretAsIntDistributionFunction(Some(name))(expr)
        case _ => f // variables and constants should have been parsed via int=>int

  // Givens for the next phase
  
  private trait MathInterpreter[T]:
    def interpret(e: MathExpression): Interpreted[T]
    def requiredType: String
  
  private def interpretMath[T: MathInterpreter](e: MathExpression): Interpreted[T] = summon[MathInterpreter[T]].interpret(e)
  
  private given mathInt_Int: MathInterpreter[Int]:
    override def interpret(e: MathExpression): Interpreted[Int] = interpretAsInt(e)
    override def requiredType: String = "Int"
  private given mathInt_Double: MathInterpreter[Double]:
    override def interpret(e: MathExpression): Interpreted[Double] = interpretAsDouble(e)
    override def requiredType: String = "Double"
  private given mathInt_IntInt: MathInterpreter[Int => Int]:
    override def interpret(e: MathExpression): Interpreted[Int => Int] = interpretAsIntIntFunction(None)(e)
    override def requiredType: String = "Int => Int"
  private given mathInt_IntDouble: MathInterpreter[Int => Double]:
    override def interpret(e: MathExpression): Interpreted[Int => Double] = interpretAsIntDoubleFunction(None)(e)
    override def requiredType: String = "Int => Double"
  private given mathInt_IntIntDist: MathInterpreter[Int => IntegerDistribution]:
    override def interpret(e: MathExpression): Interpreted[Int => IntegerDistribution] = interpretAsIntDistributionFunction(None)(e)
    override def requiredType: String = "Int => IntegerDistribution"
  
  // Math expressions: External API
  
  def evaluateAsInt(expr: String): Either[String, Int] =
    parse(expr, mathExpressionExact(using _)) match
      case Success(tree, _) => interpretAsInt(tree) match
        case Interpreted.Success(v) => Right(v)
        case Interpreted.Failure(e) => Left(prettyPrintErrors(expr)(e))
      case f: Failure => Left(f.trace().longAggregateMsg)
  
  def evaluateAsDouble(expr: String): Either[String, Double] =
    parse(expr, mathExpressionExact(using _)) match
      case Success(tree, _) => interpretAsDouble(tree) match
        case Interpreted.Success(v) => Right(v)
        case Interpreted.Failure(e) => Left(prettyPrintErrors(expr)(e))
      case f: Failure => Left(f.trace().longAggregateMsg)

  def evaluateAsIntIntFunction(expr: String): Either[String, Int => Int] =
    parse(expr, mathExpressionExact(using _)) match
      case Success(tree, _) => interpretAsIntIntFunction(None)(tree) match
        case Interpreted.Success(v) => Right(v)
        case Interpreted.Failure(e) => Left(prettyPrintErrors(expr)(e))
      case f: Failure => Left(f.trace().longAggregateMsg)
  
  def evaluateAsIntDoubleFunction(expr: String): Either[String, Int => Double] =
    parse(expr, mathExpressionExact(using _)) match
      case Success(tree, _) => interpretAsIntDoubleFunction(None)(tree) match
        case Interpreted.Success(v) => Right(v)
        case Interpreted.Failure(e) => Left(prettyPrintErrors(expr)(e))
      case f: Failure => Left(f.trace().longAggregateMsg)
      
  def evaluateAsIntDistributionFunction(expr: String): Either[String, Int => IntegerDistribution] =
    parse(expr, mathExpressionExact(using _)) match
      case Success(tree, _) => interpretAsIntDistributionFunction(None)(tree) match
        case Interpreted.Success(v) => Right(v)
        case Interpreted.Failure(e) => Left(prettyPrintErrors(expr)(e))
      case f: Failure => Left(f.trace().longAggregateMsg)
  
  // Tree expressions: Interpreters
  
  private class ParamCatcher:
    val usedParams = scala.collection.mutable.HashSet[String]()
    def record(param: String): Unit = usedParams.add(param)
  
  extension [T] (result: Interpreted[T])(using mapNode: MapNode, pc: ParamCatcher)
    private def checkExtraParams: Interpreted[T] =
      val extrasBuilder = IndexedSeq.newBuilder[Interpreted.Error]
      for (k, v) <- mapNode.values if !pc.usedParams.contains(k) do
        extrasBuilder += Interpreted.Error(mapNode.index, s"For '${mapNode.name}', parameter '$k' is unknown")
      val extras = extrasBuilder.result()
      if extras.isEmpty then result else result match
        case Interpreted.Success(v) => Interpreted.Failure(extras)
        case Interpreted.Failure(e) => Interpreted.Failure(e ++ extras)
  
  private def interpretMathParam[T: MathInterpreter](name: String)(using mapNode: MapNode, pc: ParamCatcher): Interpreted[T] =
    pc.record(name)
    mapNode.values.get(name) match
      case None => Interpreted.error(mapNode.index, s"For '${mapNode.name}', parameter '$name' is required of type '${summon[MathInterpreter[T]].requiredType}'")
      case Some(tree) => tree match
        case a: ArrayNode => Interpreted.error(a.index, s"For '${mapNode.name}', parameter '$name' cannot be an array")
        case m: MapNode => Interpreted.error(m.index, s"For '${mapNode.name}', parameter '$name' cannot be a map")
        case MathNode(index, expr) => summon[MathInterpreter[T]].interpret(expr)
  
  private def interpretAsOptimizer(expr: TreeExpression): Interpreted[Optimizer] = expr match
    case m: MathNode => Interpreted.error(m.index, "A mathematical expression cannot be interpreted as an optimizer")
    case a: ArrayNode => Interpreted.error(a.index, "An array cannot be interpreted as an optimizer")
    case given MapNode =>
      given ParamCatcher = ParamCatcher()
      summon[MapNode].name match
        case "OnePlusOneEA" =>
          val mutationDistributionSource = interpretMathParam[Int => IntegerDistribution]("mutationDistribution")
          mutationDistributionSource.map(OnePlusOneEA.apply).checkExtraParams
        case "DEGA" =>
          val mutationDistributionSource = interpretMathParam[Int => IntegerDistribution]("mutationDistribution")
          mutationDistributionSource.map(DEGAPlus.apply).checkExtraParams
        case "OnePlusLLGA" =>
          (interpretMathParam[Int => IntegerDistribution]("mutationDistribution"),
           interpretMathParam[Int => IntegerDistribution]("crossoverDistribution"),
          ).conjoinTuple.map(OnePlusLLGA.apply).checkExtraParams
        case "MuPlusOneGA" =>
          (interpretMathParam[Int]("populationSize"),
           interpretMathParam[Double]("crossoverProbability"),
           interpretMathParam[Int => IntegerDistribution]("mutationOnlyDistribution"),
           interpretMathParam[Int => IntegerDistribution]("mutationAfterCrossoverDistribution"),
          ).conjoinTuple.map(MuPlusOneGA.apply).checkExtraParams
        case "NFGA" =>
          (interpretMathParam[Int => IntegerDistribution]("mutationParentSelectionDistribution"),
           interpretMathParam[Int => IntegerDistribution]("mutationDistanceDistribution"),
           interpretMathParam[Int => IntegerDistribution]("firstParentSelectionDistribution"),
           interpretMathParam[Double]("crossoverProbability"),
           interpretMathParam[Int => IntegerDistribution]("crossoverParentMinimumDistance"),
           interpretMathParam[Int => Int]("crossoverParentMaximumDistance"),
           interpretMathParam[Int => IntegerDistribution]("secondParentSelectionDistribution"),
           interpretMathParam[Int => IntegerDistribution]("crossoverDistanceDistribution"),
          ).conjoinTuple.map(NeverForgettingGA.apply).checkExtraParams
        case _ => Interpreted.error(summon[MapNode].index, s"Unknown optimizer name: '${summon[MapNode].name}'")
  
  def evaluateAsOptimizer(expr: String): Either[String, Optimizer] =
    parse(expr, TreeParser.get(0).expressionExact(using _)) match
      case Success(tree, _) => interpretAsOptimizer(tree) match
        case Interpreted.Success(v) => Right(v)
        case Interpreted.Failure(e) => Left(prettyPrintErrors(expr)(e))
      case f: Failure => Left(f.trace().longAggregateMsg)

end SetupParser
