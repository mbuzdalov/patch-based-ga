package com.github.mbuzdalov.patchga.main

import com.github.mbuzdalov.patchga.distribution.*
import com.github.mbuzdalov.patchga.util.Loops
import fastparse.*
import fastparse.Parsed.{Failure, Success}

import scala.reflect.ClassTag

object SetupParser:
  import SingleLineWhitespace.*
  
  // General definitions for all parsers

  private def plusMinusOpt[S: P] = P(CharIn("+\\-").?)
  private def digits[$: P] = P(CharsWhileIn("0-9"))
  private def exponent[$: P] = P(CharIn("eE") ~~ plusMinusOpt ~~ digits)
  private def fractional[$: P] = P("." ~~ digits)
  private def integral[$: P] = P("0" | CharIn("1-9") ~~ digits.?)
  private def identifier[$: P] = P(CharIn("A-Za-z") ~~ CharsWhileIn("0-9A-Za-z", min = 0))
  private def nnConstant[S: P] = P(integral ~~ fractional.? ~~ exponent.?)
  
  // Distributions: Intermediate representation structures
  
  private sealed trait Expression
  private case class Variable(pos: Int, name: String) extends Expression
  private case class Constant(pos: Int, value: String) extends Expression
  private case class Application(pos: Int, function: String, arguments: IArray[Expression]) extends Expression

  // Distribution: Parser
  
  private object DistributionParser:
    private def unwrapInfix(tree: (Expression, Seq[(Int, String, Expression)])): Expression =
      tree._2.foldLeft(tree._1)((left, arg) => Application(arg._1, arg._2, IArray(left, arg._3)))
    
    private def unwrapInfix2(tree: (Int, String, Expression, Seq[(Int, String, Expression)])): Expression =
      val newHead = tree._2 match
        case "" | "+" => tree._3
        case "-" => Application(tree._1, "-", IArray(Constant(tree._1, "0"), tree._3))
        case _ => throw AssertionError(s"tree._1 is '${tree._1}'")
      unwrapInfix((newHead, tree._4))
    
    private def nnConstantExp[S: P]: P[Expression] = P(Index ~~ nnConstant.!).map((index, value) => Constant(index, value))
    private def variableOrApplication[$: P]: P[Expression] = P(
      Index ~~ identifier.! ~ ("(" ~/ expression.rep(sep = ",") ~ ")").?
    ).map: (index, id, maybeArgs) =>
      maybeArgs match
        case None => Variable(index, id)
        case Some(args) => Application(index, id, IArray(args*))
    
    private def parentheses[$: P]: P[Expression] = P("(" ~/ expression ~ ")")
    private def factor[$: P]: P[Expression] = P(variableOrApplication | parentheses | nnConstantExp)
    private def product[$: P]: P[Expression] = P(factor ~ (Index ~ StringIn("*", "/", "div").! ~/ factor).rep).map(unwrapInfix)
    private def sum[$: P]: P[Expression] = P(Index ~ plusMinusOpt.! ~ product ~ (Index ~ CharIn("+\\-").! ~/ product).rep).map(unwrapInfix2)

    // un-private this if/when the downstream parser needs it
    private def expression[$: P]: P[Expression] = P(sum)
    def exactExpression[$: P]: P[Expression] = P(Start ~ expression ~ End)
  
  // Distribution: Interpretation error reporting machinery
  
  private case class ErrorMessage(index: Int, message: String)
  private type Errors = IndexedSeq[ErrorMessage]
  private def error(index: Int, message: String) = Left(IndexedSeq(ErrorMessage(index, message)))
  
  private def prependError[T](index: Int, message: String, others: IArray[Either[Errors, T]]): Left[Errors, Nothing] =
    val errors = IndexedSeq.newBuilder[ErrorMessage]
    errors += ErrorMessage(index, message)
    others.foreach:
      case Left(error) => errors ++= error
      case Right(v) =>
    Left(errors.result())
  
  private def prettyPrintErrors[T](text: String)(errors: Errors): String =
    val builder = StringBuilder()
    for e <- errors do
      builder.append(text).append('\n')
      Loops.repeat(e.index)(builder.append(' '))
      builder.append("^\n")
      builder.append(s"Error at index ${e.index + 1}: ${e.message}\n")
    builder.result()
  
  private def collectErrors(seq: Seq[Either[Errors, Any]]): Left[Errors, Nothing] =
    val errors = IndexedSeq.newBuilder[ErrorMessage]
    for a <- seq do a match
      case Left(e) => errors.addAll(e)
      case _ =>
    Left(errors.result())
  
  // Distribution: Interpretation special functions

  private def roundDbl(a: Double): Double = math.round(a).toDouble

  // Distribution: Some kind of metaprogramming
  
  private inline def lift[A, U, V](inline op: U => V): (A => U) => A => V =
    a => (x: A) => op(a(x))
  
  private inline def lift[A, R](inline op: (R, R) => R): (A => R, A => R) => A => R =
    (a, b) => (x: A) => op(a(x), b(x))
  
  private def ensureNonEmpty(args: IArray[Expression])(using application: Application): Either[Errors, IArray[Expression]] =
    if args.length > 0 then Right(args) else error(application.pos, s"'${application.function}' requires at least one argument")
  
  private def ensureOne(args: IArray[Expression])(using application: Application): Either[Errors, Expression] =
    if args.length == 1 then Right(args(0)) else error(application.pos, s"'${application.function}' requires one argument")
  
  private def ensureTwo(args: IArray[Expression])(using application: Application): Either[Errors, (Expression, Expression)] =
    if args.length == 2 then Right(args(0), args(1))
    else error(application.pos, s"'${application.function}' requires two arguments")
  
  extension[T: ClassTag](interpreter: Expression => Either[Errors, T])
    private def forPair(arg: (Expression, Expression)): Either[Errors, (T, T)] =
      interpreter(arg._1) match
        case Left(e1) => interpreter(arg._2) match
          case Left(e2) => Left(e1 ++ e2)
          case Right(v2) => Left(e1)
        case Right(v1) => interpreter(arg._2) match
          case Left(e2) => Left(e2)
          case Right(v2) => Right(v1, v2)

    private def forSeq(arg: IArray[Expression]): Either[Errors, IArray[T]] =
      val errorBuilder = IndexedSeq.newBuilder[ErrorMessage]
      val result = Array.ofDim[T](arg.length)
      Loops.foreach(0, arg.length): i =>
        interpreter(arg(i)) match
          case Left(e) => errorBuilder ++= e
          case Right(v) => result(i) = v
      val errors = errorBuilder.result()
      if errors.isEmpty then Right(IArray.unsafeFromArray(result)) else Left(errors)
  
  extension[A, B](pair: (Expression => Either[Errors, A], Expression => Either[Errors, B]))
    private def lift(arg: (Expression, Expression)): Either[Errors, (A, B)] =
      pair._1(arg._1) match
        case Left(e1) => pair._2(arg._2) match
          case Left(e2) => Left(e1 ++ e2)
          case Right(v2) => Left(e1)
        case Right(v1) => pair._2(arg._2) match
          case Left(e2) => Left(e2)
          case Right(v2) => Right(v1, v2)
  
  // Distribution: Actual interpreters
  
  private def interpretAsInt(e: Expression): Either[Errors, Int] = e match
    case Variable(index, name) => error(index, s"Variable '$name' is not an Int")
    case Constant(index, value) => value.toIntOption match
      case Some(v) => Right(v)
      case None => error(index, s"Constant '$value' cannot be parsed as an Int")
    case a@Application(index, fun, args) =>
      given Application = a // puts the current application in the context for removing some boilerplate
      fun match
        case "+" => ensureTwo(args).map(interpretAsInt.forPair).joinRight.map(_ + _)
        case "-" => ensureTwo(args).map(interpretAsInt.forPair).joinRight.map(_ - _)
        case "*" => ensureTwo(args).map(interpretAsInt.forPair).joinRight.map(_ * _)
        case "div" => ensureTwo(args).map(interpretAsInt.forPair).joinRight.map(_ / _)
        case "min" => ensureNonEmpty(args).map(interpretAsInt.forSeq).joinRight.map(_.reduce(math.min))
        case "max" => ensureNonEmpty(args).map(interpretAsInt.forSeq).joinRight.map(_.reduce(math.max))
        case "floor" => ensureOne(args).map(interpretAsDouble).joinRight.map(v => math.floor(v).toInt)
        case "ceil"  => ensureOne(args).map(interpretAsDouble).joinRight.map(v => math.ceil(v).toInt)
        case "round" => ensureOne(args).map(interpretAsDouble).joinRight.map(v => roundDbl(v).toInt)
        case _ => error(index, s"Unknown function '$fun'")
  
  private def interpretAsDouble(e: Expression): Either[Errors, Double] = e match
    case Variable(index, name) => error(index, s"Variable '$name' is not a Double")
    case Constant(index, value) => value.toDoubleOption match
      case Some(v) => Right(v)
      case None => error(index, s"Constant '$value' cannot be parsed as a Double")
    case a@Application(index, fun, args) =>
      given Application = a // puts the current application in the context for removing some boilerplate
      fun match
        case "+" => ensureTwo(args).map(interpretAsDouble.forPair).joinRight.map(_ + _)
        case "-" => ensureTwo(args).map(interpretAsDouble.forPair).joinRight.map(_ - _)
        case "*" => ensureTwo(args).map(interpretAsDouble.forPair).joinRight.map(_ * _)
        case "/" => ensureTwo(args).map(interpretAsDouble.forPair).joinRight.map(_ / _)
        case "min" => ensureNonEmpty(args).map(interpretAsDouble.forSeq).joinRight.map(_.reduce(math.min))
        case "max" => ensureNonEmpty(args).map(interpretAsDouble.forSeq).joinRight.map(_.reduce(math.max))
        case "log" => ensureOne(args).map(interpretAsDouble).joinRight.map(math.log)
        // the following functions are supported because doubles can sometimes use int contexts
        case "div"   => ensureTwo(args).map(interpretAsInt.forPair).joinRight.map(_ / _).map(v => v)
        case "floor" => ensureOne(args).map(interpretAsDouble).joinRight.map(math.floor)
        case "ceil"  => ensureOne(args).map(interpretAsDouble).joinRight.map(math.ceil)
        case "round" => ensureOne(args).map(interpretAsDouble).joinRight.map(roundDbl)
        case _ => error(index, s"Unknown function '$fun'")
  
  private def interpretAsIntIntFunction(varName: String)(e: Expression): Either[Errors, Int => Int] =
    // First, try to greedily parse this as int
    interpretAsInt(e) match
      case Right(v) => Right((_: Int) => v)
      case Left(intErr) => e match
        // and only if greedily parsing as int fails, try to parse as a function
        case Variable(index, `varName`) => Right((v: Int) => v)
        case Variable(index, otherName) => error(index, s"Variable '$otherName' is not known")
        case Constant(index, value) => Left(intErr)
        case a@Application(index, fun, args) =>
          given Application = a // puts the current application in the context for removing some boilerplate
          fun match
            case "+" => ensureTwo(args).map(interpretAsIntIntFunction(varName).forPair).joinRight.map(lift[Int, Int](_ + _).tupled)
            case "-" => ensureTwo(args).map(interpretAsIntIntFunction(varName).forPair).joinRight.map(lift[Int, Int](_ - _).tupled)
            case "*" => ensureTwo(args).map(interpretAsIntIntFunction(varName).forPair).joinRight.map(lift[Int, Int](_ * _).tupled)
            case "div" => ensureTwo(args).map(interpretAsIntIntFunction(varName).forPair).joinRight.map(lift[Int, Int](_ / _).tupled)
            case "min" => ensureNonEmpty(args).map(interpretAsIntIntFunction(varName).forSeq).joinRight.map(_.reduce(lift(math.min)))
            case "max" => ensureNonEmpty(args).map(interpretAsIntIntFunction(varName).forSeq).joinRight.map(_.reduce(lift(math.max)))
            case "floor" => ensureOne(args).map(interpretAsIntDoubleFunction(varName)).joinRight.map(lift(v => math.floor(v).toInt))
            case "ceil"  => ensureOne(args).map(interpretAsIntDoubleFunction(varName)).joinRight.map(lift(v => math.ceil(v).toInt))
            case "round" => ensureOne(args).map(interpretAsIntDoubleFunction(varName)).joinRight.map(lift(v => roundDbl(v).toInt))
            case _ => error(index, s"Unknown function '$fun'")
  
  private def interpretAsIntDoubleFunction(varName: String)(e: Expression): Either[Errors, Int => Double] =
    // First, try to greedily parse this as double
    interpretAsDouble(e) match
      case Right(v) => Right((_: Int) => v)
      case Left(dblErr) => e match
        // and only if greedily parsing as double fails, try to parse as a function
        case Variable(index, `varName`) => Right((v: Int) => v)
        case Variable(index, otherName) => error(index, s"Variable '$otherName' is not known")
        case Constant(index, value) => Left(dblErr)
        case a@Application(index, fun, args) =>
          given Application = a // puts the current application in the context for removing some boilerplate
          fun match
            case "+" => ensureTwo(args).map(interpretAsIntDoubleFunction(varName).forPair).joinRight.map(lift[Int, Double](_ + _).tupled)
            case "-" => ensureTwo(args).map(interpretAsIntDoubleFunction(varName).forPair).joinRight.map(lift[Int, Double](_ - _).tupled)
            case "*" => ensureTwo(args).map(interpretAsIntDoubleFunction(varName).forPair).joinRight.map(lift[Int, Double](_ * _).tupled)
            case "/" => ensureTwo(args).map(interpretAsIntDoubleFunction(varName).forPair).joinRight.map(lift[Int, Double](_ / _).tupled)
            case "min" => ensureNonEmpty(args).map(interpretAsIntDoubleFunction(varName).forSeq).joinRight.map(_.reduce(lift((a, b) => math.min(a, b))))
            case "max" => ensureNonEmpty(args).map(interpretAsIntDoubleFunction(varName).forSeq).joinRight.map(_.reduce(lift((a, b) => math.max(a, b))))
            case "log" => ensureOne(args).map(interpretAsIntDoubleFunction(varName)).joinRight.map(lift(math.log))
            // integer division and rounding functions still useful here
            case "floor" => ensureOne(args).map(interpretAsIntDoubleFunction(varName)).joinRight.map(lift(math.floor))
            case "ceil"  => ensureOne(args).map(interpretAsIntDoubleFunction(varName)).joinRight.map(lift(math.ceil))
            case "round" => ensureOne(args).map(interpretAsIntDoubleFunction(varName)).joinRight.map(lift(roundDbl))
            case "div" => ensureTwo(args).map(interpretAsIntIntFunction(varName).forPair).joinRight.map(lift[Int, Int](_ / _).tupled).map(f => (v: Int) => f(v).toDouble)
            case _ => error(index, s"Unknown function '$fun'")
  
  private def interpretAsIntDistributionFunction(varName: String)(e: Expression): Either[Errors, Int => IntegerDistribution] =
    // First, try to interpret this as int => int
    interpretAsIntIntFunction(varName)(e) match
      case Right(v) => Right((n: Int) => ConstantDistribution(v(n)))
      case Left(iiErr) => e match
        case a@Application(index, fun, args) =>
          given Application = a
          fun match
            case "uniform" => ensureTwo(args).map(interpretAsIntIntFunction(varName).forPair).joinRight.map:
              case (min, max) => (n: Int) => UniformDistribution(min(n), max(n))
            case "powerLaw" => ensureTwo(args).map((interpretAsIntIntFunction(varName), interpretAsIntDoubleFunction(varName)).lift).joinRight.map:
              case (size, beta) => (n: Int) => PowerLawDistribution(size(n), beta(n))
            case "binomial" => ensureTwo(args).map((interpretAsIntIntFunction(varName), interpretAsIntDoubleFunction(varName)).lift).joinRight.map:
              case (size, p) => (n: Int) => BinomialDistribution(size(n), p(n))
            case _ => error(index, s"Unknown function '$fun'")
        case _ => Left(iiErr) // variables and constants should have been parsed via int=>int
  
  // Distribution: External API
  
  def evaluateAsInt(expr: String): Either[String, Int] =
    parse(expr, DistributionParser.exactExpression(using _)) match
      case Success(tree, _) => interpretAsInt(tree).left.map(prettyPrintErrors(expr))
      case f: Failure => Left(f.trace().longAggregateMsg)
  
  def evaluateAsDouble(expr: String): Either[String, Double] =
    parse(expr, DistributionParser.exactExpression(using _)) match
      case Success(tree, _) => interpretAsDouble(tree).left.map(prettyPrintErrors(expr))
      case f: Failure => Left(f.trace().longAggregateMsg)

  def evaluateAsIntIntFunction(expr: String, varName: String): Either[String, Int => Int] =
    parse(expr, DistributionParser.exactExpression(using _)) match
      case Success(tree, _) => interpretAsIntIntFunction(varName)(tree).left.map(prettyPrintErrors(expr))
      case f: Failure => Left(f.trace().longAggregateMsg)
  
  def evaluateAsIntDoubleFunction(expr: String, varName: String): Either[String, Int => Double] =
    parse(expr, DistributionParser.exactExpression(using _)) match
      case Success(tree, _) => interpretAsIntDoubleFunction(varName)(tree).left.map(prettyPrintErrors(expr))
      case f: Failure => Left(f.trace().longAggregateMsg)
      
  def evaluateAsIntDistributionFunction(expr: String, varName: String): Either[String, Int => IntegerDistribution] =
    parse(expr, DistributionParser.exactExpression(using _)) match
      case Success(tree, _) => interpretAsIntDistributionFunction(varName)(tree).left.map(prettyPrintErrors(expr))
      case f: Failure => Left(f.trace().longAggregateMsg)
end SetupParser
