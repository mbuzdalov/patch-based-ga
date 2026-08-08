package com.github.mbuzdalov.patchga.main

import com.github.mbuzdalov.patchga.util.Loops
import fastparse.*
import fastparse.Parsed.{Success, Failure}

object SetupParser:
  import SingleLineWhitespace._
  
  private def plusMinusOpt[S: P]  = P(CharIn("+\\-").?)
  private def digits[$: P]        = P(CharsWhileIn("0-9"))
  private def exponent[$: P]      = P(CharIn("eE") ~~ plusMinusOpt ~~ digits)
  private def fractional[$: P]    = P("." ~~ digits)
  private def integral[$: P]      = P("0" | CharIn("1-9") ~~ digits.?)
  private def identifier[$: P]    = P(CharIn("A-Za-z") ~~ CharsWhileIn("0-9A-Za-z", min = 0))

  private sealed trait Expression
  private case class Variable(pos: Int, name: String) extends Expression
  private case class Constant(pos: Int, value: String) extends Expression
  private case class Application(pos: Int, function: String, arguments: IArray[Expression]) extends Expression
  
  private def roundDbl(a: Double): Double = math.round(a).toDouble
  
  private def unwrapInfix(tree: (Expression, Seq[(Int, String, Expression)])): Expression =
    tree._2.foldLeft(tree._1)((left, arg) => Application(arg._1, arg._2, IArray(left, arg._3)))
  
  private def unwrapInfix2(tree: (Int, String, Expression, Seq[(Int, String, Expression)])): Expression =
    val newHead = tree._2 match
      case "" | "+" => tree._3
      case "-" => Application(tree._1, "-", IArray(Constant(tree._1, "0"), tree._3))
      case _ => throw AssertionError(s"tree._1 is '${tree._1}'")
    unwrapInfix((newHead, tree._4))
  
  private def nnConstant[S: P]: P[Expression] = P(
    Index ~~ (integral ~~ fractional.? ~~ exponent.?).!
  ).map((index, value) => Constant(index, value))

  private def variableOrApplication[$: P]: P[Expression] = P(
    Index ~~ identifier.! ~ ("(" ~/ expression.rep(sep = ",") ~ ")").?
  ).map: (index, id, maybeArgs) =>
    maybeArgs match
      case None => Variable(index, id)
      case Some(args) => Application(index, id, IArray(args*))
  
  private def parentheses[$: P]: P[Expression] = P("(" ~/ expression ~ ")")
  private def factor[$: P]: P[Expression] = P(variableOrApplication | parentheses | nnConstant)
  private def product[$: P]: P[Expression] = P(factor ~ (Index ~ StringIn("*", "/", "div").! ~/ factor).rep).map(unwrapInfix)
  private def sum[$: P]: P[Expression] = P(Index ~ plusMinusOpt.! ~ product ~ (Index ~ CharIn("+\\-").! ~/ product).rep).map(unwrapInfix2)
  private def expression[$: P]: P[Expression] = P(sum)
  private def exactExpression[$: P]: P[Expression] = P(Start ~ expression ~ End)
  
  private case class ErrorMessage(index: Int, message: String)
  private type Errors = IndexedSeq[ErrorMessage]
  private def error(index: Int, message: String) = Left(IndexedSeq(ErrorMessage(index, message)))
  
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
  
  private inline def lift[A, U, V](inline op: U => V): (A => U) => A => V =
    a => (x: A) => op(a(x))
  
  private inline def lift[A, R](inline op: (R, R) => R): (A => R, A => R) => A => R =
    (a, b) => (x: A) => op(a(x), b(x))
  
  private inline def unaryOp[A, R](index: Int, name: String, args: IArray[Expression],
                                   inline extractor: Expression => Either[Errors, A],
                                   inline op: A => R): Either[Errors, R] =
    if args.length == 1
    then extractor(args(0)) match
      case Left(e) => Left(e)
      case Right(v) => Right(op(v))
    else error(index, s"'$name' requires one argument")
  
  private inline def foldOp[T](index: Int, isBinary: Boolean, name: String, args: IArray[Expression],
                               inline extractor: Expression => Either[Errors, T],
                               inline op: (T, T) => T): Either[Errors, T] =
    if isBinary && args.length != 2 then error(index, s"'$name' requires two arguments")
    else if !isBinary && args.length == 0 then error(index, s"'$name' requires at least one argument")
    else args.tail.foldLeft(extractor(args(0))): (left, right) =>
      left match
        case Left(e1) => extractor(right) match
          case Left(e2) => Left(e1 ++ e2)
          case Right(v) => Left(e1)
        case Right(lv) => extractor(right) match
          case Left(e2) => Left(e2)
          case Right(rv) => Right(op(lv, rv))
  
  private def interpretAsInt(e: Expression): Either[Errors, Int] = e match
    case Variable(index, name) => error(index, s"Variable '$name' is not an Int")
    case Constant(index, value) => value.toIntOption match
      case Some(v) => Right(v)
      case None => error(index, s"Constant '$value' cannot be parsed as an Int")
    case Application(index, fun, args) => fun match
      case "+" => foldOp(index, isBinary = true, "+", args, interpretAsInt, _ + _)
      case "-" => foldOp(index, isBinary = true, "-", args, interpretAsInt, _ - _)
      case "*" => foldOp(index, isBinary = true, "*", args, interpretAsInt, _ * _)
      case "div" => foldOp(index, isBinary = true, "div", args, interpretAsInt, _ / _)
      case "min" => foldOp(index, isBinary = false, "min", args, interpretAsInt, math.min)
      case "max" => foldOp(index, isBinary = false, "max", args, interpretAsInt, math.max)
      case "floor" => unaryOp(index, "floor", args, interpretAsDouble, v => math.floor(v).toInt)
      case "ceil" => unaryOp(index, "ceil", args, interpretAsDouble, v => math.ceil(v).toInt)
      case "round" => unaryOp(index, "round", args, interpretAsDouble, v => roundDbl(v).toInt)
      case _ => error(index, s"Unknown function '$fun'")
  
  private def interpretAsDouble(e: Expression): Either[Errors, Double] = e match
    case Variable(index, name) => error(index, s"Variable '$name' is not a Double")
    case Constant(index, value) => value.toDoubleOption match
      case Some(v) => Right(v)
      case None => error(index, s"Constant '$value' cannot be parsed as a Double")
    case Application(index, fun, args) => fun match
      case "+" => foldOp(index, isBinary = true, "+", args, interpretAsDouble, _ + _)
      case "-" => foldOp(index, isBinary = true, "-", args, interpretAsDouble, _ - _)
      case "*" => foldOp(index, isBinary = true, "*", args, interpretAsDouble, _ * _)
      case "/" => foldOp(index, isBinary = true, "/", args, interpretAsDouble, _ / _)
      case "min" => foldOp(index, isBinary = false, "min", args, interpretAsDouble, math.min)
      case "max" => foldOp(index, isBinary = false, "max", args, interpretAsDouble, math.max)
      case "log" => unaryOp(index, "log", args, interpretAsDouble, math.log)
      // the following functions are supported because doubles can sometimes use int contexts
      case "div" => foldOp(index, isBinary = true, "div", args, interpretAsInt, _ / _).map(v => v)
      case "floor" => unaryOp(index, "floor", args, interpretAsDouble, math.floor)
      case "ceil" => unaryOp(index, "ceil", args, interpretAsDouble, math.ceil)
      case "round" => unaryOp(index, "round", args, interpretAsDouble, roundDbl)
      case _ => error(index, s"Unknown function '$fun'")
  
  private def interpretAsIntIntFunction(varName: String)(e: Expression): Either[Errors, Int => Int] =
    // First, try to greedily parse this as int
    val intConstantResult = interpretAsInt(e)
    intConstantResult match
      case Right(v) => Right((_: Int) => v)
      case Left(intErr) => e match
        // and only if greedily parsing as int fails, try to parse as a function
        case Variable(index, `varName`) => Right((v: Int) => v)
        case Variable(index, otherName) => error(index, s"Variable '$otherName' is not known")
        case Constant(index, value) => error(index, s"Constant '$value' cannot be parsed as Int") // otherwise it's Right(v) above
        case Application(index, fun, args) => fun match
          case "+" => foldOp(index, isBinary = true, "+", args, interpretAsIntIntFunction(varName), lift(_ + _))
          case "-" => foldOp(index, isBinary = true, "-", args, interpretAsIntIntFunction(varName), lift(_ - _))
          case "*" => foldOp(index, isBinary = true, "*", args, interpretAsIntIntFunction(varName), lift(_ * _))
          case "div" => foldOp(index, isBinary = true, "div", args, interpretAsIntIntFunction(varName), lift(_ / _))
          case "min" => foldOp(index, isBinary = false, "min", args, interpretAsIntIntFunction(varName), lift(math.min))
          case "max" => foldOp(index, isBinary = false, "max", args, interpretAsIntIntFunction(varName), lift(math.max))
          case "floor" => unaryOp(index, "floor", args, interpretAsIntDoubleFunction(varName), lift(v => math.floor(v).toInt))
          case "ceil" => unaryOp(index, "ceil", args, interpretAsIntDoubleFunction(varName), lift(v => math.ceil(v).toInt))
          case "round" => unaryOp(index, "round", args, interpretAsIntDoubleFunction(varName), lift(v => roundDbl(v).toInt))
          case _ => error(index, s"Unknown function '$fun'")
  
  private def interpretAsIntDoubleFunction(varName: String)(e: Expression): Either[Errors, Int => Double] =
    // First, try to greedily parse this as double
    val doubleConstantResult = interpretAsDouble(e)
    doubleConstantResult match
      case Right(v) => Right((_: Int) => v)
      case Left(dblErr) => e match
        // and only if greedily parsing as double fails, try to parse as a function
        case Variable(index, `varName`) => Right((v: Int) => v)
        case Variable(index, otherName) => error(index, s"Variable '$otherName' is not known")
        case Constant(index, value) => error(index, s"Constant '$value' cannot be parsed as Double") // otherwise it's Right(v) above
        case Application(index, fun, args) => fun match
          case "+" => foldOp(index, isBinary = true, "+", args, interpretAsIntDoubleFunction(varName), lift(_ + _))
          case "-" => foldOp(index, isBinary = true, "-", args, interpretAsIntDoubleFunction(varName), lift(_ - _))
          case "*" => foldOp(index, isBinary = true, "*", args, interpretAsIntDoubleFunction(varName), lift(_ * _))
          case "/" => foldOp(index, isBinary = true, "/", args, interpretAsIntDoubleFunction(varName), lift(_ / _))
          case "min" => foldOp(index, isBinary = false, "min", args, interpretAsIntDoubleFunction(varName), lift((a, b) => math.min(a, b))) // could not lift just math.min
          case "max" => foldOp(index, isBinary = false, "max", args, interpretAsIntDoubleFunction(varName), lift((a, b) => math.max(a, b)))
          case "log" => unaryOp(index, "log", args, interpretAsIntDoubleFunction(varName), lift(math.log))
          // integer division and rounding functions still useful here
          case "floor" => unaryOp(index, "floor", args, interpretAsIntDoubleFunction(varName), lift(math.floor))
          case "ceil" => unaryOp(index, "ceil", args, interpretAsIntDoubleFunction(varName), lift(math.ceil))
          case "round" => unaryOp(index, "round", args, interpretAsIntDoubleFunction(varName), lift(roundDbl))
          case "div" => foldOp(index, isBinary = true, "/", args, interpretAsIntIntFunction(varName), lift(_ / _)).map(f => (v: Int) => f(v).toDouble)
  
  def evaluateAsInt(expr: String): Either[String, Int] =
    parse(expr, exactExpression(using _)) match
      case Success(tree, _) => interpretAsInt(tree).left.map(prettyPrintErrors(expr))
      case f: Failure => Left(f.trace().longAggregateMsg)
  
  def evaluateAsDouble(expr: String): Either[String, Double] =
    parse(expr, exactExpression(using _)) match
      case Success(tree, _) => interpretAsDouble(tree).left.map(prettyPrintErrors(expr))
      case f: Failure => Left(f.trace().longAggregateMsg)

  def evaluateAsIntIntFunction(expr: String, varName: String): Either[String, Int => Int] =
    parse(expr, exactExpression(using _)) match
      case Success(tree, _) => interpretAsIntIntFunction(varName)(tree).left.map(prettyPrintErrors(expr))
      case f: Failure => Left(f.trace().longAggregateMsg)
  
  def evaluateAsIntDoubleFunction(expr: String, varName: String): Either[String, Int => Double] =
    parse(expr, exactExpression(using _)) match
      case Success(tree, _) => interpretAsIntDoubleFunction(varName)(tree).left.map(prettyPrintErrors(expr))
      case f: Failure => Left(f.trace().longAggregateMsg)
end SetupParser
