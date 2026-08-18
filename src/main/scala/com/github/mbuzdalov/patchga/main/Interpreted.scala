package com.github.mbuzdalov.patchga.main

import scala.collection.IterableOps
import scala.collection.immutable.VectorBuilder

sealed trait Interpreted[+T]:
  def map[U](fun: T => U): Interpreted[U]
  def flatMap[U](fun: T => Interpreted[U]): Interpreted[U]

object Interpreted:
  case class Success[+T](result: T) extends Interpreted[T]:
    override def map[U](fun: T => U): Interpreted[U] = Success(fun(result))
    override def flatMap[U](fun: T => Interpreted[U]): Interpreted[U] = fun(result)

  case class Failure(errors: IndexedSeq[Error]) extends Interpreted[Nothing]:
    override def map[U](fun: Nothing => U): Interpreted[U] = this
    override def flatMap[U](fun: Nothing => Interpreted[U]): Interpreted[U] = this

  case class Error(index: Int, message: String)
  
  def error(index: Int, message: String): Failure = Failure(IndexedSeq(Error(index, message)))

  extension [A, CC[_], C] (seq: IterableOps[Interpreted[A], CC, C])
    def conjoinSeq: Interpreted[CC[A]] =
      val errorBuilder = IndexedSeq.newBuilder[Error]
      val successBuilder = seq.iterableFactory.newBuilder[A]
      seq.foreach:
        case Success(v) => successBuilder += v
        case Failure(e) => errorBuilder ++= e
      val errors = errorBuilder.result()
      if errors.isEmpty then Success(successBuilder.result()) else Failure(errors)
  
  //noinspection ScalaWeakerAccess: this cannot be private because it's the return type of a public thing
  type UnInterpreted[T <: Tuple] <: Tuple = T match
    case EmptyTuple => EmptyTuple
    case Interpreted[a] *: b => a *: UnInterpreted[b]
  
  trait TupleIsInterpreted[T <: Tuple]:
    def collectErrors(input: T, builder: VectorBuilder[Error]): Unit
    def conjoin(input: T): Interpreted[UnInterpreted[T]]
  
  given TupleIsInterpreted[EmptyTuple]:
    override def collectErrors(input: EmptyTuple, builder: VectorBuilder[Error]): Unit = ()
    override def conjoin(input: EmptyTuple): Interpreted[EmptyTuple] = Success(input)
    
  given rec_tuple_generic: [A, T <: Tuple] => (sub: TupleIsInterpreted[T]) => TupleIsInterpreted[Interpreted[A] *: T]:
    override def collectErrors(input: Interpreted[A] *: T, builder: VectorBuilder[Error]): Unit = input match
      case ia *: tt =>
        ia match
          case Success(v) =>
          case Failure(e) => builder ++= e
        sub.collectErrors(tt, builder)
    
    override def conjoin(input: Interpreted[A] *: T): Interpreted[A *: UnInterpreted[T]] = input match
      case ia *: tt => ia match
        case Success(v) => sub.conjoin(tt) match
          case Success(vv) => Success(v *: vv)
          case f: Failure => f
        case Failure(e) =>
          val builder = VectorBuilder[Error]()
          builder ++= e
          sub.collectErrors(tt, builder)
          Failure(builder.result())
    
  given rec_tuple_success: [A, T <: Tuple] => (sub: TupleIsInterpreted[T]) => TupleIsInterpreted[Success[A] *: T]:
    override def collectErrors(input: Success[A] *: T, builder: VectorBuilder[Error]): Unit = input match
      case ia *: tt => sub.collectErrors(tt, builder)
    
    override def conjoin(input: Success[A] *: T): Interpreted[UnInterpreted[Success[A] *: T]] = input match
      case ia *: tt => sub.conjoin(tt) match
        case Success(vv) => Success(ia.result *: vv)
        case f: Failure => f
  
  given rec_tuple_failure: [T <: Tuple] => (sub: TupleIsInterpreted[T]) => TupleIsInterpreted[Failure *: T]:
    override def collectErrors(input: Failure *: T, builder: VectorBuilder[Error]): Unit = input match
      case ia *: tt =>
        builder ++= ia.errors
        sub.collectErrors(tt, builder)
    override def conjoin(input: Failure *: T): Interpreted[UnInterpreted[Failure *: T]] = input match
      case ia *: tt =>
        val builder = VectorBuilder[Error]()
        builder ++= ia.errors
        sub.collectErrors(tt, builder)
        Failure(builder.result())
  
  extension [T <: Tuple] (t: T)(using ex: TupleIsInterpreted[T])
    def conjoinTuple: Interpreted[UnInterpreted[T]] = ex.conjoin(t)
    