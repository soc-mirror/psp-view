package psp
package query

import scala.language.experimental.macros

import psp.api
import psp.core.{ =?>, Builds, Interval, Predicate, Size, DirectAccessType, ForeachableType, SequentialAccessType }
import psp.collection.{ Foreach, Direct, Linear }

class ViewEnvironment[A0, Repr, CC0[X]](val repr: Repr) extends api.ViewEnvironment[A0, Repr, CC0] {
  type A = A0
  type CC[X] = CC0[X]

  def unknownView(tc: ForeachableType[A, Repr, CC]): AtomicView     = ???
  def linearView(tc: SequentialAccessType[A, Repr, CC]): LinearView = ???
  def indexedView(tc: DirectAccessType[A, Repr, CC]): IndexedView   = ???

  sealed trait View[+A] extends Any with api.BuilderView[A, Repr] {
    type MapTo[+X] = View[X]
    // Eventually
    // type Input[X]  = Foreach[X]

    def isAtomic: Boolean = ???

    override final def map[B](f: A => B): MapTo[B] = macro QueryImpl.map
    override final def flatMap[B](f: A => Foreach[B]): MapTo[B] = macro QueryImpl.flatMap
    override final def flatten[B](implicit ev: A <:< Input[B]): MapTo[B] = macro QueryImpl.flatten
    override final def collect[B](pf: A =?> B): MapTo[B] = macro QueryImpl.collect
    override final def ++[A1 >: A](that: Foreach[A1]): MapTo[A1] = macro QueryImpl.++

    override final def withFilter(p: Predicate[A]): MapTo[A] = ???
    override final def filter(p: Predicate[A]): MapTo[A] = ???
    override final def filterNot(p: Predicate[A]): MapTo[A] = ???
    override final def drop(n: Int): MapTo[A] = ???
    override final def take(n: Int): MapTo[A] = ???
    override final def takeWhile(p: Predicate[A]): MapTo[A] = ???
    override final def dropWhile(p: Predicate[A]): MapTo[A] = ???
    override final def dropRight(n: Int): MapTo[A] = ???
    override final def takeRight(n: Int): MapTo[A] = ???
    override final def slice(range: Interval): MapTo[A] = ???
    override final def labeled(label: String): MapTo[A] = ???
    override final def sized(size: Size): MapTo[A] = ???
    override final def reverse: MapTo[A] = ???
    override final def sliding(size: Size, step: Int): MapTo[Cluster[A]] = ???
    override final def groupBy[B](f: A => B): MapTo[(B, Cluster[A])] = ???

    override final def native(implicit pcb: Builds[A, Repr]): Repr = force[Repr]
    override final def force[That](implicit pcb: Builds[A, That]): That = pcb build this

    override def toString = viewChain reverseMap (_.description) mkString " "
  }

  type AtomicView <: View[A]
  type LinearView <: AtomicView with Linear[A]
  type IndexedView <: AtomicView with Direct[A]
}
