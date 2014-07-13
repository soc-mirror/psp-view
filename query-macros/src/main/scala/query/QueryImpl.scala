package psp
package query

class QueryImpl(val c: scala.reflect.macros.blackbox.Context) {
  def map(f: c.Tree): c.Tree = ???
  def flatMap(f: c.Tree): c.Tree = ???
  def flatten(ev: c.Tree): c.Tree = ???
  def collect(pf: c.Tree): c.Tree = ???
  def ++(that: c.Tree): c.Tree = ???
}