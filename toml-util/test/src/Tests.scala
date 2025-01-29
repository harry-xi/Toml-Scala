package top.harryxi.toml

import utest._

object FooTests extends TestSuite {
  case class Toml1(
      a: Int,
      table: Table1
  )
  case class Table1(
      b: Int
  )
  def tests = Tests {
    val table =
      """
      |a = 1
      |[table]
      |b = 2
      """.stripMargin
    assert(fromToml[Toml1](table) == Toml1(a = 1, table = Table1(b = 2)))
  }
}
