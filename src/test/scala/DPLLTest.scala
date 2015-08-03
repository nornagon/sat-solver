package net.nornagon.dpll

import org.scalatest._

class DPLLTest extends FlatSpec with Matchers {
  import DPLL._
  behavior of "DPLL"
  it should "solve a 3-color problem" in {
    val colors = Set("red", "green", "blue")
    val countries = Set("a", "b", "c", "d")
    val ca = countries.map { cou =>
      cou -> colors.map { col =>
        col -> Var(s"$cou is $col")
      }.toMap
    }.toMap

    // Each country may hove only one color.
    val colorExclusivity: Expr = countries map { c =>
      (for ((col1,v1) <- ca(c)) yield {
        v1 & ((for ((col2,v2) <- ca(c); if col1 != col2) yield ~v2) reduce (_&_))
      }) reduce (_|_)
    } reduce (_ & _)

    // Countries that share a border may not also share a color.
    val borders = Seq(
      (ca("a"), ca("b")),
      (ca("b"), ca("c")),
      (ca("c"), ca("d")),
      (ca("d"), ca("a"))
    )

    val adjacency = borders map { case (c1, c2) =>
      colors map { col =>
        (c1(col) & ~c2(col))
      } reduce (_ | _)
    } reduce (_ & _)

    val expr = colorExclusivity & adjacency

    val assn = solve(expr).get

    ca foreach { case (c, cols) =>
      (cols.values map (_.tvar) map assn) count (x => x) should be (1)
    }
    borders foreach { case (c1, c2) =>
      val col1 = c1.collectFirst { case (col, v) if assn(v.tvar) => col }.get
      val col2 = c2.collectFirst { case (col, v) if assn(v.tvar) => col }.get
      col1 should not be col2
    }
  }
}
