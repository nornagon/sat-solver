package net.nornagon.dpll

object DPLL {
  implicit def var2lit(v: Variable): Literal = Literal(v, false)

  case class Variable(name: String) {
    def unary_~(): Literal = Literal(this, true)
  }
  case class Literal(variable: Variable, negated: Boolean) {
    def trueAt(x: Boolean) = negated ^ x
    def unary_~(): Literal = Literal(variable, !negated)

    def |(l: Literal): Clause = Clause(Set(this, l))
    def ^(l: Literal): Theory = (this | l) & (~this | ~l)

    override def toString = if (negated) s"~${variable.name}" else variable.name
  }
  case class Clause(literals: Set[Literal]) {
    def |(l: Literal): Clause = Clause(literals + l)
    def &(c: Clause): Theory = Theory(Set(this, c))
    def variables = literals.map(_.variable)
    def satisfiedGiven(v: Variable, x: Boolean) =
      literals.exists(l => l.variable == v && l.trueAt(x))
    def contains(v: Variable) = literals.exists(_.variable == v)
    def fix(v: Variable, x: Boolean): Option[Clause] = {
      if (!contains(v)) Some(this)
      else if (satisfiedGiven(v, x)) None
      else {
        Some(Clause(literals.filter(_.variable != v)))
      }
    }
    override def toString = literals.map(_.toString).mkString(" | ")
  }
  case class Theory(clauses: Set[Clause]) {
    def &(c: Clause): Theory = Theory(clauses + c)
    def &&(t: Theory): Theory = Theory(clauses ++ t.clauses)
    def variables = clauses.flatMap(_.variables)
    def fix(v: Variable, x: Boolean): Theory = {
      Theory(clauses.flatMap(_.fix(v, x)))
    }
    override def toString = "{\n" + clauses.map("  " + _.toString).mkString("\n") + "\n}"

    def findUnitClause: Option[Literal] = {
      clauses.find(_.literals.size == 1).map(_.literals.head)
    }
  }

  // DSL for expressing combinatorial logic and reducing it to CNF by the Tseitin transformation
  sealed trait Expr {
    val tvar = Variable(toString)
    def cnf: Theory
    def isTrue: Theory = {
      cnf & Clause(Set(tvar))
    }

    def &(b: Expr): Expr = And(this, b)
    def |(b: Expr): Expr = Or(this, b)
    def ^(b: Expr): Expr = Xor(this, b)
    def unary_~(): Expr = Not(this)
  }
  case class Var(name: String) extends Expr {
    override def toString = name
    def cnf: Theory = Theory(Set.empty)
  }
  case class And(a: Expr, b: Expr) extends Expr {
    override def toString = s"($a & $b)"
    def cnf: Theory =
      (~a.tvar | ~b.tvar | tvar) & (a.tvar | ~tvar) & (b.tvar | ~tvar) && a.cnf && b.cnf
  }
  case class Or(a: Expr, b: Expr) extends Expr {
    override def toString = s"($a | $b)"
    def cnf: Theory =
      (a.tvar | b.tvar | ~tvar) & (~a.tvar | tvar) & (~b.tvar | tvar) && a.cnf && b.cnf
  }
  case class Not(a: Expr) extends Expr {
    override def toString = s"~$a"
    def cnf: Theory = a match {
      case Not(e: Expr) => e.cnf
      case _ => (~a.tvar | ~tvar) & (a.tvar | tvar) && a.cnf
    }
  }
  case class Xor(a: Expr, b: Expr) extends Expr {
    override def toString = s"($a ^ $b)"
    def cnf: Theory =
      (~a.tvar | ~b.tvar | ~tvar) &
      (a.tvar | b.tvar | ~tvar) &
      (a.tvar | ~b.tvar | tvar) &
      (~a.tvar | b.tvar | tvar) && a.cnf && b.cnf
  }

  type Assignment = Map[Variable, Boolean]

  def solve(t: Theory): Option[Assignment] = {
    // A theory with no clauses is a tautology.
    if (t.clauses.isEmpty) {
      return Some(Map.empty)
    }

    // A theory with an empty clause cannot be satisfied.
    if (t.clauses.exists(_.literals.isEmpty)) {
      return None
    }

    t.findUnitClause map { l =>
      return solve(t.fix(l.variable, !l.negated)) map { _ + (l.variable -> !l.negated) }
    }

    // Choose a variable.
    val allVars = t.variables
    val variable = allVars.head

    // It must be either true or false.
    solve(t.fix(variable, true)) map { _ + (variable -> true) } orElse {
      solve(t.fix(variable, false)) map { _ + (variable -> false) }
    }
  }

  def solve(e: Expr): Option[Assignment] = solve(e.isTrue)
}
