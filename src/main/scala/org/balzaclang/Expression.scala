package org.balzaclang

import scala.util.{Failure, Success, Try}


trait Expression[T] {
  def ~== (other: Expression[T]): Equals[T] = Equals(this, other)
}

trait IntExpression extends Expression[Int] {
  def ~+(n: Long): Plus = this ~+ IntVal(n)
  def ~-(n: Long): Minus = this ~- IntVal(n)
  def ~== (n: Long): Equals[Int] = this ~== IntVal(n)
  def ~> (n: Long): Greater = this ~> IntVal(n)
  def ~>= (n: Long): GreaterEqual = this ~>= IntVal(n)
  def ~< (n: Long): Lower = this ~< IntVal(n)
  def ~<= (n: Long): LowerEqual = this ~<= IntVal(n)

  def ~+(other: IntExpression): Plus = Plus(this, other)
  def ~-(other: IntExpression): Minus = Minus(this, other)
  def ~==(other: IntExpression): Equals[Int] = Equals(this, other)
  def ~>(other: IntExpression): Greater = Greater(this, other)
  def ~>=(other: IntExpression): GreaterEqual = GreaterEqual(this, other)
  def ~<(other: IntExpression): Lower = Lower(this, other)
  def ~<=(other: IntExpression): LowerEqual = LowerEqual(this, other)
}

trait StringExpression extends Expression[String] {
  def ~+(s: String): Concat = this ~+ StringVal(s)
  def ~== (s: String): Equals[String] = this ~== StringVal(s)

  def ~+(other: StringExpression): Concat = Concat(this, other)
  def ~==(other: StringExpression): Equals[String] = Equals(this, other)
}

trait BoolExpression extends Expression[Boolean] {
  def ~&& (b: Boolean): And = this ~&& BoolVal(b)
  def ~|| (b: Boolean): Or = this ~|| BoolVal(b)
  def ~== (b: Boolean): Equals[Boolean] = this ~== BoolVal(b)

  def ~&& (other: BoolExpression): And = And(this, other)
  def ~|| (other: BoolExpression): Or = Or(this, other)
  def ~==(other: BoolExpression): Equals[Boolean] = Equals(this, other)
  def unary_~! : Neg = Neg(this)
}

/*
 * values
 */
sealed case class IntVal(value: Long) extends IntExpression
sealed case class BoolVal(value: Boolean) extends BoolExpression
sealed case class StringVal(value: String) extends StringExpression

/*
 * variables
 */
abstract class Var[T](val name: String) extends Expression[T]
sealed case class IntVar(override val name: String) extends Var[Int](name) with IntExpression
sealed case class BoolVar(override val name: String) extends Var[Boolean](name) with BoolExpression
sealed case class StringVar(override val name: String) extends Var[String](name) with StringExpression

/*
 * operations
 */
sealed case class Neg(a: BoolExpression) extends BoolExpression
sealed case class And(a: BoolExpression, b: BoolExpression) extends BoolExpression
sealed case class Or(a: BoolExpression, b: BoolExpression) extends BoolExpression
sealed case class Equals[T](a: Expression[T], b: Expression[T]) extends BoolExpression
sealed case class Plus(a: IntExpression, b: IntExpression) extends IntExpression
sealed case class Concat(a: StringExpression, b: StringExpression) extends StringExpression
sealed case class Minus(a: IntExpression, b: IntExpression) extends IntExpression
sealed case class Mul(a: IntExpression, b: IntExpression) extends IntExpression
sealed case class Div(a: IntExpression, b: IntExpression) extends IntExpression
sealed case class Greater(a: IntExpression, b: IntExpression) extends BoolExpression
sealed case class Lower(a: IntExpression, b: IntExpression) extends BoolExpression
sealed case class GreaterEqual(a: IntExpression, b: IntExpression) extends BoolExpression
sealed case class LowerEqual(a: IntExpression, b: IntExpression) extends BoolExpression

object Var {
  // Var decomposition for pattern matching
  def unapply(v: Var[_]): Option[String] = Some(v.name)
}

case object Expression {

  class ExpressionEvaluationException(e:Expression[_], msg: Option[String]) extends RuntimeException {
    override def toString: String = "Error evaluating expression "+e+msg.fold("")(_ => ": "+msg)
  }

  implicit def emptyRho[T]: Map[String,Expression[T]] = Map()

  def msgWrongType = "wrong type"
  def msgVariableNotFound(name:String) = s"variable $name not found"
  def msgCycleDetected(name:String) = s"cycle detected resolving variable $name"

  def fail[E,T](e: Expression[E]): Failure[T] = Failure(new ExpressionEvaluationException(e, None))
  def fail[E,T](e: Expression[E], msg:String): Failure[T] = Failure(new ExpressionEvaluationException(e, Some(msg)))

  def eval[T](e: Expression[_])(implicit rho: Map[String,Expression[T]]) : Try[Expression[_]] = e match {

    case IntVal(_) => Success(e)
    case BoolVal(_) => Success(e)
    case StringVal(_) => Success(e)

    case Var(name) => rho.get(name) match {
      case Some(Var(name2)) if name == name2 => fail(e, msgCycleDetected(name))
      case Some(e1) => eval(e1)(rho)
      case None => fail(e, msgVariableNotFound(name))
    }

    case Neg(a) => eval(a)(rho) match {
      case Success(BoolVal(x)) => Success(BoolVal(!x))
      case Success(e1) => fail(e1, msgWrongType)
      case Failure(ex) => Failure(ex)
    }

    case And(a,b) => eval(a)(rho) match {
      case Success(BoolVal(false)) => Success(BoolVal(false))
      case Success(BoolVal(true)) => eval(b)(rho) match {
        case Success(BoolVal(y)) => Success(BoolVal(y))
        case Success(e1) => fail(e1, msgWrongType)
        case Failure(ex) => Failure(ex)
      }
      case Success(e1) => fail(e1, msgWrongType)
      case Failure(ex) => Failure(ex)
    }

    case Or(a,b) => eval(a)(rho) match {
      case Success(BoolVal(true)) => Success(BoolVal(true))
      case Success(BoolVal(false)) => eval(b)(rho) match {
        case Success(BoolVal(y)) => Success(BoolVal(y))
        case Success(e1) => fail(e1, msgWrongType)
        case Failure(ex) => Failure(ex)
      }
      case Success(e1) => fail(e1, msgWrongType)
      case Failure(ex) => Failure(ex)
    }

    case Equals(Var(a),Var(b)) if a == b => Success(BoolVal(true))

    case Equals(a,b) => eval(a)(rho) match {
      case Success(BoolVal(x)) => eval(b)(rho) match {
        case Success(BoolVal(y)) => Success(BoolVal(x.equals(y)))
        case Success(e1) => fail(e1, msgWrongType)
        case Failure(ex) => Failure(ex)
      }
      case Success(e1) => fail(e1, msgWrongType)
      case Failure(ex) => Failure(ex)
    }

    case Plus(a,b) => eval(a)(rho) match {
      case Success(IntVal(x)) => eval(b)(rho) match {
        case Success(IntVal(y)) => Success(IntVal(x + y))
        case Success(e1) => fail(e1, msgWrongType)
        case Failure(ex) => Failure(ex)
      }
      case Success(StringVal(x)) => eval(b)(rho) match {
        case Success(StringVal(y)) => Success(StringVal(x + y))
        case Success(e1) => fail(e1, msgWrongType)
        case Failure(ex) => Failure(ex)
      }
      case Success(e1) => fail(e1, msgWrongType)
      case Failure(ex) => Failure(ex)
    }

    case Minus(a,b) => eval(a)(rho) match {
      case Success(IntVal(x)) => eval(b)(rho) match {
        case Success(IntVal(y)) => Success(IntVal(x - y))
        case Success(e1) => fail(e1, msgWrongType)
        case Failure(ex) => Failure(ex)
      }
      case Success(e1) => fail(e1, msgWrongType)
      case Failure(ex) => Failure(ex)
    }

    case Mul(a,b) => eval(a)(rho) match {
      case Success(IntVal(x)) => eval(b)(rho) match {
        case Success(IntVal(y)) => Success(IntVal(x * y))
        case Success(e1) => fail(e1, msgWrongType)
        case Failure(ex) => Failure(ex)
      }
      case Success(e1) => fail(e1, msgWrongType)
      case Failure(ex) => Failure(ex)
    }

    case Div(a,b) => eval(a)(rho) match {
      case Success(IntVal(x)) => eval(b)(rho) match {
        case Success(IntVal(y)) if y != 0 => Success(IntVal(x / y))
        case Success(IntVal(y)) if y == 0 => fail(e, "divide by 0")
        case Success(e1) => fail(e1, msgWrongType)
        case Failure(ex) => Failure(ex)
      }
      case Success(e1) => fail(e1, msgWrongType)
      case Failure(ex) => Failure(ex)
    }

    case Greater(a,b) => eval(a)(rho) match {
      case Success(IntVal(x)) => eval(b)(rho) match {
        case Success(IntVal(y)) => Success(BoolVal(x > y))
        case Success(e1) => fail(e1, msgWrongType)
        case Failure(ex) => Failure(ex)
      }
      case Success(e1) => fail(e1, msgWrongType)
      case Failure(ex) => Failure(ex)
    }

    case GreaterEqual(a,b) => eval(a)(rho) match {
      case Success(IntVal(x)) => eval(b)(rho) match {
        case Success(IntVal(y)) => Success(BoolVal(x >= y))
        case Success(e1) => fail(e1, msgWrongType)
        case Failure(ex) => Failure(ex)
      }
      case Success(e1) => fail(e1, msgWrongType)
      case Failure(ex) => Failure(ex)
    }

    case Lower(a,b) => eval(a)(rho) match {
      case Success(IntVal(x)) => eval(b)(rho) match {
        case Success(IntVal(y)) => Success(BoolVal(x < y))
        case Success(e1) => fail(e1, msgWrongType)
        case Failure(ex) => Failure(ex)
      }
      case Success(e1) => fail(e1, msgWrongType)
      case Failure(ex) => Failure(ex)
    }

    case LowerEqual(a,b) => eval(a)(rho) match {
      case Success(IntVal(x)) => eval(b)(rho) match {
        case Success(IntVal(y)) => Success(BoolVal(x <= y))
        case Success(e1) => fail(e1, msgWrongType)
        case Failure(ex) => Failure(ex)
      }
      case Success(e1) => fail(e1, msgWrongType)
      case Failure(ex) => Failure(ex)
    }
  }
}

/*
 * Facade interface
 */
object Val {
  def apply(b: Boolean): BoolVal = BoolVal(b)
  def apply(s: String): StringVal = StringVal(s)
  def apply(n: Int): IntVal = IntVal(n)
}