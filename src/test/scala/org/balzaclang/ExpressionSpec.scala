package org.balzaclang

import org.balzaclang.Expression._
import org.scalatest.TryValues._
import org.scalatest.{FlatSpec, Matchers}

class ExpressionSpec extends FlatSpec with Matchers {

  "And expression " should "be short-circuit evaluated" in {
    eval(Val(false) ~&& (BoolVar("x"))).success.value shouldBe Val(false)
    eval(Val(true) ~&& (BoolVar("x"))).failed.success.value shouldBe a [ExpressionEvaluationException]
  }

  "Or expression " should "be short-circuit evaluated" in {
    eval(Val(true) ~|| (BoolVar("x"))).success.value shouldBe Val(true)
    eval(Val(false) ~|| (BoolVar("x"))).failed.success.value shouldBe a [ExpressionEvaluationException]
  }
}
