package org.bitcoins.core.compat

import org.bitcoins.testkit.util.BitcoinSUnitTest

import scala.annotation.nowarn
import scala.util.{Failure, Success}

class CompatEitherTest extends BitcoinSUnitTest {

  it should "create left and right" in {

    val right = Right("Right")
    val compatRight = CompatEither(right)
    assert(compatRight.isInstanceOf[CompatRight[Nothing, String]])
    assert(compatRight.toTry == Success("Right"))

    val exception = new RuntimeException("Left")
    val left = Left(exception)
    val compatLeft = CompatEither(left)
    assert(compatLeft.isInstanceOf[CompatLeft[RuntimeException, Nothing]])
    assert(compatLeft.toTry == Failure(exception))
  }

  it should "do traverse operations" in {
    val mappedRight = CompatEither(Right(12)).map(_ => "flower")
    assert(mappedRight == CompatEither(Right("flower")))
    @nowarn val mappedLeft = CompatEither(Left(12)).map(_ => "flower")
    assert(mappedLeft == CompatEither(Left(12)))

    val flatmappedRight: CompatEither[Int, String] =
      CompatEither(Right(12)).flatMap(_ => CompatEither(Right("flower")))
    assert(flatmappedRight == CompatRight("flower"))
    @nowarn val flatmappedLeft =
      CompatEither(Left(12)).flatMap(_ => CompatEither(Left("21")))
    assert(flatmappedLeft == CompatLeft(12))

    @nowarn val foldedRight = CompatEither(Right(12)).fold({ _ =>
                     479                                        "left"
                                                           },
                                                           { _ =>
                                                             "right"
                                                           })
    assert(foldedRight == "right")
    @nowarn val foldedLeft = CompatEither(Left(12)).fold({ _ =>
                                                           "left"
                                                         },
                                                         { _ =>
                                                           "right"
                                                         })
    assert(foldedLeft == "left")
  }

}
