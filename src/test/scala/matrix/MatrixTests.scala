package org.greg.lda

import org.scalatest.FunSuite
import org.greg.lda.matrix._

class MatrixTests extends FunSuite {
  
  assertThrows[java.lang.AssertionError] {
    new Matrix(List())
  } 
  
  assertThrows[java.lang.AssertionError] {
    new Matrix(List(List()))
  }
  
  // Matrix tests
  val mat1 = new Matrix(List(List(0.0, 1.0), List(2.0, 3.0)))
  val mat1t = new Matrix(List(List(0.0, 2.0), List(1.0, 3.0)))
  val mat1inv = mat1.inverse()

  assert(mat1.transpose() == mat1t)
  assert(mat1.determinant() == -2.0)
  assert(mat1inv.ncol == 2 && mat1inv.nrow == 2)

  val mat2 = new Matrix(List(List(0.0, 1.0)))

  assertThrows[java.lang.AssertionError] {
    mat2.inverse()
  }

  // Identity matrix tests
  val i2 = Identity(2)
  
  assert(i2 == new Matrix(List(List(1.0, 0.0), List(0.0, 1.0))))
  
}
