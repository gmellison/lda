package org.greg.wine
package random

import scala.math.sqrt
import scala.math.log
import scala.math.exp
import scala.math.Pi
import scala.math.pow
import org.greg.wine.matrix._

class Normal(m: Double = 0.0, v: Double = 1.0) {

  assert(v >= 0)

  val mean = m
  val variance = v

  def pdf(x: Double): Double = {
    val coef = (1.0 / sqrt(2.0 * Pi * this.variance))
    val exp_arg = (-1.0 / (2 * this.variance)) * pow((x - this.mean), 2) 
    coef * exp(exp_arg)
  }
  
   
  override def toString() = {
    s"~ Normal(mean = $mean, variance = $variance)"
  }
}

class MultiNormal(m: Matrix, cov: Matrix) {

  val covariance = cov
  val mean = m
  val k = cov.ncol

  assert(this.mean.nrow == 1)
  assert(this.mean.ncol > 0)
  assert(this.covariance.nrow == this.covariance.ncol)
  
  def pdf(x: Matrix): Double = {

    assert(x.nrow == 1)
    assert(x.ncol == this.mean.ncol)
 
    val cov_inv = this.covariance.inverse()
    val cov_det = this.covariance.determinant()
    val coef = (1 / sqrt(cov_det * pow(2 * Pi, k)))
    val res = x.subtract(this.mean)
    val exp_arg: Double = res.matrix_mult(cov_inv).
      matrix_mult(res.transpose()).
      scalar_mult(- 0.5).
      matrix(0)(0)

    coef * exp(exp_arg)
  }
}
