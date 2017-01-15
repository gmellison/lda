package org.greg.wine
package random

import scala.math.sqrt
import scala.math.log
import scala.math.exp
import scala.math.Pi
import scala.math.pow
//import org.greg.wine.matrix.Matrix

class Normal(mean: Double = 0, variance: Double = 1) {

  assert(variance >= 0)

  def pdf(x: Double): Double = {
    (1 / sqrt(2 * Pi * this.variance)) * exp( (1 / 2 * variance) * pow((x - this.mean), 2) )
  }
   
  override def toString() = {
    s"~ Normal(mean = $mean, variance = $variance)"
  }
}

/*class MultiNormal(mean: Matrix, covariance: Matrix) {*/
  //assert(mean.nrow == 1)
  //assert(mean.ncol > 0)
  //assert(covariance.ncol == covariance.ncol)
  
  //val k = covariance.ncol

  //def pdf(x: Matrix): Double = {

    //1.0    
///*    assert(x.nrow == 1)<]*/
    ////assert(x.ncol == mean.ncol)
 
    ////val inv_cov = covariance.inverse().determinant()
    ////val coef = inv_cov.scalar_mult(1 / pow(2 * Pi)) 
    ////val exp_arg: Double = (-1.0 / 2.0)


  //}
/*}*/
