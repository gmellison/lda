package org.greg.lda
package matrix

abstract trait Design {
  def col_means: List[Double]
  def covariance: Matrix
}

class DesignMatrix(elements: List[List[Double]]) extends Matrix (elements) with Design {
 

  private def mean(x: List[Double]) = x.foldLeft(0.0)((s, e) => s + e) / x.size.toDouble

  private def covariance(x1: List[Double], x2: List[Double]): Double = {
    assert(x1.size == x2.size)
    assert(x1.size > 1)

    val x1_bar = this.mean(x1) 
    val x2_bar = this.mean(x2)

    val pair_deviations = (x1 zip x2).map({ case (e1, e2) => (e1 - x1_bar) * (e2 - x2_bar) } )
    pair_deviations.foldLeft(0.0)((s, e) => s + e) / (x1.size - 1)
  }

  private def variance(x: List[Double]): Double = this.covariance(x, x)

  def col_means(): List[Double] = this.transpose().matrix.map(x => this.mean(x))
  def covariance(): Matrix = {
    val covariances = for (col1 <- this.transpose().matrix) yield this.transpose().matrix.map(col2 => {
         this.covariance(col1, col2)
    })
    new Matrix(covariances)
  } 
}
