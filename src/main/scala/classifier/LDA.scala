package org.greg.lda
package classifiers

import org.greg.lda.matrix._

class LDA(data: Map[Int, DesignMatrix]) {
  
  assert(data.size > 0)

  val k = data.values.head

  val train_data = data
  val train_means = this.train_data.mapValues(x => x.col_means())
  val covariances = this.train_data.mapValues(x => x.covariance())

  def predict(new_x: List[Double]) = {

    assert(new_x.size == this.k)

     
    
  
  }
}
