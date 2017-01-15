package org.greg.wine
package classifiers

import org.greg.wine.matrix._

class LDA(data: Map[Int, DesignMatrix]) {
 
  val train_data = data
  val train_means = this.train_data.mapValues(x => x.col_means())
  val train_covariances = this.train_data.mapValues(x => x.covariance())

  def train() = {
     
  } 

  def predict() = {
  
  }
}
