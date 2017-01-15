package org.greg.wine
package matrix


object Identity {

  def apply(n: Int): Matrix = {
    val rows = for (i <- 1 to n) yield 1 to n map (j => {
      if (i == j) 1.0
      else 0.0
    })

    new Matrix(rows.map(_.toList).toList)
  }

}

