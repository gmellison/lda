package org.greg.lda
package matrix

import scala.math.abs

class Matrix(elements: List[List[Double]]) {
  /* provides a matrix representation, along with some useful methods.
   * matrix needs to be input as a nested list of doubles.
   */
  
  assert(elements.size > 0)  
  assert(elements.head.size > 0)
  assert(elements.forall(x => x.size == elements.head.size))

  val matrix: List[List[Double]] = elements
  val nrow = this.matrix.size
  val ncol = this.matrix.head.size

  // equality method for comparing matrix instances:
  override def equals(that: Any): Boolean = {
    that match {
      case that: Matrix => {
        if (this.nrow == that.nrow && this.ncol == that.ncol) {
          val elem_equality = for (
            i <- 0 to this.nrow-1;
            j <- 0 to this.ncol-1) yield this.matrix(i)(j) == that.matrix(i)(j)         
          elem_equality.forall(x => x)
        } else false
      }
      case _ => false
    }
  }

  
  def transpose(): Matrix = {
    val t = for (i <- 0 to this.matrix.head.size - 1) yield {
      for (j <- 0 to this.matrix.size - 1) yield {
        this.matrix(j)(i) 
      } 
    }
    val lists = t.map(_.toList).toList
    new Matrix(lists)

  }

  private def addRows(r1: List[Double], r2: List[Double]): List[Double] = {
    // helper method for matrix addition
    // given two lists l1 and l2, sums them elementwise
    (r1 zip r2).map({ case (e1, e2) => e1 + e2 }) 
  }

  def add(that: Matrix): Matrix = {
    // matrix addition. input is another matrix
    assert(this.nrow == that.nrow)
    assert(this.ncol == that.ncol)

    val m1 = this.matrix
    val m2 = that.matrix

    new Matrix((m1 zip m2).map({ case (r1, r2) => this.addRows(r1, r2) } ))     
  }
    
  private def subRows(r1: List[Double], r2: List[Double]): List[Double] = {
    // helper method for matrix subtraction
    (r1 zip r2).map({ case (e1, e2) => e1 - e2 } ) 
  }

  def subtract(that: Matrix): Matrix = {
    // matrix subtraction
    assert(this.nrow == that.nrow)
    assert(this.ncol == that.ncol)

    val m1 = this.matrix
    val m2 = that.matrix

    new Matrix((m1 zip m2).map( { case (r1, r2) => this.subRows(r1, r2) }))     
  }

  def scalar_mult(c: Double): Matrix = {
    val rows_prod = this.matrix.map(row => row.map(x => c * x))
    new Matrix(rows_prod)
  }


  private def dot(l1: List[Double], l2: List[Double]): Double = {
    // internal dot product method, used in matrix multiplication method
    assert(l1.size == l2.size)
    assert(l1.size > 0)
    (l1 zip l2).map( { case (e1, e2) => e1 * e2 }).foldLeft(0.0)((s, e) => s + e)
  }

  def matrix_mult(that: Matrix): Matrix = {
    // matrix multiplication. takes another matrix as its only arg.
    assert(this.ncol == that.nrow)

    val new_mat_elems = for (row_this <- this.matrix) yield that.transpose().matrix.map(col_that => { 
         this.dot(row_this, col_that)
    })

    new Matrix(new_mat_elems)
  }

  override def toString() = {
    val row_strings = for (row <- this.matrix) yield row.mkString("\n[", ", ", "]")
    row_strings.mkString
  }

  def row_norm() = {
    val row_sums = this.matrix.map(row => row.foldLeft(0.0)((s, e) => s + abs(e)))
    row_sums.foldLeft(row_sums.head)((m, e) => if (e > m) e else m)
  }

  def col_norm() = {
    val col_sums = this.transpose().matrix.map(row => row.foldLeft(0.0)((s, e) => s + abs(e)))
    col_sums.foldLeft(col_sums.head)((m, e) => if (e > m) e else m)
  }


  private def close_enough(m2: Matrix, tolerance: Double = 0.001): Boolean = {
    val elem_close = (this.matrix zip m2.matrix).map({ case (r1, r2) => {
      (r1 zip r2).map( { case (e1, e2) => {
          abs(e1 - e2) < tolerance
        }})
      }})
      val all = for (
        i <- elem_close;
        j <- i) yield j
      all.forall(x => x)
  }

  def inverse(): Matrix = {
    // computes the inverse using newton's method. 
    // returns an error if this isn't square
    
    assert(this.nrow == this.ncol)
    assert(abs(this.determinant()) > 1.0e-6)
    
    val x0 = this.transpose().scalar_mult( 1.0 / (this.row_norm() * this.col_norm()))
    val i = Identity(this.nrow) // make k x k identity matrix 
    
    def inverse_recur(x: Matrix, i: Matrix): Matrix = {
      val update = x.matrix_mult(i.scalar_mult(2.0).subtract(this.matrix_mult(x)))
      if (this.matrix_mult(update).close_enough(i)) {
        update
      } else {
        inverse_recur(update, i)
      }
    }
    inverse_recur(x0, i)
  }

  // The following functions are used to calculate the matrix determinant recursively
  // they are "borrowed" from https://github.com/joom/matrix-challenge/blob/master/code/matrix.scala
  
  // getMinor(m)(i, j) = the minor of the matrix m for (i,j)
  private def getMinor(i: Int, j: Int) : Matrix = {
    // without(xs, n) = xs without nth element (n starting from 1)
    def without[A](xs: List[A], n: Int) : List[A] = {
      xs.zipWithIndex.filterNot { case (_, i) => i + 1 == n}.unzip._1
    }
    val minor_vals = without(this.matrix, i).map(r => without(r, j))
    new Matrix(minor_vals)
  }
  

  // getFirstRowPairs(m) = the list of (row, col) for the first row of the matrix
  private def getFirstRowPairs(m: List[List[Double]]) : List[(Int, Int)] = {
    (1 to (m.length)).toList.map(y => (1, y))
  }
 
  // getCell(m)(i, j) = the cell in the matrix m in (i,j)
  private def getCell(m: List[List[Double]])(i: Int, j: Int) : Double = {
    m(i - 1)(j - 1)
  }
  
  // evensNegative(xs) = the same list with the elements with even indices negated
  //                    indices start from 1
  private def evensNegative(xs: List[Double]) : List[Double] = {
    xs.zipWithIndex
      .map { case (x, n) => (x, n % 2 != 0) }
      .map { case (x, isEven) => if (isEven) -x else x }
  }
  
  // getDeterminant(m) = calculates determinant for matrix m
  def determinant() : Double = {

    assert(this.ncol == this.nrow)

    if (this.matrix.size == 1) getCell(this.matrix)(1, 1)
    else {
      val list = getFirstRowPairs(this.matrix).map(p =>
        (getCell(this.matrix)(p._1, p._2)) * (this.getMinor(p._1, p._2).determinant()))
      evensNegative(list).sum
    }
  }
}

