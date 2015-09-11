/**
 * Created by tam on 27/07/2015.
 */
package com.ladybug

object Jaccard {
  def calculate(userList1: Map[String, Boolean], userList2: Map[String, Boolean]): Double = {

    userList1.nonEmpty && userList2.nonEmpty match {
      case true =>
        val filteredUserList1 = userList1.filterKeys(key => userList2.keySet.contains(key))
        var M11, M10, M01 = 0

        for {
          (k1, v1) <- filteredUserList1
          v2 <- userList2.get(k1)
        } {
          if (v1 && v2) {
            M11 += 1
          } else if (v1 && !v2) {
            M10 += 1
          } else if (!v1 && v2) {
            M01 += 1
          }
        }
        (1.0 * M11) / (M10 + M01 + M11)
      case false => 0
    }
  }
}
