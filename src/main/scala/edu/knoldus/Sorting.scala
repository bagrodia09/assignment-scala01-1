package edu.knoldus

import scala.annotation.tailrec

class Sorting {

  def insertionSort(array: Array[Int]): Array[Int] = {
    val myDataList: List[Int] = array.toList
    def InsideInsertionSort(list: List[Int]): List[Int] =
      if (list.isEmpty) Nil
      else insert(list.head, InsideInsertionSort(list.tail))

    def insert(x: Int, list: List[Int]): List[Int] =
      if (list.isEmpty || x <= list.head) x :: list
      else list.head :: insert(x, list.tail)

    InsideInsertionSort(myDataList).toArray
  }

  def selectionSort(array: Array[Int]): Array[Int] = {
    @tailrec
    def InsideSelectionSort(array: Array[Int], list: List[Int] = List()): Array[Int] = {
      val min = array.min
      val anOtherList = list ++ array.toList.filter(_ == min)
      if (array.min == array.max) anOtherList.toArray
      else InsideSelectionSort(array.filter(_ > min), anOtherList)
    }
    InsideSelectionSort(array)
  }

  def bubbleSort(array: Array[Int]): Array[Int] = {
    @tailrec
    def InsideBubbleSort(array: Array[Int], len: Int): Int = {
      if (len == 1) return 0
      for (i <- 0 until len - 1) {
        if (array(i) > array(i + 1)) {
          val temp = array(i)
          array(i) = array(i + 1)
          array(i + 1) = temp
        }
      }
      InsideBubbleSort(array, len - 1)
    }
    InsideBubbleSort(array, array.length)
    array
  }

}