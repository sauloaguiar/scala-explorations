/* Find the last element of a list.
Example:
scala> last(List(1, 1, 2, 3, 5, 8))
res0: Int = 8 */

def lastBuiltIn[A](list: List[A]): A = {
  list.last
}

def lastRecursive[A](list: List[A]): A = list match {
  case tail :: Nil  => tail
  case _ :: tail    => lastRecursive(tail)
  case _            => throw new NoSuchElementException
}

require(lastBuiltIn(List(1,2,3)) == lastRecursive(List(1,2,3)))

/*
Find the last but one element of a list.
Example:
scala> penultimate(List(1, 1, 2, 3, 5, 8))
res0: Int = 5
 */

def penultimateRecursive[A](list: List[A]): A = list match {
  case pen :: tail :: Nil => pen
  case _ :: tail          => penultimateRecursive(tail)
  case _                  => throw new NoSuchElementException
}

penultimateRecursive(List(1,2,3))

/*
Find the Kth element of a list.
By convention, the first element in the list is element 0.
Example:

scala> nth(2, List(1, 1, 2, 3, 5, 8))
res0: Int = 2
 */

def findKth[A](k: Int, list: List[A]): A = k match {
  case 0 => list.head
  case _ => findKth(k-1, list.tail)
}

findKth(2, List(1,1,2,3,5,8))

/*
Find the number of elements of a list.
Example:
scala> length(List(1, 1, 2, 3, 5, 8))
res0: Int = 6
 */

def length[A](list: List[A]): Int = list match {
  case Nil => 0
  case head :: tail => 1 + length(tail)
}

def lengthTail[A](list: List[A]): Int = {
  def lengthWithAcc[A](acc: Int, list: List[A]): Int = list match {
    case Nil => acc
    case head :: tail => lengthWithAcc(acc+1, tail)
  }
  lengthWithAcc(0, list)
}

require(length(List(1,2,3)) == lengthTail(List(1,2,3)))

/*
Reverse a list.
Example:
scala> reverse(List(1, 1, 2, 3, 5, 8))
res0: List[Int] = List(8, 5, 3, 2, 1, 1)
 */

def reverse[A](list: List[A]): List[A] = list match {
  case head :: tail =>  reverse(tail) ::: List(head)
  case Nil  =>  Nil
}

def reverseTail[A](list: List[A]): List[A] = {
  def reverseWithAcc[A](acc: List[A], list: List[A]): List[A] = list match {
    case head :: tail => reverseWithAcc(head +: acc, tail)
    case Nil          => acc
  }
  reverseWithAcc(List(), list)

}
reverse(List(1, 1, 2, 3, 5, 8))
reverseTail(List(1, 1, 2, 3, 5, 8))