
// Q10
val X = LazyList(1, 1, 1, 1, 1,
  1, 1, 1, 1, 1,
  1, 1, 1, 1, 1,
  1, 1, 1, 1, 1,
  1, 1, 1, 1, 1,
  1, 1, 1, 1, 1)

def evaluate1(epsilon: Double, lazylist: LazyList[Int]): Double = {
  lazylist.reverse.takeWhile(_ > epsilon).foldLeft(0.0) { (acc, x) => x + 1 / acc }
}

val res10 = evaluate1(0.0000001, X)

// Q11
val Y = LazyList(4, 2, 1, 3, 1, 2, 8)

def evaluate2(epsilon: Double, lazylist: LazyList[Int]): Double = {
  LazyList.continually(lazylist).flatten.reverse.takeWhile(_ > epsilon).foldLeft(0.0) { (acc, x) => x + 1 / acc }
}

val res11 = evaluate2(0.0000001, Y)