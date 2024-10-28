def evaluate1(list: List[Int]): Double = {
  list.reverse.foldLeft(0.0) { (acc, x) => x + 1 / acc }
}

val lazylist = LazyList.fill(30)(1)

def evaluate2(n: Int, list: LazyList[Int]): Double = {
  list.take(n).reverse.foldLeft(0.0) { (acc, x) => x + 1 / acc }
}
val res2 = evaluate1(List(1, 1, 1, 1, 1,
  1, 1, 1, 1, 1,
  1, 1, 1, 1, 1,
  1, 1, 1, 1, 1,
  1, 1, 1, 1, 1,
  1, 1, 1, 1, 1))
val result2 = evaluate2(30, lazylist)
if (math.abs(result2 - res2) == 0) println("OK")