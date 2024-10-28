
// wrong b/c not looking at contribution of acc
def evaluate3(epsilon: Double, list: LazyList[Int]): Double = {
  case total =>
    val ux = list.reverse.foldLeft(0.0) { (acc, x) => x + 1 / acc }
    if (ux < epsilon) total
    else evaluate3(epsilon, list :+ 1)
}
