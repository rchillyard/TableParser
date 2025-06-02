def evaluate1(seq: Seq[Int]): Double = {
  seq.reverse.foldLeft(0.0) { (acc, x) => x + 1 / acc }
}

val res = evaluate1(List(4, 2, 1, 3, 1, 2, 8))
val result = (res * 1000).round / 1000.toDouble

val X = 4.359
if (math.abs(result - X) == 0) println("OK")