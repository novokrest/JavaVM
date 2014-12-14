
object Test {
  import Macros._
  import HList._

  def main(args: Array[String]): Unit = {
    val x = "x" :: true :: 1.0 :: HNil
    val y = 1 :: "y" :: false :: HNil
    val z = x ++ y
    def f(x: Any, y: String): String = x.toString + " :: " + y

    println(z.foldr(f, "nil"))

    printf("scala is %s%d%c %c%f%s!\n", "mag", 1, 'c', 'p', 0, "wer")
  }

}
