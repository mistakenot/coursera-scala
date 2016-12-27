import org.scalatest._
import scala.com.example._

class VideoOneSpec extends FlatSpec with Matchers {
  "init" should "return all except last of list" in {
    val list = List(1, 2, 3)
    val actual = VideoOne.init(list)

    actual shouldEqual List(1, 2)
  }

  "remove" should "remove correct item from list" in {
    val list = List(1, 2, 3)
    val actual = VideoOne.remove(list, 1)

    actual shouldEqual List(1, 3)
  }

  "flattern" should "flattern a 2 dimensional list" in {
    val list = List(List(1, 2), List(3, 4), List(5, 6))
    val actual = VideoOne.flattern(list)

    actual shouldEqual List(1, 2, 3, 4, 5, 6)
  }
}
