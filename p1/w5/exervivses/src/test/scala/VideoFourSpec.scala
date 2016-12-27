import org.scalatest._
import scala.com.example._

class VideoFourSpec extends FlatSpec with Matchers {
  "pack" should "correctly pack a list of chars" in {
    val list = List('a', 'a', 'a', 'b', 'c', 'c')
    val actual = VideoFour.pack(list)

    actual shouldEqual List(List('a', 'a', 'a'), List('b'), List('c', 'c'))
  }

  "encode" should "correctly create encode strings" in {
    val list = List()
  }
}