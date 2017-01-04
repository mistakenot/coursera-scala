package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

	test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }


	test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }


	test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

  test("neighboursWithHistory") {
    new Level1 {
      val start = Block(Pos(0, 0), Pos(0, 0))
      val stream = neighborsWithHistory(start, List.empty)

      val fst = stream.head
      assert(fst._1 == Block(Pos(0, 1), Pos(0, 2)))
      assert(fst._2 == List(Right))

      val scnd = stream.tail.head
      assert(scnd._1 == Block(Pos(1, 0), Pos(2, 0)))
      assert(scnd._2 == List(Down))
      
      val streamTwo = neighborsWithHistory(scnd._1, scnd._2)
      val thrd = streamTwo.head 
      assert(thrd._1 == Block(Pos(1, 1), Pos(2, 1)))
      assert(thrd._2 == List(Down, Right))
    }
  }

  test("pathsFromStart") {
    /* y1234567
    x |ooo-------
    1 |oSoooo----
    2 |ooooooooo-
    3 |-ooooooooo
    4 |-----ooToo
    5 |------ooo-*/

    new Level1 {
      val stream = pathsFromStart

      val fst = stream.head
      assert(fst._1 == Block(Pos(1, 2), Pos(1, 3)))
      assert(fst._2 == List(Right))

      val scnd = stream.tail.head
      assert(scnd._1 == Block(Pos(2, 1), Pos(3, 1)))
      assert(scnd._2 == List(Down))

      val thrd = stream.tail.tail.head
      assert(thrd == (Block(Pos(1, 4), Pos(1, 4)), List(Right, Right)))
    }
  }

}
