package com.scalaplayground.dungeonexplore.PathFinding

import com.scalaplayground.dungeonexplore.Position.Position
import com.scalaplayground.dungeonexplore.constants.Constants.{NUM_COLS, NUM_ROWS}
import com.scalaplayground.dungeonexplore._
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class PathFindingTest extends FlatSpec with Matchers {
  behavior of "PathFinding"
  behavior of "Dijkstra"

  it should "return a simple shortest path" in {
    val testObject = new Dijkstra

    var tile_0_0 = new EmptyTile(new Position(0, 0))
    var tile_0_1 = new EmptyTile(new Position(0, 1))
    var tile_0_2 = new EmptyTile(new Position(0, 2))

    tile_0_0.neighbors = Seq(new Vertex(tile_0_1, 1))
    tile_0_1.neighbors = Seq(new Vertex(tile_0_0, 1), new Vertex(tile_0_2, 1))
    tile_0_2.neighbors = Seq(new Vertex(tile_0_1, 1))


    val tiles: Seq[Tile] = Seq[Tile](
      tile_0_0,
      tile_0_1,
      tile_0_2
    )

    val expectedPath = Seq[Tile](
      tile_0_0,
      tile_0_1,
      tile_0_2
    )

    val source = tiles.head
    val target = tiles(2)
    val path = testObject.findShortestPath(tiles, source, target)

    path should contain theSameElementsAs expectedPath
  }

  it should "return a longer shortest path with diagonals" in {
    val testObject = new Dijkstra

    var tile_0_0 = new EmptyTile(new Position(0, 0))
    var tile_0_1 = new EmptyTile(new Position(0, 1))
    var tile_0_2 = new EmptyTile(new Position(0, 2))
    var tile_1_0 = new VerticalWall(new Position(1, 0))
    var tile_1_1 = new EmptyTile(new Position(1, 1))
    var tile_1_2 = new EmptyTile(new Position(1, 2))
    var tile_2_0 = new EmptyTile(new Position(2, 0))
    var tile_2_1 = new EmptyTile(new Position(2, 1))
    var tile_2_2 = new EmptyTile(new Position(2, 2))
    var tile_3_0 = new EmptyTile(new Position(3, 0))
    var tile_3_1 = new EmptyTile(new Position(3, 1))
    var tile_3_2 = new EmptyTile(new Position(3, 2))

    val tiles = Seq[Tile](
      tile_0_0,
      tile_0_1,
      tile_0_2,
      tile_1_0,
      tile_1_1,
      tile_1_2,
      tile_2_0,
      tile_2_1,
      tile_2_2,
      tile_3_0,
      tile_3_1,
      tile_3_2
    )

    tiles.map(tile => {
      if (tile.passable) {
        getTileAtPosition(tiles, tile.position.x - 1, tile.position.y - 1) match {
          case Some(t) => {
            if (t.passable) {
              tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
            }
          }
          case None => Unit
        }

        getTileAtPosition(tiles, tile.position.x, tile.position.y - 1) match {
          case Some(t) => {
            if (t.passable) {
              tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
            }
          }
          case None => Unit
        }

        getTileAtPosition(tiles, tile.position.x + 1, tile.position.y - 1) match {
          case Some(t) => {
            if (t.passable) {
              tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
            }
          }
          case None => Unit
        }

        getTileAtPosition(tiles, tile.position.x - 1, tile.position.y) match {
          case Some(t) => {
            if (t.passable) {
              tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
            }
          }
          case None => Unit
        }

        getTileAtPosition(tiles, tile.position.x + 1, tile.position.y) match {
          case Some(t) => {
            if (t.passable) {
              tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
            }
          }
          case None => Unit
        }

        getTileAtPosition(tiles, tile.position.x - 1, tile.position.y + 1) match {
          case Some(t) => {
            if (t.passable) {
              tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
            }
          }
          case None => Unit
        }

        getTileAtPosition(tiles, tile.position.x, tile.position.y + 1) match {
          case Some(t) => {
            if (t.passable) {
              tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
            }
          }
          case None => Unit
        }

        getTileAtPosition(tiles, tile.position.x + 1, tile.position.y + 1) match {
          case Some(t) => {
            if (t.passable) {
              tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
            }
          }
          case None => Unit
        }
      }

    })

    val testBoard: Seq[Tile] = Seq[Tile](
      tile_0_0,
      tile_0_1,
      tile_0_2,
      tile_1_0,
      tile_1_1,
      tile_1_2,
      tile_2_0,
      tile_2_1,
      tile_2_2,
      tile_3_0,
      tile_3_1,
      tile_3_2
    )

    val expectedPath = Seq[Tile](
      tile_0_0,
      tile_1_1,
      tile_2_2,
      tile_3_2
    )

    val source = tile_0_0
    val target = tile_3_2
    val path = testObject.findShortestPath(testBoard, source, target)

    path should contain theSameElementsAs expectedPath

  }

  it should "work for large board" in {

    var tiles = Seq[Tile]()

    def randomMap = {
      for (y <- 0 to NUM_ROWS - 1) {
        for (x <- 0 to NUM_COLS - 1) {
          val thisPos = new Position(x, y)
          if (Random.nextInt(100) <= 15) {
            tiles = tiles :+ List(new HorizontalWall(thisPos), new VerticalWall(thisPos))(Random.nextInt(2))
          }
          else {
            tiles = tiles :+ new FloorTile(thisPos)
          }
        }
      }


      // TODO: Don't assume weightedDist of 1 or do shitty repeat stuff
      tiles.map(tile => {
        getTileAtPosition(tiles, tile.position.x - 1, tile.position.y - 1) match {
          case Some(t) => tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
          case None => Unit
        }

        getTileAtPosition(tiles, tile.position.x, tile.position.y - 1) match {
          case Some(t) => tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
          case None => Unit
        }

        getTileAtPosition(tiles, tile.position.x + 1, tile.position.y - 1) match {
          case Some(t) => tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
          case None => Unit
        }

        getTileAtPosition(tiles, tile.position.x - 1, tile.position.y) match {
          case Some(t) => tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
          case None => Unit
        }

        getTileAtPosition(tiles, tile.position.x + 1, tile.position.y) match {
          case Some(t) => tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
          case None => Unit
        }

        getTileAtPosition(tiles, tile.position.x - 1, tile.position.y + 1) match {
          case Some(t) => tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
          case None => Unit
        }

        getTileAtPosition(tiles, tile.position.x, tile.position.y + 1) match {
          case Some(t) => tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
          case None => Unit
        }

        getTileAtPosition(tiles, tile.position.x + 1, tile.position.y + 1) match {
          case Some(t) => tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
          case None => Unit
        }
      })
    }

    // setup map
    randomMap

    val testObject = new Dijkstra

    val actualResult = testObject.findShortestPath(tiles, getTileAtPosition(tiles, 2, 7).get, getTileAtPosition(tiles, 4, 4).get)
    actualResult.size shouldEqual 4
  }

  def getTileAtPosition(tiles: Seq[Tile],x: Int, y: Int): Option[Tile] = {
    tiles.filter(tile => tile.position.x == x && tile.position.y == y).headOption
  }
}
