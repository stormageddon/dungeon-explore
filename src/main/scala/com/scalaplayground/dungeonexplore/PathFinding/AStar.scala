package com.scalaplayground.dungeonexplore.PathFinding

import com.scalaplayground.dungeonexplore.Tile

class AStar {
  def h(currTile: Tile, destination: Tile): Double = {
    return Math.abs(currTile.position.x - destination.position.x) + Math.abs(currTile.position.y - destination.position.y).toDouble
  }

  // returns the node in openSet having the lowest fScore[] value
  def getLowestFScore(openSet: List[Tile], fScore: Map[Tile, Double]): Tile = {
    var lowestTile: Tile = openSet.head
    var lowestDist = fScore(lowestTile)
    openSet.foreach(tile => {
      if (fScore(tile) < lowestDist) {
        lowestTile = tile
        lowestDist = fScore(tile)
      }
    })

    lowestTile
  }

  def reconstructPath(cameFrom: Map[Tile, Tile], current: Tile): Seq[Tile] = {
    var currentTile: Tile = current
    var totalPath: Seq[Tile] = Seq[Tile](currentTile)
    while (cameFrom.contains(currentTile)) {
      totalPath = currentTile +: totalPath
      currentTile = cameFrom(currentTile)
    }

    totalPath
  }

  def findShortestPath(graph: Seq[Tile], source: Tile, target: Tile): Seq[Tile] = {
    var openSet: List[Tile] = List[Tile](source)
    var cameFrom: Map[Tile, Tile] = Map[Tile, Tile]()

    var gScore: Map[Tile, Double] = Map[Tile, Double]()
    var fScore: Map[Tile, Double] = Map[Tile, Double]()

    graph.foreach(tile => {
      gScore = gScore + (tile -> Double.PositiveInfinity)
      fScore = fScore + (tile -> Double.PositiveInfinity)
    })

    gScore = gScore + (source -> 0.0)
    fScore = fScore + (source -> h(source, target))

    while (openSet.nonEmpty) {
      val current = getLowestFScore(openSet, fScore)
      if (current == target) {
        return reconstructPath(cameFrom, current)
      }

      openSet = openSet.diff(List[Tile](current))
      current.getNeighbors.foreach(neighbor => {
        val tentativeGScore = gScore(current) + 1
        if (tentativeGScore < gScore(neighbor.tile)) {
          cameFrom = cameFrom + (neighbor.tile -> current)
          gScore = gScore + (neighbor.tile -> tentativeGScore)
          fScore = fScore + (neighbor.tile -> (gScore(neighbor.tile) + h(neighbor.tile, target)))
          if (!openSet.contains(neighbor.tile)) {
            openSet = openSet ++ List[Tile](neighbor.tile)
          }
        }
      })
    }

    // return failure
    Seq[Tile](source)
  }
}
