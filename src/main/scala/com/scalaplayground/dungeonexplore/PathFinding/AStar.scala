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
    /*
        total_path := {current}
    while current in cameFrom.Keys:
        total_path.prepend(current)
        current := cameFrom[current]
    return total_path
     */
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

//      openSet.Remove(current)
      openSet = openSet.diff(List[Tile](current))
//      for each neighbor of current
      current.getNeighbors.foreach(neighbor => {
//        // d(current,neighbor) is the weight of the edge from current to neighbor
        //        // tentative_gScore is the distance from start to the neighbor through current
        //        tentative_gScore := gScore[current] + d(current, neighbor)
        val tentativeGScore = gScore(current) + 1
        //      if tentative_gScore < gScore[neighbor]
        if (tentativeGScore < gScore(neighbor.tile)) {
//          / This path to neighbor is better than any previous one. Record it!
//            cameFrom[neighbor] := current
          cameFrom = cameFrom + (neighbor.tile -> current)
//          gScore[neighbor] := tentative_gScore
          gScore = gScore + (neighbor.tile -> tentativeGScore)
//          fScore[neighbor] := gScore[neighbor] + h(neighbor)
          fScore = fScore + (neighbor.tile -> (gScore(neighbor.tile) + h(neighbor.tile, target)))
//          if neighbor not in openSet
          if (!openSet.contains(neighbor.tile)) {
//            openSet.add(neighbor)
            openSet = openSet ++ List[Tile](neighbor.tile)
          }
        }
        //      // This path to neighbor is better than any previous one. Record it!
        //      cameFrom[neighbor] := current
        //      gScore[neighbor] := tentative_gScore
        //      fScore[neighbor] := gScore[neighbor] + h(neighbor)
        //      if neighbor not in openSet
        //        openSet.add(neighbor)
        //
        //      // Open set is empty but goal was never reached
        //      return failure
      })
    }
    /*
    // A* finds a path from start to goal.
// h is the heuristic function. h(n) estimates the cost to reach goal from node n.
function A_Star(start, goal, h)
    // The set of discovered nodes that may need to be (re-)expanded.
    // Initially, only the start node is known.
    openSet := {start}

    // For node n, cameFrom[n] is the node immediately preceding it on the cheapest path from start to n currently known.
    cameFrom := an empty map

    // For node n, gScore[n] is the cost of the cheapest path from start to n currently known.
    gScore := map with default value of Infinity
    gScore[start] := 0

    // For node n, fScore[n] := gScore[n] + h(n).
    fScore := map with default value of Infinity
    fScore[start] := h(start)

    while openSet is not empty
        current := the node in openSet having the lowest fScore[] value
        if current = goal
            return reconstruct_path(cameFrom, current)

        openSet.Remove(current)
        for each neighbor of current
            // d(current,neighbor) is the weight of the edge from current to neighbor
            // tentative_gScore is the distance from start to the neighbor through current
            tentative_gScore := gScore[current] + d(current, neighbor)
            if tentative_gScore < gScore[neighbor]
                // This path to neighbor is better than any previous one. Record it!
                cameFrom[neighbor] := current
                gScore[neighbor] := tentative_gScore
                fScore[neighbor] := gScore[neighbor] + h(neighbor)
                if neighbor not in openSet
                    openSet.add(neighbor)

    // Open set is empty but goal was never reached
    return failure
     */

    // return failure
    Seq[Tile]()
  }
}
