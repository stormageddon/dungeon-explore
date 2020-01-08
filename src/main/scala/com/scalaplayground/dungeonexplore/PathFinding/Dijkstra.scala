package com.scalaplayground.dungeonexplore.PathFinding

import com.scalaplayground.dungeonexplore.{Tile, Vertex}

import scala.collection.mutable.Map

class Dijkstra {
  def findShortestPath(graph: Seq[Tile], source: Tile, target: Tile): Seq[Tile] = {
    var dist: Map[Tile, Double] = Map[Tile, Double]()
    var prev: Map[Tile, Option[Tile]] = Map[Tile, Option[Tile]]()
    graph.foreach(node => {
      dist = dist ++ Map[Tile, Double](node -> Double.PositiveInfinity)
      prev = prev ++ Map[Tile, Option[Tile]](node -> None)
    })

    dist(source) = 0.0

    var Q = graph

    while (Q.nonEmpty) {
      // u := node in Q with smallest dist[]
      val u:Tile = getNodeWithSmallestDist(Q, dist)


      // remove u from q
      Q = Q.filter(tile => {
        tile != u
      })

      if (u == target) {
        Q = Seq[Tile]()
      }

      /*
      for each neighbor v of u:	// where v has not yet been removed from Q.
      	alt := dist[u] + dist_between(u, v)
      	if alt < dist[v]	// Relax (u,v)
      	dist[v] := alt
      	previous[v] := u
       */
      u.getNeighbors.foreach(neighbor => {
        if (Q.contains(neighbor.tile)) {
          val alt = dist(u) + getDistanceBetweenNodes(u, neighbor)
          if (alt < dist(neighbor.tile)) {
            dist(neighbor.tile) = alt
            prev(neighbor.tile) = Some(u)
          }
        }
      })

    }

    var shortestPath = Seq[Tile](target)
    prev(target) match {
      case Some(next) => {
        var x = next
        while (x != null) {
          shortestPath = shortestPath :+ x
          x = prev(x).orNull
        }
      }
      case _ => {
        return shortestPath
      }
    }

    return shortestPath.reverse
  }

  def getDistanceBetweenNodes(tile: Tile, vertex: Vertex): Double = {
    vertex.weightedDist
  }

  def getNodeWithSmallestDist(tiles: Seq[Tile], dist: Map[Tile, Double]): Tile = {
    var smallestDistTile = tiles.head

    tiles.map(tile => {
        if (dist(tile) < smallestDistTile.dist) {
          smallestDistTile = tile
        }
    })

    return smallestDistTile
  }

}
