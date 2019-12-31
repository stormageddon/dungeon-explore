package com.scalaplayground.dungeonexplore.Position

class Position(xPos: Int, yPos: Int) {
  var x = xPos;
  var y = yPos;

  override def toString: String = {
    s"(${x}, ${y})"
  }
}
