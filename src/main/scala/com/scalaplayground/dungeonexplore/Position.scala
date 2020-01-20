package com.scalaplayground.dungeonexplore.Position

class Position(xPos: Int, yPos: Int) {
  var x = xPos;
  var y = yPos;

  def isEqualToPosition(obj: AnyRef): Boolean = {
    if (!obj.isInstanceOf[Position]) {
      return false
    }

    return (obj.asInstanceOf[Position].x == x && obj.asInstanceOf[Position].y == y)
  }

  override def toString: String = {
    s"(${x}, ${y})"
  }
}
