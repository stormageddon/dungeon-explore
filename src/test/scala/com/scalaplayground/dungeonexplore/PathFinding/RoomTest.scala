package com.scalaplayground.dungeonexplore.PathFinding

import com.scalaplayground.dungeonexplore.Position.Position
import com.scalaplayground.dungeonexplore.Room
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class RoomTest extends AnyFlatSpec with Matchers {
 behavior of "Room Test"

  it should "return true if it intersects the provided coordinates" in {
    // arrange
    val room = new Room(new Position(2, 2), 3, 3)
    val otherRoom = new Room(new Position(1, 4), 9, 6)

    // act
    val result = room.intersects(otherRoom)

    // assert
    result mustBe true
  }

  it should "return false if the rooms only overlap on the x axis" in {
    // arrange
    val room = new Room(new Position(2, 2), 3, 3)
    val otherRoom = new Room(new Position(1, 6), 9, 6)

    // act
    val result = room.intersects(otherRoom)

    // assert
    result mustBe false
  }

  it should "return false if the rooms only overlap on the y axis" in {
    // arrange
    val room = new Room(new Position(2, 2), 3, 3)
    val otherRoom = new Room(new Position(1, 6), 9, 6)

    // act
    val result = room.intersects(otherRoom)

    // assert
    result mustBe false
  }

  it should "return false if the rooms do not overlap" in {
    // arrange
    val room = new Room(new Position(17, 17), 2, 2)
    val otherRoom = new Room(new Position(10, 8), 2, 2)

    // act
    val result = room.intersects(otherRoom)

    // assert
    result mustBe false
  }

  it should "return true if the rooms are identical" in {
    // arrange
    val room = new Room(new Position(2, 2), 3, 3)
    val otherRoom = new Room(new Position(2, 2), 3, 3)

    // act
    val result = room.intersects(otherRoom)

    // assert
    result mustBe true
  }

  it should "properly calculate the center of a room" in {
    val room = new Room(new Position(2, 2), 3, 3)
    val expectedCenter = new Position(4, 4)
    val roomCenter = room.getCenter

    roomCenter.isEqualToPosition(expectedCenter) mustBe true
    roomCenter.isEqualToPosition(new Position(3, 2)) mustBe false

    val room2 = new Room(new Position(1, 1), 6, 1)
    val expectedCenter2 = new Position(4, 2)

    room2.getCenter.isEqualToPosition(expectedCenter2) mustBe true
  }

  it should "calculate center of larger 14x20 room" in {
    val room = new Room(new Position(14, 20), 6, 8)
    val expectedCenter = new Position(17, 24)

    room.getCenter.isEqualToPosition(expectedCenter) mustBe true
  }
}
