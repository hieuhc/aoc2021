package archive

import scala.io.Source

object day20 {
  case class TilePos(id: Long, grid: Grid) {
    val up: String    = grid.head
    val left: String  = grid.map(_.head).mkString("")
    val down: String  = grid.last
    val right: String = grid.map(_.last).mkString("")
  }
  case class Tile(positions: Seq[TilePos])
  type GridPos = (Int, Int)
  type Grid    = Seq[String]

  def rotateGrid(grid: Grid): Grid =
    grid.head.indices.map(colIdx => grid.map(_(grid.head.length - 1 - colIdx)).mkString(""))
  def flipGrid(grid: Grid): Grid = grid.map(row => row.reverse)
  def allRotationGrid(grid: Grid): Seq[Grid] =
    Seq(grid, rotateGrid(grid), rotateGrid(rotateGrid(grid)), rotateGrid(rotateGrid(rotateGrid(grid))))
  def rotateTile(tilePos: TilePos): Seq[TilePos] = {
    allRotationGrid(tilePos.grid).map(TilePos(tilePos.id, _))
  }
  def main(args: Array[String]): Unit = {
    val input: Seq[String]     = Source.fromResource("input_day20.txt").getLines().toList
    val emptyLineIdx: Seq[Int] = input.indices.filter(input(_) == "")
    val allTiles: Seq[Tile] = (Seq(-1) ++ emptyLineIdx).map { idx =>
      val tileId: Long = input(idx + 1) match {
        case s"Tile ${id}:" => id.toLong
      }
      val grid: Grid        = input.slice(idx + 2, idx + 12)
      val flippedGrid: Grid = flipGrid(grid)
      Tile(rotateTile(TilePos(tileId, grid)) ++ rotateTile(TilePos(tileId, flippedGrid)))
    }
    val tilePerRow = math.sqrt(allTiles.length).toInt

    def findFit(tile: Tile, crrPos: GridPos, state: Map[GridPos, TilePos]): Seq[TilePos] = {
      val left: Option[String] = state.get((crrPos._1 - 1, crrPos._2)).map(_.right)
      val up: Option[String]   = state.get((crrPos._1, crrPos._2 - 1)).map(_.down)
      tile.positions.filter { tilePos: TilePos =>
        (if (left.isDefined) left.get == tilePos.left else true) && (if (up.isDefined) up.get == tilePos.up else true)
      }
    }
    def movePos(prev: GridPos): GridPos = if (prev._1 == tilePerRow - 1) (0, prev._2 + 1) else (prev._1 + 1, prev._2)
    def spread(
        state: Map[GridPos, TilePos],
        visited: Seq[Long],
        prevPos: GridPos): Option[Map[GridPos, TilePos]] = {
      if (visited.length == allTiles.length) return (Some(state))
      val crrPos: GridPos = movePos(prevPos)
      val candidates: Seq[TilePos] =
        allTiles.filterNot(tile => visited.contains(tile.positions.head.id)).flatMap(tile =>
          findFit(tile, crrPos, state))
      candidates.foldLeft((None: Option[Map[GridPos, TilePos]])) { case (res, tilePos) =>
        if (res.isDefined) res
        else {
          spread(state + (crrPos -> tilePos), visited :+ tilePos.id, crrPos)
        }
      }
    }
    val tilesWithBorder =
      allTiles.flatMap(_.positions).foldLeft((None: Option[Map[GridPos, TilePos]])) {
        case (res, initPos: TilePos) =>
          if (res.isDefined) res else spread(Map((0, 0) -> initPos), Seq(initPos.id), (0, 0))
      }.get
    println(Seq(
      (0, 0),
      (0, tilePerRow - 1),
      (tilePerRow - 1, 0),
      (tilePerRow - 1, tilePerRow - 1)).map(tilesWithBorder(_).id).product)

//    part 2
    val tileRemovedBorder: Map[GridPos, TilePos] = tilesWithBorder.map { case (gridPos, tilePos) =>
      val removedBorderGrid = tilePos.grid.drop(1).dropRight(1).map(_.drop(1).dropRight(1))
      (gridPos, TilePos(tilePos.id, removedBorderGrid))
    }
    val image: Grid = (0 until tilePerRow * 8).map { rowIdx =>
      val tileRowIdx: Int   = rowIdx / 8
      val rowIdxInTile: Int = rowIdx % 8
      (0 until tilePerRow).map(tileRemovedBorder(_, tileRowIdx).grid(rowIdxInTile)).mkString("")
    }
    val monsterPattern = """                  # 
                           |#    ##    ##    ###
                           | #  #  #  #  #  #   """.stripMargin
    val monsterPositions: Seq[(Int, Int)] =
      monsterPattern.split("\n").zipWithIndex.flatMap { case (line: String, y: Int) =>
        line.zipWithIndex.collect { case (ch: Char, x: Int) if (ch == '#') => (x, y) }
      }
    def countSeaMonster(grid: Grid): Int = {
      val gridMap: Map[GridPos, Char] =
        grid.zipWithIndex.flatMap(s => s._1.zipWithIndex.map { case (c, xIdx) => ((xIdx, s._2), c) }).toMap
      (for (x <- 0 until grid.head.length;
        y     <- grid.indices)
        yield {
          monsterPositions.forall { case (baseX, baseY) => gridMap.getOrElse((baseX + x, baseY + y), "") == '#' }
        }).count(_ == true)
    }
    val allImages      = allRotationGrid(image) ++ allRotationGrid(flipGrid(image))
    val numSeaMonsters = allImages.map(countSeaMonster).max
    println(image.map(s => s.count(_ == '#')).sum - numSeaMonsters * monsterPositions.length)
  }
}
