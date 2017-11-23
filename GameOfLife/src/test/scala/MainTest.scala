/*

 Software Craftsmanship Casablanca : https://www.meetup.com/Software-Crafts-wo-manship-Casablanca
 Event: 18/11/2017  Global Day of Coderetreat @Casablanca
  Authors:
          El Arbi Aboussoror
          Abderrahim Ajedig

https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
 */

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
class MainTest extends FunSuite with BeforeAndAfter {

  var cell=new Cell(true)
  var nbcells=new Array[Cell](8)

  before{
        cell.live=true
    for(i<-0 until(nbcells.length))  nbcells(i)=new Cell()
  }


  test("new cell should be dead")
  {
    var cell=new Cell()
    assert(cell.live==false)
  }

  test("Any live cell with fewer than two live neighbours dies"){


    assert(cell.evol(nbcells).live==false)

    for(i<-0 until(nbcells.length)) {
      nbcells(i).live=true
      assert(cell.evol(nbcells).live == false)
      nbcells(i).live=false
    }

  }
test("Any live cell with two or three live neighbours lives on to the next generation"){

  nbcells(0).live=true
  nbcells(5).live=true
  assert(cell.evol(nbcells).live == true)

  nbcells(7).live=true

  assert(cell.evol(nbcells).live == true)
}

  test("Any live cell with more than three live neighbours dies"){

    nbcells(0).live=true
    nbcells(3).live=true
    nbcells(5).live=true
    nbcells(7).live=true

    assert(cell.evol(nbcells).live == false)
  }


  test("Any dead cell with exactly three live neighbours becomes a live cell"){
    cell.live=false
    nbcells(0).live=true
    nbcells(3).live=true
    assert(cell.evol(nbcells).live == false)
    nbcells(5).live=true
    assert(cell.evol(nbcells).live == true)
    nbcells(7).live=true
    assert(cell.evol(nbcells).live == false)

  }


  test("cell at position 0,0 should have 3 neihbours"){
    var grid=new Grid(100,(0,0),(1,1))
    var nbs=grid.neighbours((0,0))

    assert(nbs.length==3)
  }

}
