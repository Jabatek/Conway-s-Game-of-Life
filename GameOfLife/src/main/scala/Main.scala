/*

 Software Craftsmanship Casablanca : https://www.meetup.com/Software-Crafts-wo-manship-Casablanca
 Event: 18/11/2017  Global Day of Coderetreat @Casablanca
  Authors:
          El Arbi Aboussoror
          Abderrahim Ajedig
Algorithm
https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
 */


import scala.collection.mutable.{ArrayBuffer}
import scala.util.Random

class Cell(var live:Boolean=false){

  def evol(nbs: Array[Cell]): Cell ={
    var nbLiveCells=0

    for(nb<-nbs){
      if(nb.live)
        nbLiveCells+=1
    }
    var nextState=false
//    if( (nbLiveCells==2 || nbLiveCells==3) )
    if(live && (nbLiveCells==2 || nbLiveCells==3) || (!live && nbLiveCells==3))
      nextState=true
    new Cell(nextState)
  }

  override def toString: String = {
    live +" "
  }
}



class Grid(dim:Int,lives:Tuple2[Int,Int]*){
  var cells= Array.ofDim[Cell](dim,dim)
  for(i<-0 until(dim))
    for(j<-0 until(dim))
      cells(i)(j)=new Cell()
  for(t<-lives){
    cells(t._1)(t._2).live=true
  }

  def neighbours(cell: Tuple2[Int,Int]): ArrayBuffer[Cell] =
  {
    var nbs=new ArrayBuffer[Cell]()
    for(i<-cell._1-1 to cell._1+1 )
      for(j<-cell._2-1 to cell._2+1 )
        if(!(i==cell._1 && j==cell._2) && i>=0 && j>=0 && i<dim && j<dim)
        nbs+=(cells(i)(j))

    nbs
  }

  def nextGen(): Grid ={

    var nextgrid= new Grid(dim)
    for(i<-0 until(dim))
      for(j<-0 until(dim))
        nextgrid.cells(i)(j)=cells(i)(j).evol(neighbours((i,j)).toArray[Cell])


    nextgrid
  }



}


object Main {
  def main(args: Array[String]) = {

    var grid = new Grid(10, (3, 3), (3, 5),(2, 4), (4, 4)) // Tub "Still lifes"
//    var grid = new Grid(10, (1, 1), (1, 2),(1, 3)) // Blinker (period 2)  "Oscillators"
//    var grid = new Grid(10, (1, 1), (1, 2),(1, 3),(2, 2), (2, 3),(2, 4)) // Toad (period 2) "Oscillators"
//    var grid = new Grid(20, (3, 1), (3, 2),(3, 3),(1, 2), (2, 3)) // Glider  "Spaceships"

// Random
//     var   liveCells=new ArrayBuffer[(Int, Int)]
//      Random.setSeed(0)
//     for(i<- 5 until 15 ; j<-5 until 15)
//      if(Random.nextDouble()>.8)
//        liveCells += ((i,j))
//     var grid = new Grid(20,liveCells:_*)


    for(it<-0 to 40) {
      println("")
      println("Gen: "+it)
      for (cc <- grid.cells) {
        println()
        for (c <- cc) {
          print(if (c.live) "[]" else "--")
        }
      }
      println("")
      grid = grid.nextGen()
    }

  }
}
