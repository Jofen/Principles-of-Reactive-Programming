package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
    val prevalenceRate = 0.01
    val moveDelay = 5
    val transmissRate = 0.4
    val sickDelay = 6
    val deadDelay = 14
    val dieRate = 0.25
    val immuneDelay = 16
    val healthDelay = 18
  }

  import SimConfig._

  val persons: List[Person] = {
    val initAffected = population * prevalenceRate
    for {
      i <- (0 until population).toList
    } yield {
      val p = new Person (i)
      if (i < initAffected)
        p.setInfected()
      p.setMove()
      p
    }
  }
  
  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    //
    // to complete with simulation logic
    //
    def move() {
      if (dead) return
      val neighbours = List((row, (col - 1 + roomColumns) % roomColumns),
    		  				(row, (col + 1 + roomColumns) % roomColumns),
    		  				((row - 1 + roomRows) % roomRows, col),
    		  				((row + 1 + roomRows) % roomRows, col))
      val healthNeighbours = for {
        neighbour <- neighbours
        if (persons.find(p => p.row == neighbour._1 && p.col == neighbour._2 && (p.sick || p.dead)).isEmpty)
      } yield neighbour
      
      if (!healthNeighbours.isEmpty) {
        val nextRoom = healthNeighbours(randomBelow(healthNeighbours.size))
        row = nextRoom._1
        col = nextRoom._2
      }
      
      if (!infected && !immune) {
        if (!(persons.find(p => p.row == row && p.col == col && p.infected).isEmpty)) {
          if (random < transmissRate) {
            setInfected()
          }
        }
      }
      setMove()
    }
    
    def setMove() {
      val randMove = randomBelow(moveDelay) + 1
      afterDelay(randMove) {move()}
    }
    
    def setInfected() {
      infected = true
      afterDelay(sickDelay) {setSick()}
      afterDelay(deadDelay) {setDead()}
      afterDelay(immuneDelay) {setImmune()}
      afterDelay(healthDelay) {setHealth()}
    }
    
    def setSick() {
      sick = true
    }
    
    def setDead() {
      if (random < dieRate)
        dead = true
    }
    
    def setImmune() {
      if (dead) return
      immune = true
      sick = false
    }
    
    def setHealth() {
      if (dead) return
      // reset all
      infected = false
      sick = false
      immune = false
      dead = false
    }
  }
}
