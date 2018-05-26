package wumpus

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

case object Empty
case object Wumpus
case object Pit
case object WumpusSmell
case object PitDraft
case object Gold
case object StartMessage

object Main extends App {
  val system = ActorSystem("SystemActor")

  val gameActor = system.actorOf(Props[Game], name = "GameActor")
  val gamerActor = system.actorOf(Props(new Gamer(1, gameActor)), name = "GamerActor")

  gameActor ! StartMessage
  gamerActor ! StartMessage
}
class Gamer(intialCell:Int, game: ActorRef) extends Actor{
  var currentCell = intialCell
  def receive ={
    case StartMessage =>
      game ! currentCell
    case Wumpus =>
      println("Game over")
      context.stop(self)
    case Gold =>
      println("Win")
      context.stop(self)
    case _       =>
      currentCell += 1
      sender ! currentCell
  }
}
class Game extends Actor{
  val map :Array[String] = new Array[String](16)
  var currentCell = 1;
  def genMap: Unit ={
    var pitCounter = 4
    var goldCounter = 1
    var wumpusCounter = 1
    val cellNums = scala.util.Random.shuffle(List.range(1, 16))
    for(i <- cellNums ) {
      if(wumpusCounter > 0){
        map(i) = "Wumpus"; wumpusCounter-=1;
      }
      else if(goldCounter > 0){
        map(i) = "Gold"; goldCounter-=1;
      } else if(pitCounter > 0){
        map(i) = "Pit"; pitCounter-=1;
      } else {
        map(i) = "Empty"
      }
      println(i + " " + map(i))
    }
  }
  def checkCell(cellNumber: Int):Unit= {
    if(map(cellNumber) == "Wumpus") {
      sender ! Wumpus
      println("send Wumpus")
      return
    }
    if(map(cellNumber) == "Pit") {
      sender ! Pit
      println("send Pit")
      return
    }
      if(map(cellNumber) == "Gold"){
        sender ! Gold
        println("send Gold")
        return
      }
    if (cellNumber >= 1 && cellNumber <= 16) {
        if (cellNumber - 1 >= 1 && cellNumber - 1 <= 16) {
          map(cellNumber - 1) match {
            case "Wumpus" => sender ! WumpusSmell; println("send WumpusSmell"); return;
            case "Pit" => sender ! PitDraft; println("send PitDraft"); return;
          }
        }
      if (cellNumber + 1 >= 1 && cellNumber + 1 <= 16) {
        map(cellNumber + 1) match {
          case "Wumpus" => sender ! WumpusSmell; println("send WumpusSmell"); return;
          case "Pit" => sender ! PitDraft; println("send PitDraft"); return;
        }
      }
      if (cellNumber + 4 >= 1 && cellNumber + 4 <= 16) {
        map(cellNumber + 4) match {
          case "Wumpus" => sender ! WumpusSmell; println("send WumpusSmell"); return;
          case "Pit" => sender ! PitDraft; println("send PitDraft"); return;
        }
      }
      if (cellNumber - 4 >= 1 && cellNumber - 4 <= 16) {
        map(cellNumber - 4) match {
          case "Wumpus" => sender ! WumpusSmell; println("send WumpusSmell"); return;
          case "Pit" => sender ! PitDraft; println("send PitDraft"); return;
        }
      }
      sender ! Empty
      println("send Empty")
      return
    }
  }
  def receive = {
    case StartMessage => {println("Start");genMap}
    case x:Int => {println("receive " + x);checkCell(x) }
  }
}