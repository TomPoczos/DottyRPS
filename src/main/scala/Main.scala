import scala.io.StdIn.readLine
import scala.annotation.tailrec

enum Shape {
  case Rock, Paper, Scissors
}

import Shape._

case class Player(name: String, playMove: () => Shape)

// loosing shape is not needed right now since it is deductible from the winning shape,
// however, this would not be the case if the new shapes would be introduced, so
// it is included for extensibility

case class RoundResult(winnerName: String, winningShape: Shape, loosingShape: Shape)

def evaluateRound(player1: Player, player2: Player): Option[RoundResult] =
  (player1.playMove(), player2.playMove()) match
    case (Rock,     Rock)     => None
    case (Rock,     Paper)    => Some(RoundResult(player2.name, Paper,    Rock))
    case (Rock,     Scissors) => Some(RoundResult(player1.name, Rock,     Scissors))
    case (Paper,    Rock)     => Some(RoundResult(player1.name, Paper,    Rock))
    case (Paper,    Paper)    => None
    case (Paper,    Scissors) => Some(RoundResult(player2.name, Scissors, Paper))
    case (Scissors, Rock)     => Some(RoundResult(player2.name, Rock,     Scissors))
    case (Scissors, Paper)    => Some(RoundResult(player1.name, Scissors, Paper))
    case (Scissors, Scissors) => None

def toOutputString(result: Option[RoundResult]): String =
  result match
    case None => "It's a tie"
    case Some(res) => f"${res.winningShape} beats ${res.loosingShape}, ${res.winnerName} wins!"

def playRound(player1: Player, player2: Player): String =
  toOutputString(evaluateRound(player1, player2)) 

def playComputerMove(): Shape =
  new scala.util.Random().nextInt(3) match 
    case 0 => Rock
    case 1 => Paper
    case 2 => Scissors

@tailrec
def playHumanMove(): Shape = 
  interact("press (1) for rock, (2) for paper or (3) for scissors") match 
    case "1" => Rock
    case "2" => Paper
    case "3" => Scissors
    case _   => playHumanMove()

def interact(question: String):String =
  println(question)
  readLine()

@main
@tailrec
def m(): Unit = 
  val player1 = interact("press (1) for computer vs computer or (2) for player vs computer") match
    case "1" => Some(Player("Computer 2", playComputerMove))
    case "2" => Some(Player(interact("type in your name"), playHumanMove))
    case _   => None
  val player2 = Player("Computer 1", playComputerMove)
  
  if player1.isDefined
    println(playRound(player1.get, player2))
  
  // calling m() is my interpretation of the "Can I play a different game each time? 
  // requirement. In real life I'd ask for clarification"
  m() 
  

