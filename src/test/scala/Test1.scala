import org.junit.Test
import org.junit.Assert._
import Shape._
import RoundResult._

class Test1 {

  val player1Name = "p1"
  val player2Name = "p2"

  def playRoundTest(shape1: Shape, shape2: Shape, expectedResult: RoundResult) =
    val player1 = Player(player1Name, () => shape1)
    val player2 = Player(player2Name, () => shape2)
    assertEquals(expectedResult, evaluateRound(player1, player2))

  @Test def rockTiesWithRock        = playRoundTest(Rock,     Rock,     Tie)
  @Test def rockLoosesToPaper       = playRoundTest(Rock,     Paper,    Win("p2", Paper,    Rock))
  @Test def rockBeatsScissors       = playRoundTest(Rock,     Scissors, Win("p1", Rock,     Scissors))
  @Test def paperBeatsRock          = playRoundTest(Paper,    Rock,     Win("p1", Paper,    Rock))
  @Test def paperTiesWithPaper      = playRoundTest(Paper,    Paper,    Tie)
  @Test def paperLoosesToScissors   = playRoundTest(Paper,    Scissors, Win("p2", Scissors, Paper))
  @Test def scissorsLooseToRock     = playRoundTest(Scissors, Rock,     Win("p2", Rock,     Scissors))
  @Test def scissorsWinToPaper      = playRoundTest(Scissors, Paper,    Win("p1", Scissors, Paper))
  @Test def scissorsTieWithScissors = playRoundTest(Scissors, Scissors, Tie)





}